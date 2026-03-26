---
name: kdl-config
description: Author, review, and explain KDL configuration and Rust derive mappings for the kdl-config crate. Use when translating Rust structs and enums into KDL, generating example KDL from types, choosing field placement, explaining derive attributes, designing canonical KDL surfaces, validating bool/conflict/render behavior, using registries or children_map, working with selectors, fragments, merge or partial overlays, schema generation, or applying from/try_from conversions for domain newtypes and validated wrappers.
---

# KDL Config Design

## Overview

Design KDL as a language, not a parser artifact. Produce shapes that are mechanically correct, easy to scan, stable to render, and hard to misuse.

Use this skill to work with the `kdl-config` crate as a **typed KDL ↔ Rust mapping system**, not just a list of derive attributes.

The goal is to help an agent:

- understand what KDL shape a Rust type expects,
- produce correct and idiomatic KDL from Rust derives,
- explain why a field belongs in attributes, positional args, value children, or child nodes,
- reason about booleans, conflicts, defaults, registries, selectors, fragments, rendering, and validation,
- use `from` / `try_from` correctly for domain types,
- and keep authored KDL canonical, stable, and human-readable.

Use these source documents together:

- Read [SPEC.md](references/SPEC.md) for the behavioral contract: derives, attributes, placement, selectors, conflicts, booleans, merge, partials, fragments, schema, and validation.
- Read [GUIDE.md](references/GUIDE.md) for authoritative crate-specific guidance, examples, caveats, and practical authoring patterns.
- Read [STYLE.md](references/STYLE.md) when designing or reviewing the *language shape* of a KDL surface, not just its mechanical correctness.
- Read [VALIDATION.md](references/VALIDATION.md) for validation rules and behavior.

Keep this skill aligned with those files when the crate evolves.

## Load Only What You Need

- Read **SPEC.md** when the task is about exact attribute behavior, legal combinations, defaults, precedence, rendering, validation, fragments, merge, or schema behavior.
- Read **GUIDE.md** when the task is about how to author KDL from Rust types, how fields map to KDL, or how to explain crate behavior to a user or agent.
- Read **STYLE.md** when the task is about designing a public KDL surface that should feel deliberate and human-authored.

## Mental Model

Think in two layers:

1. **Mechanical mapping**
   - A Rust type defines what KDL is accepted and how it renders.
   - Each field maps to a placement: positional arg, keyed attribute, value child node, child node, repeated child nodes, registry, or children map.
   - Conflicts, defaults, bool mode, render placement, and validation refine that mapping.

2. **Language design**
   - A public KDL surface should still feel like a coherent language.
   - The fact that the crate *can* parse several shapes does **not** mean all shapes should be presented as equal-status authoring forms.
   - Prefer one canonical rendered form.

## Workflow

1. Identify the Rust container kind.
   - `KdlNode` / node-mode `Kdl`
   - `KdlValue` / value-mode `Kdl`
   - `KdlChoice` / choice-mode `Kdl`
   - `KdlMerge`
   - `KdlPartial`
   - `KdlSchema`

2. Identify the root KDL form.
   - What node name is expected?
   - Is this a node, scalar value, tagged enum, or choice enum?

3. Inspect container-level defaults.
   - `rename_all`
   - `default_placement`
   - `default_bool`
   - `default_flag_style`
   - `default_conflict`
   - `skip_serialize_none`
   - `skip_serialize_empty_collections`
   - `deny_unknown`
   - `validate`
   - `post_decode`

4. Inspect each field.
   - Placement attributes
   - Name overrides
   - defaults / requiredness
   - bool / flag style / conflict overrides
   - render override
   - conversion (`from`, `try_from`, `scalar`)
   - validation
   - schema overrides

5. Determine the canonical KDL shape.
   - Prefer the explicit placement annotations.
   - If placement is exhaustive, determine what the crate accepts **and** choose the single best authored form.

6. Render semantically.
   - Identity first
   - inline operational settings next
   - flags where natural
   - grouped child/value nodes after
   - repeated structured entries in a stable order

7. Cross-check correctness.
   - required fields present
   - selector and key extraction make sense
   - no illegal attribute combinations
   - no duplicate/conflicting candidates unless conflict policy allows it
   - render shape is canonical and readable

## Core Mapping Rules

### 1. Node-mode structs map to KDL nodes

A node-mode struct typically looks like:

```rust
#[derive(Kdl)]
#[kdl(node = "server", rename_all = "kebab-case")]
struct ServerConfig {
    #[kdl(positional = 0)]
    name: String,

    #[kdl(attr)]
    host: String,

    #[kdl(attr, try_from = "i64")]
    port: Port,

    #[kdl(child)]
    tls: TlsConfig,
}
```

Canonical KDL:

```kdl
server "api" host="localhost" port=8080 {
    tls cert-path="/etc/cert.pem" key-path="/etc/key.pem"
}
```

Use `node = "..."` explicitly for user-facing config surfaces.

### 2. Placement is the first thing to resolve

Field placement determines where a value comes from and where it should usually be shown:

- `attr` / `keyed` → `key=value`
- `positional = N` → nth positional arg
- `positional = "rest"` → collect remaining args into `Vec`
- `value` → child value node
- `child` → nested child node
- `children` → repeated nested child nodes
- `children_any` / `choice` → repeated choice children
- `registry` → repeated keyed container nodes
- `children_map` → map collected from child nodes
- `flatten` → nested fields appear on parent
- `modifier` → capture `+`, `-`, `!`, `~`

When no explicit placement is given, use the container default placement. If that default is `exhaustive`, the crate may accept multiple shapes; still present one canonical authored shape.

### 3. Use positional args for identity

When a field is naturally the subject or identity of a node, positional placement is usually the best fit.

```rust
#[derive(Kdl)]
#[kdl(node = "provider")]
struct ProviderConfig {
    #[kdl(positional = 0)]
    name: String,

    #[kdl(attr)]
    model: String,
}
```

```kdl
provider "openai" model="gpt-4.1"
```

Do **not** use positional args just because they are shorter. Keep operational meaning keyed.

### 4. Use keyed attributes for operational semantics

Prefer attributes for:
- paths
- URLs
- models
- limits
- thresholds
- modes
- backends
- providers
- booleans that are better explicit than structural

```rust
#[kdl(attr)] endpoint: String,
#[kdl(attr)] backend: String,
#[kdl(attr)] timeout_ms: u64,
```

```kdl
embedding default backend="ollama" endpoint="http://localhost:11434" timeout-ms=8000
```

### 5. Use value nodes for scalar child groups or lists

Use `value` when a field reads more cleanly as a child node, especially for lists or values you want visually separated from inline attrs.

```rust
#[kdl(value)]
tags: Vec<String>,
```

```kdl
config {
    tags "alpha" "beta" "gamma"
}
```

### 6. Use child nodes for nested structure

Use `child` or `children` when the field has its own internal structure.

```rust
#[kdl(child)]
database: DatabaseConfig,
```

```kdl
config {
    database host="db.local" port=5432
}
```

```rust
#[kdl(children, name = "plugin")]
plugins: Vec<PluginConfig>,
```

```kdl
config {
    plugin name="cache" priority=2
    plugin name="audit" priority=3
}
```

### 7. Use registries and children maps for keyed repeated structures

Use `registry` when all entries share a container node name and the key comes from a stable place like the first arg.

```rust
#[kdl(registry, container = "rule")]
rules: HashMap<String, RuleConfig>,
```

```kdl
config {
    rule "alpha" kind="allow"
    rule "beta" kind="deny"
}
```

Use `children_map` when keys should come from the child name, an attribute, a selector, or some more flexible extraction strategy.

```rust
#[kdl(children_map)]
categories: HashMap<String, CategoryConfig>,
```

```kdl
config {
    docs chunk-size=3000
    code chunk-size=2000
}
```

or:

```rust
#[kdl(children_map, map_node = "category", select(attr("name"), consume))]
categories: HashMap<String, CategoryConfig>,
```

```kdl
config {
    category name="docs" chunk-size=3000
    category name="code" chunk-size=2000
}
```

## Derive Guidance

### `Kdl`

Use the unified derive unless explicitness matters. It auto-selects behavior based on the type shape and attributes. This is usually the best user-facing derive because it keeps examples simpler. fileciteturn0file0L7-L15

### `KdlNode`

Use for structs and tagged enums that map to KDL nodes. It generates decode, render, update, and schema behavior. Use it when you want the type to be a node, full stop. fileciteturn0file0L7-L15

### `KdlValue`

Use for value-mode types:
- unit enums that map to scalar values,
- newtype structs that should behave like scalar values.

This is ideal when the type is semantically richer than a plain scalar but should still behave like one in KDL.

```rust
#[derive(KdlValue)]
#[kdl(rename_all = "kebab-case")]
enum LogLevel {
    Debug,
    Info,
    Warning,
    Error,
}
```

```kdl
config log-level="warning"
```

### `KdlChoice`

Use when the **node name** selects the variant. This is ideal for heterogeneous child lists.

```rust
#[derive(KdlChoice)]
enum Step {
    #[kdl(name = "fetch")]
    Fetch(FetchStep),

    #[kdl(name = "transform")]
    Transform(TransformStep),

    Skip,
}
```

```kdl
pipeline {
    fetch url="https://example.com"
    transform format="json"
    skip
}
```

### `KdlMerge`

Use when the type participates in layered config. This generates `DeepMerge` and lets fields control merge policy.

### `KdlPartial`

Use when you want a sparse overlay type whose fields are optional and can be applied to a concrete base config. This is a typed “patch” shape for user-supplied overrides.

### `KdlSchema`

Use when you only need schema metadata, not parsing/rendering.

## Container-Level Attributes

### `node = "name"`

Sets the node name expected for parsing and used for rendering.

Use this on all public config structs unless you explicitly want “accept any node name” behavior.

### `rename_all = "..."`

Use `kebab-case` by default for KDL-facing config. This keeps KDL idiomatic and readable.

### `default_placement = "..."`

This is the baseline placement strategy for fields without explicit placement.

- `exhaustive` is flexible but may permit multiple accepted shapes.
- `attr` is good for dense, flat settings nodes.
- `value` is good when fields are intended to read like grouped child values.
- `child` is only appropriate when unannotated fields are conceptually nested child nodes.

For public, stable config surfaces, explicit field placement is usually better than relying heavily on exhaustive defaults.

### `default_bool = "..."`

Controls how bools parse by default:

- `presence+value`: accepts flags and explicit values
- `value-only`: only explicit `#true` / `#false`
- `presence-only`: only positive presence

Use:
- `presence+value` for ergonomic authoring
- `value-only` when explicitness matters
- `presence-only` only when the field is truly a pure switch

### `default_flag_style = "..."`

Controls how bools render by default.

Use field-level overrides when a specific bool should read as `with-x` / `without-x` or remain fully explicit.

### `default_conflict = "..."`

Controls how multiple candidates resolve.

Use:
- `error` as the safe default
- `last` when later definitions should override earlier ones
- `append` only for `Vec<T>`
- `first` rarely

### `skip_serialize_none`

Use to keep rendered KDL clean by omitting `None` values.

### `skip_serialize_empty_collections`

Use to keep rendered KDL clean by omitting empty collections.

### `deny_unknown`

Use on strict, user-authored config surfaces where typos should be caught early.

Be careful with selectors and keyed map extraction: if key material remains on the child node, `deny_unknown` can reject it unless you `consume` it.

### `post_decode`

Use for normalization after parsing and before final validation. Good examples:
- lowercasing hostnames,
- computing derived defaults,
- canonicalizing order or representation.

### `validate(func = "...")`

Use for invariants that depend on the fully decoded struct.

## Field-Level Attributes

### Placement

#### `attr` / `keyed`

Use for inline settings:

```rust
#[kdl(attr)]
host: String,
```

```kdl
server host="localhost"
```

#### `positional = N`

Use for subject-like identity:

```rust
#[kdl(positional = 0)]
name: String,
```

```kdl
provider "openai"
```

#### `positional = "rest"`

Use for simple lists that are naturally positional.

#### `value`

Use for child value nodes, especially lists or visually separated scalar settings.

```rust
#[kdl(value)]
include: Vec<String>,
```

```kdl
filter {
    include "src/**" "tests/**"
}
```

#### `child`

Use for one nested struct-like concept.

#### `children`

Use for repeated structured entries with the same node name.

#### `children_any` / `choice`

Use for repeated heterogeneous entries selected by node name.

#### `registry`

Use for repeated structured entries keyed by a stable extracted key and sharing a container node name.

#### `children_map`

Use when the map key comes from the node name, an attr, a function, or some more flexible key extraction path.

#### `flatten`

Use sparingly. Flatten is best when the nested type is purely structural reuse and the user should experience the fields as belonging to the parent.

Do **not** flatten if the nested concept deserves its own visible namespace in the config.

#### `modifier`

Use only on types that participate in merge-aware layered KDL. This captures `+`, `-`, `!`, or `~` so downstream logic can honor it.

### Naming

#### `name = "..."` / `rename = "..."`

Use when the KDL name must differ from the renamed Rust field or variant name.

#### `alias = "..."`

Use for compatibility or migration, not as a first-class stylistic choice. Keep one canonical rendered name.

### Requiredness and Defaults

#### `required`

Use only when a field has no sensible default and omission should be a hard error.

#### `optional`

Use when you want to make optionality explicit in the code or schema.

#### `default`

Use `Default::default()`.

#### `default = ...`

Use literal defaults for simple, stable defaults.

#### `default_fn = "..."`

Use when the default is computed or shared.

### Bool Behavior

#### `bool = "presence+value"`

Best general-purpose option.

#### `bool = "value-only"`

Use when explicit configuration is important and flags would be too implicit.

#### `bool = "presence-only"`

Use only for true switch-like presence semantics.

### Flag Style

#### `flag_style = "both"`

Good general-purpose acceptance mode.

#### `flag_style = "value|no"`

Use when you want explicit negative spelling while staying fairly compact.

#### `flag_style = "with|without"`

Use when the boolean reads more naturally as an authorial mode or capability toggle.

### Conflict

#### `conflict = "error"`

Safest default.

#### `conflict = "last"`

Good for override-driven config.

#### `conflict = "first"`

Use only when “first declaration wins” is deliberate and documented.

#### `conflict = "append"`

Use only on `Vec<T>`, especially when accepting values from multiple placements.

### Render

Use `render = "..."` when parsing may accept several shapes but rendering must normalize to one canonical form.

This is especially useful for compatibility: accept multiple shapes, emit one shape.

### `path = "..."`

Use to re-root parsing into a nested KDL path without changing the rendered form. This is decode-oriented indirection. Use sparingly; explicit child structure is often clearer for public config.

### `scalar` / `value_type` / `value_like` / `kdl_value`

Use when the field type is a scalar-like custom type and the derive needs help treating it as a value type instead of a node type.

## `from` and `try_from`

This is one of the most important agent-facing parts of the crate.

### What problem they solve

KDL only gives you scalar wire values: strings, integers, floats, bools, null. Your domain model often wants richer types:

- `ProviderId(Arc<str>)`
- `Port(u16)`
- `Percent(f64)` with bounds
- `UserId(String)`
- `Slug(String)` with validation
- `Url` or `PathLike` wrappers
- semantic newtypes that should not be plain `String` in your domain

`from` and `try_from` let a field parse as an intermediate KDL-native type and then convert into the actual domain type.

### `from = "Type"`

Use this when conversion is infallible.

Typical use cases:
- newtypes around `String`
- `Arc<str>` wrappers
- semantic IDs
- cheap representation changes
- “this is just a domain label, not a validation boundary”

Example:

```rust
struct ProviderId(std::sync::Arc<str>);

impl From<String> for ProviderId {
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

impl From<ProviderId> for String {
    fn from(value: ProviderId) -> Self {
        value.0.to_string()
    }
}

#[derive(Kdl)]
#[kdl(node = "config")]
struct Config {
    #[kdl(attr, from = "String")]
    provider: ProviderId,
}
```

```kdl
config provider="openai"
```

Why use `from` here:
- KDL should stay simple and human-authored
- the domain should still use `ProviderId`, not raw `String`
- there is no failure condition, so `TryFrom` would add needless ceremony

### `try_from = "Type"`

Use this when conversion can fail.

Typical use cases:
- validated numeric ranges
- bounded IDs
- parsed strings
- URLs, ports, durations, slugs
- domain wrappers that enforce invariants

Example:

```rust
struct Port(u16);

impl TryFrom<i64> for Port {
    type Error = String;

    fn try_from(value: i64) -> Result<Self, String> {
        if (1..=65535).contains(&value) {
            Ok(Self(value as u16))
        } else {
            Err(format!("port out of range: {value}"))
        }
    }
}

impl From<Port> for i64 {
    fn from(value: Port) -> Self {
        value.0 as i64
    }
}

#[derive(Kdl)]
#[kdl(node = "server")]
struct ServerConfig {
    #[kdl(attr, try_from = "i64")]
    port: Port,
}
```

```kdl
server port=8080
```

Why use `try_from` here:
- KDL should still accept a natural scalar
- the domain type should protect the invariant
- validation belongs at the conversion boundary

### Choosing between them

Use `from` when:
- conversion always succeeds,
- the type is mostly a semantic wrapper,
- you are changing representation, not validating.

Use `try_from` when:
- the domain type has invariants,
- bad KDL should produce a clear parse error,
- the field value needs validation during construction.

### Newtypes and wrappers

These attributes are especially good for newtypes.

Good candidates:
- `UserId(String)`
- `ProviderName(String)`
- `Tag(Arc<str>)`
- `Port(u16)`
- `PositiveInt(i64)`
- `Weight(f64)`

If the type should behave like a scalar *everywhere*, consider implementing `FromKdlValue` or using `KdlValue` directly instead of repeating `from` / `try_from` on many fields.

### `Option<T>` and `Vec<T>`

`from` and `try_from` apply element-wise to `Option<T>` and `Vec<T>` field shapes, which makes them a good fit for optional domain wrappers or repeated identifiers.

```rust
#[kdl(attr, from = "String")]
maybe_provider: Option<ProviderId>,

#[kdl(value, from = "String")]
providers: Vec<ProviderId>,
```

### Why not always implement `FromKdlValue`?

Use `from` / `try_from` when:
- the scalar behavior is field-local,
- you want the conversion boundary to be explicit at the field,
- the type is domain-specific and not a general-purpose scalar across the whole crate.

Implement `FromKdlValue` or derive/use `KdlValue` when:
- the type is *inherently* a scalar KDL type everywhere,
- you want broad reuse without repeated annotations.

## Validation

Use validation to keep config errors close to the field that owns the rule.
See [VALIDATION.md](references/VALIDATION.md) for more details.

## Enums

### Tagged enums

Use when the discriminator comes from an arg, attr, or selector.

### Choice enums

Use when the node name itself chooses the variant.

### Value enums

Use when the enum should be a scalar value.

For all enum modes, keep one canonical rendered form and avoid teaching multiple equal-status styles.

## Fragments

Fragments are expanded before typed parsing. The typed layer does not “see” `fragment`, `with`, or `from`; it sees the expanded result.

Use fragments for reusable typed templates, not as a replacement for clear type design.

Use:
- `fragment` for reusable typed config snippets
- `with` to apply a fragment inside an existing parent node
- `from` to materialize a new node from a fragment

When explaining a typed config, distinguish:
- what the user authors,
- what fragment expansion produces,
- what the typed parser ultimately consumes.

## Merge and Partial Overlays

### `KdlMerge`

Use to define how layered configs combine.

Common policies:
- `deep` for nested structured config
- `keep` when base must win
- `replace` when overlay must win
- `append` for additive collections
- `merge(func = "...")` for domain-specific policies

### `KdlPartial`

Use when you want a sparse overlay type, usually with `Option<T>` fields, that applies onto a concrete base config.

This is especially useful for:
- environment or profile overrides,
- local developer overrides,
- partial config editors,
- patch-like typed updates.

### Merge-aware modifiers

When layered loading is in play, node prefixes matter:

- `+` append
- `-` remove
- `!` replace
- `~` flatten / merge into target

Only discuss or emit these when the config system is actually using merge-aware layering.

## Schema Guidance

Use schema generation when:
- editor tooling needs machine-readable structure,
- config UIs need descriptions or types,
- you want validation/documentation support derived from the same Rust types.

Schema overrides should refine clarity, not contradict actual parse behavior.

## Canonicalization Rules

The following rules define the expected canonical authoring and explanation style for `kdl-config` surfaces.

1. A skill user or agent **MUST** identify the canonical rendered form of a type before presenting examples, recommendations, or generated KDL.
2. Public, user-authored KDL surfaces **SHOULD** use `rename_all = "kebab-case"` unless another naming convention is an intentional part of the language design.
3. Positional arguments **MUST** be used for identity, subject, or other naturally positional concepts, and **MUST NOT** be used merely because they are shorter than keyed attributes.
4. Keyed attributes **SHOULD** be used for operational semantics such as paths, URLs, models, backends, modes, thresholds, limits, and similar configuration values.
5. Child groups **SHOULD** be used when settings are conceptually related, configured together, share a namespace or prefix, or are likely to grow.
6. Repeated positional values or repeated value-node args **SHOULD** be used for simple scalar lists.
7. Repeated child nodes **MUST** be used for repeated structured entries whose elements carry their own fields or internal structure.
8. When parsing accepts multiple compatible source shapes, rendering **MUST** normalize to a single canonical form. `render = ...` **SHOULD** be used whenever necessary to enforce that normalization.
9. Aliases **MAY** be accepted for compatibility or migration, but aliases **MUST NOT** be presented as equal-status authoring forms. Exactly one rendered name **MUST** be treated as canonical.
10. `deny_unknown` **SHOULD** be enabled on strict user-authored config surfaces where typo detection is part of the intended UX.
11. When selector-based extraction leaves otherwise unknown key material on a node, the selector **SHOULD** use `consume` unless preserving that material is explicitly required by the inner type.
12. Public config types **SHOULD** prefer explicit field placement annotations over reliance on exhaustive placement.
13. Exhaustive placement **MAY** be used as an acceptance mechanism, but it **MUST NOT** be presented as justification for teaching multiple equal-status authored forms.
14. Explanations and generated examples **MUST** present the canonical authored form first. Alternate accepted forms **MAY** be mentioned only for compatibility, migration, or debugging purposes.

## Non-Conforming Patterns

The following patterns are non-conforming for canonical documentation, generated examples, or agent guidance.

1. An agent **MUST NOT** present every parse-accepted shape as though it were an equal-status authored form.
2. An agent **MUST NOT** recommend positional arguments for operational meaning solely because positional syntax is shorter.
3. A design **MUST NOT** flatten concepts that materially deserve their own visible child node or namespace.
4. A boolean field **MUST NOT** use `presence-only` semantics when the language needs an explicit and teachable false state.
5. A field **MUST NOT** use `append` conflict behavior unless the field type is a `Vec<T>` or another append-compatible collection explicitly supported by the crate.
6. A design **SHOULD NOT** use `registry` when `children_map` more accurately models the intended key source or language shape.
7. Aliases **MUST NOT** be documented or emitted as stylistic alternatives to the canonical name. They are compatibility mechanisms only.
8. A field using `from` or `try_from` **MUST** satisfy both decode-side and render-side conversion requirements. An agent **MUST NOT** recommend these attributes without accounting for round-trip behavior.
9. Generated or recommended KDL **MUST NOT** look like a serializer dump when a clearer human-authored shape is available.
10. A public config language **SHOULD NOT** rely on exhaustive placement without also defining and documenting a single canonical rendered form.

## Response Style

When using this skill:

- Explain the type shape before presenting final KDL.
- Show the canonical authored form first.
- Mention alternative accepted forms only when compatibility or debugging requires it.
- For `from` / `try_from`, explain the domain boundary and why the conversion exists.
- When reviewing, separate:
  1. mechanical correctness,
  2. canonical shape choice,
  3. readability and language design.
