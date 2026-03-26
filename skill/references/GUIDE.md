# kdl-config — Authoritative Agent Skill Reference

## What This Crate Does

`kdl-config` maps Rust structs and enums to [KDL](https://kdl.dev) configuration documents via derive macros. It provides:

- **Decoding** — parse KDL text into typed Rust values (`KdlDecode`)
- **Rendering** — produce canonical KDL from Rust values (`KdlRender`)
- **Round-trip updates** — modify existing KDL ASTs in-place, preserving formatting and comments (`KdlUpdate`)
- **Schema generation** — derive structural schemas for tooling (`KdlSchema`)
- **Deep merging** — layer configs with per-field merge policies (`DeepMerge`)
- **Partial overlays** — apply sparse option-wrapped configs onto base configs (`PartialConfig<T>`)
- **Validation** — declarative field, collection, cross-field, and struct-level constraints
- **Fragments** — reusable KDL template blocks expanded before typed parsing
- **Layered loading** — stack multiple config files with merge modifiers (`+`, `-`, `!`, `~`)

The crate solves the problem of turning human-authored KDL into strongly typed Rust configuration, and back, without losing author intent.

Consumers normally depend only on `kdl-config` itself. The derive macros generate code against `::kdl_config::*`, so downstream crates typically do not need to depend on `kdl` directly.

---

## How To Use This Skill

Use this guide when you need to:

- design a Rust type that maps cleanly to KDL,
- infer what KDL shape a type expects,
- explain what a derive attribute does,
- decide whether something belongs in args, attrs, value nodes, or child nodes,
- understand `from` / `try_from`,
- author layered config with merge modifiers,
- or review a KDL surface for correctness and readability.

Treat this as a **field manual for agents**. The goal is not just to list attributes, but to explain:
- what each attribute does,
- what KDL it produces,
- when to use it,
- and what tradeoffs it implies.

---

## Mental Model

### The Core Mapping

Every Rust struct in node mode maps to a KDL **node**. A KDL node has three places to put data:

```kdl
node-name "positional-arg" key="named-attr" {
    child-node "value"
}
```

That yields the core mapping model:

| KDL position | Field placement | Rust example | KDL example |
|---|---|---|---|
| **Positional argument** | `positional = N` | `id: String` | `server "alpha"` |
| **Keyed attribute** | `attr` / `keyed` | `port: u16` | `server port=8080` |
| **Value child node** | `value` | `tags: Vec<String>` | `server { tags "web" "api" }` |
| **Child node** | `child` / `children` | `database: DbConfig` | `server { database host="..." }` |

A good shorthand:

- **identity** belongs in positional args,
- **operational settings** belong in keyed attrs,
- **scalar child groups or lists** belong in value nodes,
- **nested structure** belongs in child nodes.

### Default Type-Driven Expectations

When no explicit placement is given, the crate uses defaults based on type and container policy.

As a practical authoring rule:

- **Scalar types** (`String`, numbers, `bool`, `PathBuf`, etc.) are usually attr/positional/value candidates.
- **Nested structs** are usually child nodes.
- **`Vec<T>` of scalars** usually want value placement.
- **`Vec<T>` of structs** usually want children placement.
- **`HashMap<_, _>`** generally needs `registry` or `children_map`.

For public config surfaces, do not rely too heavily on implicit behavior. Prefer explicit placement.

### Exhaustive Default Placement

If a field has no explicit placement and the container uses the default exhaustive mode, `kdl-config` may try several locations and accept whichever one matches:

1. keyed attr
2. positional arg
3. value child
4. child node

That is useful for compatibility and convenience, but it can also create ambiguity. For stable, documented config surfaces, choose one canonical placement and annotate it.

### Parse → Convert → Validate → Construct

The decode pipeline is best understood as:

1. **Parse** raw values from KDL by placement
2. **Convert** using `From`, `TryFrom`, or scalar conversion helpers
3. **Validate** field-level rules
4. **Validate** cross-field and struct-level rules
5. **Post-process** with `post_decode` if present
6. **Construct / finalize** the Rust value

That separation matters. A field may successfully parse from KDL but still fail conversion or validation.

---

## Derive Macros

### `#[derive(Kdl)]` — Unified Derive

This is usually the best default. It inspects the container and selected attributes and generates the appropriate behavior.

High-level modes:

| Annotation / shape | Mode | Purpose |
|---|---|---|
| plain struct or `#[kdl(node = "...")]` | **Node** | KDL node mapping |
| `#[kdl(choice)]` on enum | **Choice** | dispatch by child node name |
| `#[kdl(value)]` on enum/newtype | **Value** | scalar KDL value |
| `#[kdl(schema)]` | **Schema** | schema-only metadata |

Use `Kdl` when you want one derive that follows the type shape naturally.

### `#[derive(KdlNode)]`

Generates node-mode traits only:

- `KdlDecode`
- `KdlRender`
- `KdlUpdate`
- `KdlSchema`

Use this when you want to be explicit that the type is a KDL node.

### `#[derive(KdlValue)]`

Use for scalar-like types:

- **unit enums** that map to string values,
- **newtype structs** that map to a single scalar value.

This is ideal when the type is semantically richer than a primitive but should still behave like a scalar in KDL.

### `#[derive(KdlChoice)]`

Use for enums selected by **node name**. Each variant corresponds to a different child node name.

This is especially useful for heterogeneous child lists.

### `#[derive(KdlMerge)]`

Generates `DeepMerge` support for layered config.

Use when a type participates in base + overlay merging.

### `#[derive(KdlPartial)]`

Generates `PartialConfig<T>` support for sparse overlays.

Use when you want a patch-like or override-only shape, usually with `Option<T>` fields.

### `#[derive(KdlSchema)]`

Generates only schema metadata.

Use when you need documentation or tooling shape information but not runtime decode/render behavior.

---

## Container-Level Attributes

These go on the struct or enum itself via `#[kdl(...)]`.

### `node = "name"` — Expected KDL Node Name

Sets the KDL node name used for parsing and rendering.

```rust
#[derive(KdlNode)]
#[kdl(node = "database")]
struct DatabaseConfig {
    #[kdl(attr)]
    host: String,
    #[kdl(attr)]
    port: i64,
}
```

```kdl
database host="localhost" port=5432
```

Use `node = "..."` on all public config structs. Without it, the KDL surface is usually less clear and less constrained.

### `rename_all = "..."` — Naming Convention

Transforms field and variant names for KDL.

Supported values:

| Value | Rust field | KDL key |
|---|---|---|
| `"kebab-case"` | `timeout_ms` | `timeout-ms` |
| `"snake_case"` | `timeoutMs` | `timeout_ms` |
| `"lowercase"` | `MaxRetries` | `maxretries` |
| `"UPPERCASE"` | `max_retries` | `MAX_RETRIES` |
| `"none"` | unchanged | unchanged |

Use `kebab-case` for user-facing KDL unless you have a strong reason not to.

### `default_placement = "..."` — Placement Policy for Unannotated Fields

Controls how unannotated fields are searched.

| Value | Behavior |
|---|---|
| `"exhaustive"` | try attr → positional → value → child |
| `"attr"` | only keyed attrs |
| `"value"` | only value child nodes |
| `"child"` | only child nodes |

Use this to set broad container style, but still prefer explicit field placement for public surfaces.

### `default_bool = "..."` — Default Bool Parsing Mode

| Value | Accepts |
|---|---|
| `"presence+value"` | flags and explicit bool values |
| `"value-only"` | only `key=#true` / `key=#false` |
| `"presence-only"` | only positive presence |

Use:
- `presence+value` for ergonomic configs,
- `value-only` when explicitness matters,
- `presence-only` only for true switch-like intent.

### `default_flag_style = "..."` — Default Bool Rendering Style

| Value | Typical render style |
|---|---|
| `"both"` | accept both styles; render canonical compact form |
| `"value|no"` | `enabled` / `no-enabled` |
| `"with|without"` | `with-feature` / `without-feature` |

### `default_conflict = "..."` — Duplicate Resolution

Controls what happens when the same field is found in multiple places or multiple times.

| Value | Behavior |
|---|---|
| `"error"` | reject duplicates |
| `"first"` | keep first |
| `"last"` | keep last |
| `"append"` | append into `Vec<T>` |

Use `error` as the safe default. Use `last` for override-driven configs. Use `append` only on vectors.

### `skip_serialize_none`

Omit `Option<T>` fields that are `None` when rendering.

Use this to keep rendered KDL clean.

### `skip_serialize_empty_collections`

Omit empty `Vec`, `HashMap`, and similar collection fields when rendering.

Use this to avoid noisy serializer-looking output.

### `deny_unknown`

Reject unknown attrs or child nodes.

Use this on strict, user-authored config surfaces where typos should become errors immediately.

### `selector = ...`

Used primarily on tagged enums to specify where the discriminator comes from.

Common forms:
- `arg(0)`
- `attr("type")`
- `name`
- `any(...)`

### `validate(func = "path")`

Runs a struct-level validator after decoding.

Signature:

```rust
fn(&Self) -> Result<(), String>
```

Use for invariants spanning multiple fields.

### `post_decode = "path"`

Runs after decoding and allows mutation/normalization.

Signature:

```rust
fn(&mut Self) -> Result<(), String>
```

Use for:
- normalization,
- derived defaults,
- canonicalization,
- final shaping before use.

---

## Field-Level Attributes

These go above individual fields with `#[kdl(...)]`.

## Placement Attributes

### `attr` — Keyed Attribute

Maps the field to `key=value`.

```rust
#[kdl(attr)]
host: String,
```

```kdl
server host="localhost"
```

This is the best fit for operational settings like ports, models, URLs, paths, and thresholds.

For `Vec<T>`, rendering can use repeated attrs if requested:

```rust
#[kdl(attr, render = "attr")]
ports: Vec<i64>,
```

```kdl
server ports=80 ports=443
```

### `keyed`

Alias for `attr`.

Use it when you want extra clarity in mixed-placement examples.

### `positional = N` — Positional Argument

Maps the field to the Nth positional argument.

```rust
#[kdl(positional = 0)]
name: String,
#[kdl(positional = 1)]
version: i64,
```

```kdl
package "my-lib" 2
```

Use positional args for identity or natural subjects, not for deep operational meaning.

### `positional = "rest"` — Collect Remaining Positional Args

Collects all remaining positional args into a `Vec<T>`.

```rust
#[kdl(positional = "rest")]
values: Vec<String>,
```

```kdl
config "a" "b" "c"
```

Use for simple argument-like lists.

### `value` — Value Child Node

Maps the field to a child node that carries scalar values as args.

```rust
#[kdl(value)]
tags: Vec<String>,
```

```kdl
config {
    tags "web" "api"
}
```

Scalar example:

```rust
#[kdl(value)]
threshold: i64,
```

```kdl
config {
    threshold 7
}
```

Use `value` when the setting reads better as a child node than as an inline attribute.

### `child` — Single Child Node

Maps the field to one nested child node.

```rust
#[kdl(child)]
database: DatabaseConfig,
```

```kdl
config {
    database host="localhost" port=5432
}
```

Use when the field is a structured concept with its own internal shape.

### `children` — Repeated Child Nodes

Maps to a `Vec<T>` of repeated child nodes.

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

Use repeated child nodes for structured repeated entries.

### `children_any` / `choice`

Collects heterogeneous child nodes into a `Vec<T>` where `T` is a choice enum.

Use this when child node names drive enum dispatch.

### `flag` / `flag = "token"` / `neg_flag = "token"`

For booleans, parse from presence-style flags.

```rust
#[kdl(attr, flag)]
enabled: bool,
```

```kdl
server enabled
server no-enabled
```

Custom tokens:

```rust
#[kdl(attr, flag = "on", neg_flag = "off")]
enabled: bool,
```

```kdl
server on
server off
```

Use flags only when the field is naturally switch-like in authored config.

### `registry` — Repeated Keyed Container Nodes

Collects repeated nodes into a keyed map or ordered key-value list.

```rust
#[kdl(registry, container = "provider")]
providers: HashMap<String, ProviderConfig>,
```

```kdl
config {
    provider "openai" model="gpt-4"
    provider "anthropic" model="claude-3"
}
```

By default the key comes from the first positional arg. Use this when every entry has the same container node name and a clear stable key.

### `children_map` — Keyed Child Collection

Collects children into a map where the key can come from the child name, an arg, an attr, or a selector.

Keyed by child name:

```rust
#[kdl(children_map)]
categories: HashMap<String, CategoryConfig>,
```

```kdl
config {
    docs { indexing chunk_size=3000 }
    code { indexing chunk_size=2000 }
}
```

Keyed by first arg with fixed node name:

```rust
#[kdl(children_map, map_node = "category")]
categories: HashMap<String, CategoryConfig>,
```

```kdl
config {
    category "docs" { indexing chunk_size=3000 }
    category "code" { indexing chunk_size=2000 }
}
```

Use `children_map` when the key source is part of the design, not just a registry convention.

### `modifier` — Merge Modifier Capture

Captures the node prefix modifier (`+`, `-`, `!`, `~`) into a `Modifier` or `Option<Modifier>` field.

```rust
#[kdl(modifier)]
modifier: Option<Modifier>,
```

```kdl
+provider "openai" model="gpt-4"
-provider "openai"
```

Use only in merge-aware layered config systems.

### `flatten` — Inline Nested Struct Fields

Inlines another struct's fields into the current node.

```rust
#[derive(KdlNode)]
#[kdl(node = "server")]
struct ServerConfig {
    #[kdl(attr)]
    host: String,

    #[kdl(flatten)]
    tls: TlsConfig,
}

#[derive(KdlNode)]
struct TlsConfig {
    #[kdl(attr)]
    cert: String,
    #[kdl(attr)]
    key: String,
}
```

```kdl
server host="example.com" cert="/path/cert.pem" key="/path/key.pem"
```

Use flatten only when the nested type is structural reuse and should not have its own visible namespace in KDL.

---

## Naming and Routing Attributes

### `name = "..."` / `rename = "..."`

Override the KDL key name for a field or variant.

```rust
#[kdl(attr, name = "max-retries")]
max_retries: u32,
```

Use this when the public KDL vocabulary should differ from the Rust field name.

### `alias = "..."` / `aliases = ...`

Accept alternate names during decoding.

Use aliases for compatibility and migration, not as equal-status style choices.

### `container = "..."`

For `registry`, specifies the repeated child node name.

### `map_node = "..."`

For `children_map`, specifies a fixed repeated child node name when keys come from args/attrs/selectors.

### `path = "..."`

Re-roots decoding through a child path before extracting the field.

Use sparingly. It is useful for compatibility or structural reuse, but explicit child structure is often clearer.

---

## Defaults, Optionality, and Skipping

### `required`

Forces the field to be present.

Use when omission should be a hard error.

### `optional`

Marks the field as optional.

Use when you want to make optionality explicit for readability or schema purposes.

### `default`

Use `Default::default()` when the field is missing.

### `default = ...`

Provides a literal default.

### `default_fn = "path"`

Provides a computed or shared default.

### `skip`

Exclude the field from KDL entirely.

Use for internal-only or derived fields that should not be serialized or parsed from config.

### `no_skip_serialize`

Opt out of container-level skip behavior for this field.

### `skip_serializing_if = "path"`

Custom render skip predicate.

---

## Bool and Conflict Overrides

### `bool = "..."`

Override bool parsing mode for one field.

| Mode | Accepts |
|---|---|
| `"presence+value"` | flags plus explicit values |
| `"value-only"` | only explicit bool values |
| `"presence-only"` | only positive presence |

Use field overrides when a specific bool has different ergonomics than the surrounding type.

### `flag_style = "..."`

Override how a bool renders.

| Style | Typical use |
|---|---|
| `"both"` | broad compatibility |
| `"value|no"` | compact default style |
| `"with|without"` | more phrase-like authored config |

### `conflict = "..."`

Override duplicate resolution for one field.

Use this when the field has intentionally different accumulation or override semantics than the container default.

### `render = "..."`

Forces canonical render placement even if decoding accepts multiple shapes.

This is extremely useful when you want to accept compatibility inputs but always render one canonical form.

---

## Scalar-Like and Conversion Attributes

### `scalar` / `value_type` / `value_like` / `kdl_value`

Tells the derive to treat a custom type as scalar-like for value/attr/positional placements.

Use this when a custom type should behave like a primitive KDL value.

### `from = "Type"` — Infallible Conversion

Parse the field as an intermediate type, then convert with `From<Intermediate>`.

Use this when:
- conversion always succeeds,
- the field type is a semantic wrapper,
- the KDL should stay simple.

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

#[derive(KdlNode)]
#[kdl(node = "config")]
struct Config {
    #[kdl(attr, from = "String")]
    provider: ProviderId,
}
```

```kdl
config provider="openai"
```

This is ideal for **newtypes** where the domain should not use raw primitives everywhere, but the config format should remain straightforward.

Common uses:
- `UserId(String)`
- `ProviderId(Arc<str>)`
- `Tag(String)`
- semantic wrappers that do not validate

### `try_from = "Type"` — Fallible Conversion

Parse as an intermediate type, then convert with `TryFrom<Intermediate>`.

Use this when:
- the domain type enforces invariants,
- bad values should fail decode,
- the type is more than a semantic wrapper.

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

#[derive(KdlNode)]
#[kdl(node = "server")]
struct ServerConfig {
    #[kdl(attr, try_from = "i64")]
    port: Port,
}
```

```kdl
server port=8080
```

This is ideal for validated domain types such as:
- `Port(u16)`
- `Percent(f64)`
- `Weight(f64)`
- `Slug(String)`
- bounded integer newtypes
- URL-like wrappers with validation

### Choosing `from` vs `try_from`

Use `from` when the conversion is representation-only.

Use `try_from` when the conversion is a validation boundary.

That distinction is one of the most important design decisions when mapping KDL into rich domain types.

### `Option<T>` and `Vec<T>`

These conversions are especially useful with:
- `Option<Newtype>`
- `Vec<Newtype>`
- `HashMap<String, Newtype>` value types

This keeps KDL authoring simple while preserving domain safety in Rust.

### When To Use `KdlValue` Instead

If the type should behave like a scalar everywhere, prefer implementing/deriving scalar behavior once rather than repeating `from` / `try_from` on many fields.

Use `from` / `try_from` when the conversion is field-local or when you want the conversion boundary to be explicit at the field.

---

## Enums

### Tagged Enums

Use when a node carries a discriminator in an arg, attr, or selector.

### Choice Enums

Use when the **node name** itself chooses the variant.

### Value Enums

Use when the enum is really a scalar value with a fixed vocabulary.

Rule of thumb:
- node-like behavior → tagged or choice enum
- scalar vocabulary → value enum

Always document and render one canonical form, even if the parser can accept more than one.

---

## Fragments

Fragments are expanded before typed parsing. That means the typed derive layer does **not** directly parse `fragment`, `with`, or `from`. It sees the expanded result.

Use fragments for reusable typed config templates.

Think of them as authoring-time macros for KDL, not as runtime Rust fields.

Use:
- `fragment` to define reusable config blocks,
- `with` to apply one inside an existing node,
- `from` to materialize a new node from one.

When explaining a config system, distinguish clearly between:
1. authored fragment syntax,
2. expanded KDL shape,
3. final typed Rust mapping.

---

## Merge and Partial Overlays

### `KdlMerge`

Use to define how layered configs combine.

Common field policies:
- `deep` — recursively merge nested structured fields
- `keep` — preserve base
- `replace` — overlay wins
- `append` — extend collections
- custom merge function — domain-specific behavior

Use this when config is loaded from multiple layers such as:
- defaults,
- environment,
- profile,
- local override.

### `KdlPartial`

Use for sparse patch-like overlays.

This is ideal for:
- developer override files,
- admin patches,
- UI-generated partial edits,
- optional override layers.

### Merge Modifiers

When layered loading is active, node prefixes carry meaning:

- `+node` — append
- `-node` — remove
- `!node` — replace
- `~node` — flatten/merge into target

Only use or teach these when the config system actually supports merge-aware layering. They are powerful, but they should not appear in a simple single-file config unless layering is part of the design.

---

## Validation

Validation keeps config errors close to the owning field or type.

### Field-Level Validation

Use for numeric, string, and collection constraints.

Typical examples:
- `min`, `max`, `range`
- `non_empty`
- `min_len`, `max_len`
- `min_items`, `max_items`

### Cross-Field Validation

Use when one field constrains another.

Typical examples:
- `min_port <= max_port`
- timeout relationships
- one field must exist in another collection

### Struct-Level Validation

Use for richer invariants that do not fit neatly on one field.

### `post_decode` vs `validate`

Use `post_decode` when you want to normalize or fill derived state.

Use `validate` when you want to reject invalid final state.

---

## Schema Generation

Use schema support when:
- editor tooling needs a machine-readable shape,
- docs should stay aligned with the Rust type,
- validation and UX layers need descriptions or requiredness metadata.

Schema overrides should clarify the intended interface, not contradict actual runtime behavior.

---

## Canonical Authoring Heuristics

Use these rules when designing or reviewing a KDL surface:

1. Put **identity** in positional args only when it reads naturally as the subject of the node.
2. Put **operational meaning** in keyed attrs.
3. Use **value nodes** for scalar child groups and simple lists.
4. Use **child nodes** for nested structure.
5. Use **repeated child nodes** for structured repeated entries.
6. Use **registry** when repeated entries share a container node and keying convention.
7. Use **children_map** when the key source is part of the language design.
8. Use **flatten** only for implementation reuse, not to erase meaningful namespaces.
9. Prefer one **canonical rendered form**.
10. Treat compatibility inputs as decode flexibility, not as equal-status authoring styles.
11. Use `render = ...` to normalize multi-shape parsing into one stable output.
12. Prefer `rename_all = "kebab-case"` for user-facing config.
13. Use `deny_unknown` on strict user-authored config surfaces.
14. Use `from` / `try_from` to keep KDL simple while keeping Rust domain types strong.

---

## Red Flags

- Relying on exhaustive placement for a public config language without documenting a canonical form
- Using positional args for operational settings just because it is shorter
- Flattening concepts that deserve a child namespace
- Using `append` on non-vector fields
- Treating aliases as peer style choices instead of compatibility bridges
- Using `presence-only` bools where explicit false is important
- Forgetting that merge modifiers only make sense in layered systems
- Avoiding domain newtypes because “KDL only has strings and ints” instead of using `from` / `try_from`
- Designing KDL that looks like a serializer dump instead of something a person would author

---

## Agent Response Style

When answering with this skill:

- explain the type shape before showing final KDL,
- show the canonical authored form first,
- mention alternate accepted forms only when debugging or compatibility requires it,
- explain why a placement was chosen, not just what it is called,
- and when `from` / `try_from` is involved, explain the domain boundary clearly.

A good answer should tell the user not just **what** the derive does, but **why** they would choose it.
