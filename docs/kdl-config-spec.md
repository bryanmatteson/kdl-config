# KDL Config Tags and Behavior Spec

This spec defines the KDL mapping rules, tags, defaults, and resolution behavior for the configuration system built on KDL 6.5.0. It is intended for a runtime parsing crate and a serde integration derive crate.

**Scope**
- Define how Rust fields map to KDL attributes and child nodes.
- Establish sane defaults, including exhaustive placement checking.
- Define boolean semantics, built-in flags, and deterministic conflict resolution.
- Define how defaults and overrides are set at the root or via parse config.

**KDL Forms**
- Attribute form: `node attr1 attr2=value {}`
- Child form: `node { attr1 attr2 value }`

In child form, `attr1` and `attr2` are child node names. A child “value node” is written as `key <args...>` and stores its values in that child’s args.

## Tags

**Struct-Level**
- `#[kdl(node = "name")]`: expected node name for parsing, and default name for rendering.
- `#[kdl(choice)]`: for `#[derive(Kdl)]` on enums, use choice mode (parse by node name).
- `#[kdl(value)]`: for `#[derive(Kdl)]` on enums or newtype structs, use value mode (scalar conversion).
- `#[kdl(selector = <selector>)]` or `#[kdl(select(...))]`: override enum discriminator selection (default is `arg(0)`).
- `#[kdl(rename_all = "kebab-case" | "snake_case" | "lowercase" | "UPPERCASE" | "none")]`: rename strategy for field keys and enum variants.
- `#[kdl(default_placement = "exhaustive" | "attr" | "value" | "child")]`: default placement policy.
- `#[kdl(default_bool = "presence+value" | "value-only" | "presence-only")]`: default boolean behavior.
- `#[kdl(default_flag_style = "both" | "value|no" | "with|without")]`: default flag aliases.
- `#[kdl(default_conflict = "error" | "first" | "last" | "append")]`: conflict policy for multiple candidates.
- `#[kdl(deny_unknown)]`: error on unknown attributes or children.
- `#[kdl(validate(func = "path"))]`: struct-level validation. Called with `fn(&Self) -> Result<(), String>` after all fields are decoded and field-level validations pass. See [Validation](#validation).

**Field-Level Placement**
- `#[kdl(attr)]`: allow attribute placement. For non-bool fields, this means keyed `key=value`. For bool fields, this means keyed values plus flag tokens.
- `#[kdl(attr, keyed)]`: keyed attribute `key=value`.
- `#[kdl(attr, positional = N)]`: positional argument `arg[N]` (raw index in the header args list).
- `#[kdl(attr, positional = "rest")]` or `#[kdl(attr, positional)]`: collect all positional args into a `Vec<T>` (only valid for `Vec<T>` or `Option<Vec<T>>`).
- `#[kdl(attr, flag)]`: allow flag tokens for booleans using default flag names (flags only). To also allow keyed values, use `#[kdl(attr)]` or `#[kdl(attr, keyed)]`.
- `#[kdl(attr, flag = "on", neg_flag = "off")]`: override flag token names.
- `#[kdl(value)]`: allow child value node `key <args...>`.
- `#[kdl(child)]`: allow child struct node `key { ... }`.
- `#[kdl(children)]`: allow multiple child struct nodes `key { ... }` into `Vec<T>`.
- `#[kdl(children_any)]` or `#[kdl(choice)]`: allow any child node into a `Vec<T>` of choice enums (variant selected by node name).
- `#[kdl(flatten)]`: merge the child node's fields into the current struct (decode-only; rendering uses child placement).
- `#[kdl(registry)]`: parse a registry map from repeated nodes keyed by their **first positional arg**.
  - Override key source with `#[kdl(registry, key_arg = N)]`, `#[kdl(registry, key_attr = "id")]`, or `#[kdl(registry, key_fn = "path")]`.
- `#[kdl(children_map)]`: parse all children into a map keyed by their **node name**.
  - Field type must be `HashMap<K, V>`, `Vec<(K, V)>`, or `Option<Vec<(K, V)>>`.
  - With `map_node = "name"`: only children named `name` are collected; key comes from **first positional arg**.
  - Override key source with `key_arg = N`, `key_attr = "id"`, or `key_fn = "path"` (requires `map_node`).

Note: for bool fields, `attr` includes flags only when presence is enabled (`bool = "presence+value"` or `"presence-only"`). With `bool = "value-only"`, `attr` means keyed values only. In `presence-only` mode, only the **positive** flag token is valid; negative flags are rejected.
If `keyed` is explicitly set, flags are not considered unless `flag` is also set.

**Field-Level Modifiers**
- `#[kdl(name = "custom-key")]` or `#[kdl(rename = "custom-key")]`: override KDL key name.
- `#[kdl(path = "a.b.c")]`: re-root decoding at the child path (relative by default). Use a leading `/` for absolute paths (e.g., `"/app.http"`). Rendering is unchanged (path is decode-only).
- `#[kdl(container = "custom-container")]`: override the container node name for `registry` fields (the repeated node name).
- `#[kdl(required)]` or `#[kdl(optional)]`: override required-ness.
- `#[kdl(default)]`, `#[kdl(default = "...")]`, `#[kdl(default_fn = "path")]`: default value.
- `#[kdl(bool = "presence+value" | "value-only" | "presence-only")]`: override bool behavior.
- `#[kdl(flag_style = "both" | "value|no" | "with|without")]`: override flag aliases.
- `#[kdl(conflict = "error" | "first" | "last" | "append")]`: override conflict policy for this field.
- `#[kdl(render = "attr" | "value" | "child" | "children" | "registry")]`: canonical render target.
- `#[kdl(skip)]`: skip both parsing and rendering; the field is set to `Default::default()` when parsing.
- `#[kdl(skip_serializing_if = "path")]`: rendering-only skip predicate.
- `#[kdl(scalar)]` (aliases: `#[kdl(value_type)]`, `#[kdl(value_like)]`, `#[kdl(kdl_value)]`): treat custom type as scalar for attr/positional/value placements (requires `FromKdlValue` implementation).
- `#[kdl(modifier)]`: capture the node's signal modifier (`+`, `-`, `!`) into a `Modifier` or `Option<Modifier>` field.
- `#[kdl(validate(...))]`: decode-time validation rules. Multiple rules may be combined in a single `validate(...)` or across repeated attributes. See [Validation](#validation).

Note: `#[kdl(modifier)]` cannot be combined with other placements and only one modifier field is allowed per struct or enum variant.

## Registry / Keyed Nodes

Registry fields map repeated nodes keyed by their **first positional argument**. This supports patterns like:

```
config-node first { ... }
config-node second { ... }
config-node third key=value { ... }
```

Rust shape (map):
```rust
struct Test {
    #[kdl(registry, container = "config-node")]
    config_nodes: HashMap<String, ConfigNode>,
}
```

Rust shape (ordered):
```rust
struct Test {
    #[kdl(registry, container = "config-node")]
    config_nodes: Vec<(String, ConfigNode)>,
}
```

Optional ordered shape:
```rust
struct Test {
    #[kdl(registry, container = "config-node")]
    config_nodes: Option<Vec<(String, ConfigNode)>>,
}
```

Rules:
- The **container name** defaults to the field key (after rename). You can override it with `container = "..."`.
- By default, each `config-node <key> { ... }` entry uses its **first positional arg** as the map key: `first`, `second`, `third`.
  - `key_arg = N` uses positional arg `N` instead.
  - `key_attr = "id"` uses the `id=` attribute as the key.
    - The attribute must have exactly one value.
    - By default, the key attribute is **consumed** (removed before parsing the inner type). Use `preserve` to keep it.
  - `key_fn = "path"` calls a helper `fn(&KdlNode) -> Result<String, KdlConfigError>` to compute the key.
    - The helper does not mutate or strip data from the node; if the key lives in attrs/args, the inner type must accept it or use `key_attr` / `key_arg`.
- The node’s **body and attributes** are parsed into `ConfigNode` as usual (it can itself have attrs, values, or children).
- The key must be a string value; non-string keys are an error.
- Missing container yields an empty map.
- For `Option<Vec<(String, T)>>`, missing container yields `None`.
- If duplicate keys appear, conflict policy applies (`error`, `first`, or `last`).
  - For `Vec<(String, T)>`, `last` removes the earlier entry and appends the last occurrence (preserving document order).
  - For `Vec<(String, T)>`, `append` keeps all entries (including duplicates) in document order.
  - For `HashMap<String, T>`, `append` is invalid.

Rendering:
- The map renders as repeated nodes named `config-node`.
- Each entry renders as `config-node <key> { ... }`.
  - For `HashMap`, entries are rendered sorted by key for determinism.
  - For `Vec<(String, T)>`, entries are rendered in vector order.

## Children Maps

Children maps collect all (or specific) child nodes into a map, with flexible key extraction.

**Key from child name** (basic case):
```
config {
    docs { indexing chunk_size=3000 }
    code { indexing chunk_size=2000 }
}
```

Rust shape:
```rust
struct Config {
    #[kdl(children_map)]
    categories: HashMap<String, CategoryOverrides>,
}
```

Each child node's **name** becomes the map key; its body is parsed into the value type.

**Key from first arg** (with `map_node`):
```
config {
    category "docs" { indexing chunk_size=3000 }
    category "code" { indexing chunk_size=2000 }
}
```

Rust shape:
```rust
struct Config {
    #[kdl(children_map, map_node = "category")]
    categories: HashMap<String, CategoryOverrides>,
}
```

Only children named `category` are collected; the **first positional arg** becomes the key.

Rules:
- Field type must be `HashMap<K, V>`, `Vec<(K, V)>`, or `Option<Vec<(K, V)>>`.
- Without `map_node`: all children are collected; key = child name.
- With `map_node = "name"`: only children named `name` are collected; key = first positional arg.
  - `key_arg = N` uses positional arg `N` instead.
  - `key_attr = "id"` uses the `id=` attribute as the key.
    - The attribute must have exactly one value.
    - By default, the key attribute is **consumed** (removed before parsing the inner type). Use `preserve` to keep it.
  - `key_fn = "path"` calls a helper `fn(&KdlNode) -> Result<K, KdlConfigError>` to compute the key.
- Keys are converted to `K` using `FromKdlValue`; invalid conversions are errors.
- Without `map_node`, the key comes from the child name (string), so non-string key types will not parse.
- Missing children yields an empty map.
- For `Option<Vec<(K, V)>>`, missing children yields `None`.
- If duplicate keys appear, conflict policy applies (`error`, `first`, `last`, or `append`).
  - For `Vec<(K, V)>`, `append` keeps all entries in document order.
  - For `HashMap`, `append` is invalid.
- `children_map` cannot be combined with `container` (use `registry` for that pattern).

Rendering:
- Without `map_node`: each entry renders as `<key> { ... }`.
- With `map_node`: each entry renders as `<map_node> <key> { ... }`.
- `HashMap` renders sorted by key; `Vec<(K, V)>` preserves order.

## Selectors (`select(...)`)

Selectors are an advanced way to extract a key or discriminator from a node. They are supported on
`registry`, `children_map`, and tagged enums (`#[derive(Kdl)]`). For `child`/`children` fields, selectors
are only used when `inject` is enabled.

Selector forms:
- `arg(N)`: use positional argument `N`
- `attr("name")`: use the value of attribute `name`
- `name`: use the node name
- `func("path::to::fn")`: call a helper `fn(&KdlNode) -> Result<K, KdlConfigError>`
- `any(...)`: try selectors in order and use the first match

Options:
- `consume`: remove the selected token before parsing (default)
- `preserve`: keep the selected token
- `inject` / `inject="field"`: inject the selected value into a field (only for `child`/`children`/`registry`/`children_map`)

Examples:

```
#[kdl(registry, container = "item", select(attr("id"), consume))]
```

```
#[kdl(children_map, map_node = "category", select(attr("name")))]
```

```
#[kdl(node = "choice", select = any(attr("type"), arg(0)))]
```

## Tuple and Unit Structs

- Unit structs parse nodes with no args, attrs, or children.
- Tuple structs map fields to positional arguments (`arg[0]`, `arg[1]`, ...).
  - Use `#[kdl(positional = N)]` on a field to override its index.
  - Tuple fields must be scalar value types (string, number, bool, or `Option<T>`).
  - Defaults and `optional` are supported.

## Enums

Enums are encoded as **tagged nodes** with a discriminator selected by `selector`/`select(...)` (default is `arg(0)`).

Example:
```
test value
test with-struct-type {}
test with-struct { key "" }
```

Rust shape:
```rust
#[derive(KdlNode)]
#[kdl(node = "test", rename_all = "kebab-case")]
enum Test {
    Value,
    WithStructType(NewStruct),
    WithStruct {
        #[kdl(value, render = "value")]
        key: String,
    },
}
```

Rules:
- The **outer node name** matches `#[kdl(node = "...")]` when provided.
- The **variant discriminator** is selected by `selector`/`select(...)` (default is `arg(0)`).
  - Default discriminator is the variant name (string, after `rename_all`).
  - Use `#[kdl(tag = <literal>)]` to set a non-string discriminator (int/float/bool) or override the string tag.
- By default, the discriminator is **consumed** (removed) before parsing the variant payload; use `preserve` to keep it.
- The remaining args/attrs/children are parsed as the variant payload.
- Unit variants accept **no additional args/attrs/children**.
- Tuple variants consume positional args in order after the discriminator.
  - Attributes/children are not allowed for tuple variants.
- Newtype variants are decoded with `KdlDecode` from the same node, with name checking disabled for the inner type.
- Struct variants (named fields) are parsed like structs using the enum’s defaults and overrides.

Rendering:
- Enums render as `outer <tag> ...`.
- String tags are rendered as identifiers when possible (quoted if needed).
- Non-string tags render using KDL literal forms (e.g., `#true`, `1`, `3.14`).
- Struct/newtype/tuple variants render their payload inline with canonical placement rules.

### Choice Enums (`KdlChoice` / `#[kdl(choice)]`)

Choice enums select a variant by the **node name**, not a discriminator arg.

Rules:
- Variants must be unit or single-field newtypes.
- Variant names use `rename_all` and `#[kdl(name = "...")]`.
- Unit variants accept **no args/attrs/children**.
- Newtype variants parse the inner type using the variant name as the node name (inner type name must match unless it allows any name).
- Rendering emits a node with the variant name; newtype variants render the inner body.

### Value Enums / Newtypes (`KdlValue` / `#[kdl(value)]`)

Value mode maps to scalar values instead of nodes.

Rules:
- Unit enums map to string values; `rename_all` and `#[kdl(name = "...")]` control the string tag.
- Newtype structs delegate to the inner type’s `FromKdlValue` implementation.

## Defaults and Overrides

**Defaults**
- Default placement policy: `exhaustive`.
- Default bool behavior: `presence+value`.
- Default flag aliases: both `key/no-key` and `with-key/without-key`.
- Default conflict policy: `error`.
- Default rendering: attr keyed for scalars, value node for `Vec<T>` of scalars, child node for nested structs/enums, and `children` for `Vec<T>` of nested types.
- Unknown keys: allowed unless `deny_unknown` is set.
  - Unknown means any attribute or child node that does not match any field placement for the current struct.

**Override Precedence**
1. Field-level tags.
2. Root struct tags.
3. Runtime parse config defaults.

## Placement Semantics

**Attribute Placement**
- Keyed: `key=value`.
- Positional: `arg[N]`.
- Flags: bare tokens, only for bools.
  - Positional indices are based on the **raw arg list**. Flag tokens do **not** remove or reorder args.
  - If a positional arg string matches a flag token and both are enabled, this produces multiple candidates and the conflict policy applies.

**Child Placement**
- Value node: `key <args...>`.
- Child struct: `key { ... }`.
- Multiple children: repeated `key { ... }` into `Vec<T>`.
- Registry: repeated `<container> <key> { ... }` into `HashMap<String, T>` or `Vec<(String, T)>` (only when `#[kdl(registry)]` is set).

## Fragment Mechanics (kdl_config layer)

Fragments are a kdl_config–level primitive resolved before application parsing. They provide reusable, optionally typed configuration templates that may either:
- insert nodes into a target block, or
- merge/flatten into the target node itself.

Fragments are expanded away entirely before `#[derive(Kdl)]` parsing.
The application never sees `fragment`, `with`, or patch nodes.

**Overview**

```
fragment "name" {
  <insert-node>*
  <merge-patch>*
}

<target-node> {
  with "name"
  ...
}
```

- Insert nodes are normal KDL nodes and are inserted as children.
- Merge patches are nodes whose names begin with `~` and are flattened into the target node using deep-merge semantics.
- Insert and merge may coexist in the same fragment.
- Overrides at the with site always win.

**Fragment Definition**

```
fragment "<fragment-name>" {
  <fragment-entry>*
}
```

- `<fragment-name>` must be a string.
- Type annotations on fragment nodes are optional:
  - `(source)fragment "code"` explicitly types the fragment and validates `with` invocations against that type.
  - `fragment "code"` is implicitly typed on first `with` invocation and must match subsequent invocations.
- Fragments may appear at top-level or inside other nodes (lexical scoping optional).
- Fragments are removed from the document after expansion.

**Fragment Entries**

A fragment may contain two kinds of entries:
- Insert nodes — normal nodes (no modifier)
- Merge patches — nodes whose name begins with `~`

**Insert Nodes (no flatten)**

Insert nodes are copied verbatim into the target node’s body.

```
fragment "qdrant-hnsw" {
  hnsw {
    ef 128
    m 16
    ef-search 128
  }
}

vectors "production" qdrant {
  with "qdrant-hnsw"
}
```

Expansion result:
```
vectors "production" qdrant {
  hnsw {
    ef 128
    m 16
    ef-search 128
  }
}
```

Rules:
- Insert nodes are appended as children in the order they appear.
- Insert nodes must be valid children under the target node’s schema.
- Invalid insertions are a schema error.
- Insert nodes do not merge with existing children unless the application’s schema allows it later.

Insert nodes are ideal for:
- `hnsw { ... }`
- `language "rust" { ... }`
- `crawler { ... }`
- any block that naturally belongs inside the target.

**Merge Patches (`~` — flatten semantics)**

Merge patches are nodes whose name begins with `~`. They do not insert a child node. Instead, they merge into the target node itself.

```
fragment "local-defaults" {
  ~source local {
    chunking { max-size 1500; overlap 200 }
    enrichment { symbols; hover; definitions }
    embed "code-model"
  }
}
```

Applied to:
```
source "app" local "." {
  with "local-defaults"
}
```

Expansion result:
```
source "app" local "." {
  chunking { max-size 1500; overlap 200 }
  enrichment { symbols; hover; definitions }
  embed "code-model"
}
```

**Patch Syntax**

```
~<target-node> [<discriminator>] [<attrs...>] { <children...> }
```

- `<target-node>`: base node name (e.g. `source`, `embed`, `vectors`)
- `<discriminator>` (optional): matched against the target’s discriminator
- Patch attrs and children describe what to merge

Examples:
```
~source local { ... }
~source git { ... }
~vectors qdrant { ... }
~embed onnx { ... }
~search { limit 20 }
```

**Patch Matching Rules**

When expanding `with "<fragment>"` inside a target node `N`:
1. Collect merge patches where:
   - `patch.target_node == N.name`
2. Among those:
   - Prefer an exact discriminator match

```
~source local  // matches source ... local ...
```

   - Otherwise allow a fallback patch with no discriminator:

```
~source { ... }
```

3. Errors:
   - More than one equally-specific match → error
   - No matching patch → no merge occurs (insert nodes may still apply)

This allows fragments like:
```
fragment "code" {
  ~source local { ... }
  ~source git { ... }
  language "rust" { server "rust-analyzer" }
}
```

**Merge Semantics (`~`)**

Merge patches flatten into the target node using deep merge rules equivalent to Layer:

Header:
- Patch attributes merge into target attributes (last-wins).
- Patch positional args:
  - Not allowed by default (too ambiguous).
  - If enabled later, must use explicit selector rules.

Children are merged recursively:

| Type | Behavior |
| --- | --- |
| Scalar | Last wins |
| Vec&lt;T&gt; | Append if conflict = append, else last |
| Struct | Recursive merge |
| Registry | Merge by key, last wins |

Order of application within a target node:
1. Apply merge patch (`~`)
2. Insert insert-nodes
3. Apply with-site overrides (attrs + children)
4. Existing node content remains; later content wins

This preserves read-as-written semantics.

**Overrides at the with Site**

`with` may carry overrides in both attribute and child forms.

```
source "generated" local "./generated" {
  with "local-defaults" no-enrichment chunking.max-size=500
}
```

Attribute expansion:
- `chunking.max-size=500` → `chunking { max-size 500 }`
- `embed="prose-model"` → `embed "prose-model"`
- `no-enrichment` → child flag node

Overrides are expanded before merging and always win over fragment content.

**Combined Example (Insert + Merge)**

```
fragment "code" {
  // Insert
  language "rust" { server "rust-analyzer" }

  // Merge
  ~source local {
    chunking { max-size 1500; overlap 200 }
    enrichment { symbols; hover; definitions }
    embed "code-model"
  }

  ~source git {
    chunking { max-size 1500; overlap 200 }
    enrichment { symbols; hover; definitions }
    embed "code-model"
  }
}

source "app" local "." {
  with "code"
  include "src/**"
}
```

Result:
```
source "app" local "." {
  language "rust" { server "rust-analyzer" }
  chunking { max-size 1500; overlap 200 }
  enrichment { symbols; hover; definitions }
  embed "code-model"
  include "src/**"
}
```

**IDE & Schema Integration**

Because merge patches are typed nodes (`~source local { ... }`), IDEs can:
- Validate children against the correct schema
- Offer autocomplete inside fragment bodies
- Reject invalid insertions early

Suggested schema model:
- fragment children:
  - normal nodes allowed under any target
  - patch nodes `~<node>` validated as `<node>` schema
- `with "<name>"`:
  - context-aware suggestions based on available fragments
  - error if no matching patch or valid insert applies

**Error Cases**

Errors must include both definition and with locations.

| Error | Description |
| --- | --- |
| Unknown fragment | `with "missing"` |
| Duplicate patch | Two `~source local` patches |
| Invalid insert | Insert node not allowed under target schema |
| Circular fragment | Fragments reference each other |

**Summary**
- Insert nodes = structural reuse
- `~` merge patches = flatten + override
- Typed, schema-aware, IDE-friendly
- No new KDL syntax
- No runtime indirection
- Aligns with Layer semantics

## Signals / Modifiers

Node names may be prefixed with a modifier signal:
- `+name`: append semantics
- `-name`: remove semantics
- `!name`: replace semantics
- `~name`: flatten semantics (merge the node’s contents into the target)

These modifiers are accepted on any node (top-level, child, registry entry). For field matching, the prefix is stripped and the base name is used. The modifier is preserved on the parsed node for downstream merge logic.

Examples:
```
config {
  +child {}
  -child {}
  !enabled {}
}
```

Modifiers apply only to node names, not to attribute tokens. Boolean negation uses `no-key` / `without-key`, not `!key`.
Modifiers are only recognized on bare identifiers. Quoted node names that start with `+`, `-`, or `!` are treated as literal names and do not carry modifier semantics.

## Type Compatibility

- Attribute keyed/positional/value nodes require the field type to implement value conversion (e.g., `String`, `PathBuf`, numbers, `bool`, `Vec<T>`, `Option<T>`, or a custom type implementing `FromKdlValue` / `KdlValue`).
- Positional list placement requires `Vec<T>` or `Option<Vec<T>>` with value-compatible `T`.
- Child/children require the field type to implement nested parsing (e.g., `KdlDecode` for structs/enums).
- `children` requires `Vec<T>` or `Option<Vec<T>>`.
- `registry` requires `HashMap<String, T>`, `Vec<(String, T)>`, or `Option<Vec<(String, T)>>`.
- `children_map` requires `HashMap<K, V>`, `Vec<(K, V)>`, or `Option<Vec<(K, V)>>`.
- If a field is `Vec<T>` of nested types and placement is `child`, it is treated as `children`.
- If a field is `Vec<T>` of scalar types and placement is `value`, it uses a single value node with multiple args.

## Exhaustive Resolution (Default)

When placement is not constrained, parsing checks all **type-compatible** placements and resolves a single value. Steps:
1. Determine allowed placements for the field.
2. Collect candidates from each placement, in deterministic order.
3. If no candidates, apply default or missing-required error.
4. If one candidate, use it.
5. If multiple candidates, apply conflict policy.

Notes:
- `default_placement = "attr" | "value" | "child"` restricts candidates to that placement only (and errors if the type is incompatible).
- `registry` is only enabled when `#[kdl(registry)]` is set.

## Deterministic Candidate Order

The parser checks placements in this order:
1. Attribute keyed value: `key=value`.
2. Attribute positional value: `arg[N]` (or all args for `positional = "rest"`).
3. Attribute flags: `key`, `no-key`, `with-key`, `without-key`.
4. Child value node: `key <args...>`.
5. Child struct node: `key { ... }`.

Within a placement, explicit boolean values are checked before presence flags.

## Conflict Policy

- `error` (default): any multiple candidates is an error.
- `first`: first candidate in deterministic order wins.
- `last`: last candidate wins.
- `append`: only for `Vec<T>`, concatenates candidates in order.

Conflicting positive and negative flags within the same placement is always an error.
For `append`, candidates are concatenated by placement order, then by document order within the same placement.
Using `append` on non-`Vec<T>` fields is an error.

## Boolean Semantics

Booleans allow explicit values and presence by default:
- Explicit: `key=#true` or `key=#false`.
- Presence true: `key` or `with-key`.
- Presence false: `no-key` or `without-key`.
- In child placement, presence true is a child node with no args, no attrs, and no children (e.g., `key` or `key {}`).

If `bool = "value-only"`, flag tokens are disabled and only explicit values are accepted.
`flag_style` and `default_flag_style` apply only when presence is enabled.
In `value-only` mode, a child node with no args (e.g., `key {}`) is treated as missing/invalid.

Absence yields:
- `bool` -> `false`.
- `Option<bool>` -> `None`.

**Boolean Resolution Table (Default Conflict Policy = error)**

| Explicit Value | Presence True | Presence False | Result |
| --- | --- | --- | --- |
| none | none | none | `false` (or `None`) |
| `#true` | none | none | `true` |
| `#false` | none | none | `false` |
| none | true | none | `true` |
| none | none | true | `false` |
| `#true` | true | none | error (multiple candidates) |
| `#false` | true | none | error (multiple candidates) |
| `#true` | none | true | error (multiple candidates) |
| `#false` | none | true | error (multiple candidates) |
| none | true | true | error (conflicting flags) |

When conflict policy is `first` or `last`, the deterministic order applies, with explicit values checked before presence flags in the same placement. Conflicting positive and negative flags still error.

## Rendering

Rendering always chooses a canonical placement, even if parsing is exhaustive. Default rendering rules:
- Scalars render as keyed attributes.
- `Vec<T>` of scalars render as value nodes.
- Nested structs/enums render as child nodes.
- `Vec<T>` of nested types render as `children`.

Boolean rendering defaults:
- `bool`: render `key` when `true`, render nothing when `false`.
- `Option<bool>`: `Some(true)` -> `key`, `Some(false)` -> `no-key` (or `without-key` if configured), `None` -> nothing.
- When `bool = "value-only"`, render `key=#true` / `key=#false`.

Flag style expansion:
- `value|no`: `key` / `no-key`.
- `with|without`: `with-key` / `without-key`.
- `both`: accept both, render using `value|no` unless overridden.

`key` here is the field key after rename rules are applied.

Use `#[kdl(render = "attr" | "value" | "child" | "children" | "registry")]` to override per field.

### Fragment-Aware Updates

Round-trip parsing preserves `fragment` and `with` nodes. When rendering updates:
- The default update path may emit **explicit overrides** (expanded children) and keep fragments unchanged.
- A **fragment-aware** update mode can be used to push changes back into fragment definitions when it is safe:
  - If **all `with` invocations** of a fragment result in identical updated children, the fragment definition is updated.
  - Otherwise the fragment remains unchanged and per-`with` overrides are emitted after the `with` node.
- If `with` overrides are supported, they are treated as **local** and remain after the `with` node.
- Attribute overrides on `with` nodes are preserved when possible and updated from the expanded override view.

## Examples

**Scalar with exhaustive placement**
```
foo limit=10
foo { limit 10 }
```

**Booleans with built-in flags**
```
feature enabled
feature no-enabled
feature enabled=#true
feature { enabled }
```

**Vec with append conflict policy**
```
foo include="a"
foo { include "b" "c" }
```

## Schema Generation

Derive `KdlSchema`, or add `#[kdl(schema)]` to `#[derive(Kdl)]`, to generate schema metadata.
Union types are supported only for schema generation (no node/choice/value modes).

**Struct-Level Schema Overrides**
- `#[kdl(schema(name = "custom-name"))]` or `#[kdl(schema(rename = "custom-name"))]`: override schema node name.
- `#[kdl(schema(description = "..."))]` or `#[kdl(schema(desc = "..."))]`: add description to schema.
- `#[kdl(schema(deny_unknown))]`: mark schema as strict (no unknown properties allowed).
- `#[kdl(schema(allow_unknown))]`: mark schema as non-strict.

**Field-Level Schema Overrides**
- `#[kdl(schema(name = "custom-name"))]` or `#[kdl(schema(rename = "custom-name"))]`: override field name in schema.
- `#[kdl(schema(description = "..."))]` or `#[kdl(schema(desc = "..."))]`: add description to field.
- `#[kdl(schema(type = "string" | "integer" | "int" | "float" | "number" | "boolean" | "bool" | "null"))]`: override scalar type (only for scalar value fields).
- `#[kdl(schema(required))]` or `#[kdl(schema(optional))]`: override required-ness in schema even if parse-time required-ness differs.
- `#[kdl(schema(skip))]`: exclude field from schema generation (parsing/rendering still apply).

Example:
```rust
#[derive(Kdl)]
#[kdl(node = "config", schema(name = "app_config", description = "Application config"))]
struct Config {
    #[kdl(attr)]
    #[kdl(schema(description = "Application title"))]
    name: String,

    #[kdl(positional = 0)]
    #[kdl(schema(type = "integer", description = "Primary count"))]
    count: i64,

    #[kdl(attr)]
    #[kdl(schema(skip))]
    internal: String,
}
```

Notes:
- `schema(type = ...)` is only valid for scalar value fields.
- `schema(type)` accepts `int`, `number`, and `bool` aliases.
- `schema(skip)` excludes only from schema; parsing and rendering still apply to the field.
- `schema(deny_unknown)` and `schema(allow_unknown)` accept optional boolean values.
- Union types can generate `choice` schemas for alternative value types.
- Schema outputs may use `type=[...]` to express multiple scalar types (e.g., duration values).
- Schema outputs may include a `type_annotation` block to describe required or allowed type annotations.
- Fragment schemas should:
  - allow both insert nodes and `~` patch nodes as children,
  - validate patch nodes against the referenced target schema, and
  - support context-aware validation for `with "<name>"` within a target node.

## Validation

Validation rules are specified with `#[kdl(validate(...))]` and are enforced during `KdlDecode`. Rules are checked **after** a field is fully decoded. Validation failures produce a `KdlConfigError` with `ErrorKind::InvalidValue`.

**Attribute syntax**

Rules can be specified as nested meta or as a DSL string:
- Nested meta: `#[kdl(validate(min = 1, max = 100, non_empty))]`
- DSL string: `#[kdl(validate = "min(1) max(100) non_empty")]`

Multiple `validate(...)` attributes on the same field are merged.

**Numeric rules** (applied to `i8`–`i64`, `u8`–`u64`, `f32`, `f64`, `isize`, `usize`):

| Rule | Syntax | Condition |
| --- | --- | --- |
| Min | `min = N` | value ≥ N |
| Max | `max = N` | value ≤ N |
| Range | `range(MIN, MAX)` | MIN ≤ value ≤ MAX |
| MultipleOf | `multiple_of = N` | value % N ≈ 0 |
| Positive | `positive` | value > 0 |
| Negative | `negative` | value < 0 |
| NonNegative | `non_negative` | value ≥ 0 |
| NonPositive | `non_positive` | value ≤ 0 |

**String rules** (applied to `String`):

| Rule | Syntax | Condition |
| --- | --- | --- |
| NonEmpty | `non_empty` | length > 0 |
| MinLen | `min_len = N` | length ≥ N |
| MaxLen | `max_len = N` | length ≤ N |
| Len | `len(MIN, MAX)` | MIN ≤ length ≤ MAX |
| Pattern | `pattern = "regex"` | schema-only (no runtime regex check) |
| Ascii | `ascii` | all chars are ASCII |
| Alphanumeric | `alphanumeric` | all chars are alphanumeric |

**Collection rules** (applied to `Vec<T>`, `Option<Vec<T>>`, `HashMap<K, V>`):

| Rule | Syntax | Condition |
| --- | --- | --- |
| MinItems | `min_items = N` | item count ≥ N |
| MaxItems | `max_items = N` | item count ≤ N |

`min_items`/`max_items` on a non-collection field is a compile-time error.

**Custom function**:
- `func = "path::to::fn"`: calls `fn(&T) -> Result<(), String>`. An `Err` message becomes the validation error.

**Cross-field rules** (reference another field by name or KDL key; both fields must be numeric):

| Rule | Syntax (aliases) | Condition |
| --- | --- | --- |
| LessThan | `less_than = "field"` (`lt`) | this < other |
| LessThanOrEqual | `less_than_or_equal = "field"` (`lte`) | this ≤ other |
| GreaterThan | `greater_than = "field"` (`gt`) | this > other |
| GreaterThanOrEqual | `greater_than_or_equal = "field"` (`gte`) | this ≥ other |
| EqualTo | `equal_to = "field"` (`eq`) | this = other |
| NotEqualTo | `not_equal_to = "field"` (`neq`) | this ≠ other |

Cross-field rules run after all fields are decoded. Referencing a nonexistent field is a compile-time error. If either field is `Option<T>` and is `None`, the cross-field check is skipped.

**Struct-level validation**:
- `#[kdl(validate(func = "path"))]` on a struct: calls `fn(&Self) -> Result<(), String>` after the struct is fully constructed. Useful for invariants that span multiple fields.
- Multiple struct-level `func` validators are allowed and run in order.

**`Option<T>` handling**: when the field value is `None`, all validation rules are skipped.

**Execution order**:
1. Per-field scalar/string rules run immediately after each field is decoded.
2. Per-field collection count rules run immediately after each field is decoded.
3. Per-field `func` rules run immediately after each field is decoded.
4. Cross-field rules run after all fields are decoded.
5. Struct-level `func` validators run last, after the struct is constructed.

**Runtime traits**: the `KdlValidate` trait bridges typed values to the `Validation` enum's `validate_number` / `validate_str` methods. The `KdlValidateCount` trait provides `.count()` for collection types. The `AsF64` trait converts numeric types for cross-field comparisons. These traits are implemented for all primitive numeric types, `String`, `bool`, and their `Option<T>` wrappers.

## Rust Traits

The runtime crate defines these core traits:

| Trait | Description |
| --- | --- |
| `KdlDecode` | Parse a typed configuration from a `KdlNode`. |
| `KdlRender` | Render a value into canonical KDL text. |
| `KdlUpdate` | Update an existing `KdlNode` AST in-place for round-trip preservation. |
| `KdlEncode` | Encode a value into a new `KdlNode`. |
| `KdlSchema` | Describe the KDL schema for a type. |
| `FromKdlValue` | Convert a KDL scalar value to a Rust type. |

Derive macros generate `KdlDecode`, `KdlRender`, `KdlUpdate`, and `KdlEncode` implementations. `KdlSchema` is generated separately.

## Built-in Newtypes

The runtime crate provides these reusable newtypes that implement `KdlDecode`, `KdlRender`, `KdlUpdate`, `FromKdlValue`, and `KdlSchema`:

| Type | Description |
| --- | --- |
| `Scalar<T>` | Generic wrapper for map value nodes and other node-valued scalar shapes. Implements all core traits by delegating to `T`. |
| `Duration` | Parses duration strings (`"500ms"`, `"30s"`, `"5m"`, `"2h"`, `"1d"`) or raw millisecond integers into `std::time::Duration`. |
| `Weight` | A float constrained to `0.0..=1.0`. |
| `PositiveCount` | A non-zero `u32` (`NonZeroU32`). |

All newtypes optionally derive `serde::Serialize` and `serde::Deserialize` behind the `serde` cargo feature.

## Notes

- This spec intentionally favors clarity over implicit behavior. Defaults aim to be sane but predictable.
- Strictness can be increased with `deny_unknown` and `default_conflict = "error"`.
