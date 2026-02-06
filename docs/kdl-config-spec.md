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
- `#[kdl(container = "custom-container")]`: override the container node name for `registry` fields (the repeated node name).
- `#[kdl(required)]` or `#[kdl(optional)]`: override required-ness.
- `#[kdl(default)]`, `#[kdl(default = "...")]`, `#[kdl(default_fn = "path")]`: default value.
- `#[kdl(bool = "presence+value" | "value-only" | "presence-only")]`: override bool behavior.
- `#[kdl(flag_style = "both" | "value|no" | "with|without")]`: override flag aliases.
- `#[kdl(conflict = "error" | "first" | "last" | "append")]`: override conflict policy for this field.
- `#[kdl(render = "attr" | "value" | "child" | "children" | "registry")]`: canonical render target.
- `#[kdl(skip)]`: skip both parsing and rendering; the field is set to `Default::default()` when parsing.
- `#[kdl(skip_serializing_if = "path")]`: rendering-only skip predicate.
- `#[kdl(scalar)]` or `#[kdl(value_type)]`: treat custom type as scalar for attr/positional/value placements (requires `FromKdlValue` implementation).
- `#[kdl(modifier)]`: capture the node's signal modifier (`+`, `-`, `!`) into a `Modifier` or `Option<Modifier>` field.

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

## Templates and Use

Templates use KDL’s native **type annotation** syntax:

```
(source)template "code" {
  chunking max-size=1500
  enrichment enabled
}
```

This means: a `template` node typed as `source` with a template name `"code"`.

**Template Rules**
- Templates must use a type annotation: `(type)template "name"`.
- Templates accept a **single string argument** (the template name).
- Templates do **not** accept attributes or modifiers.
- Templates may appear:
  - At the document top level.
  - As children of the single root node (when the document has exactly one non-template top-level node).
- Template definitions are removed before parsing and stored in a registry.

**Use Rules**
- A `use` node references a template by name: `use "code"`.
- `use` accepts a **single string argument**.
- `use` does **not** accept attributes, children, or modifiers.
- `use` must be a child of a parent node (never top-level).

**Expansion / Validation**
- Each `use` node is replaced **in place** with the template’s children.
- The template’s **type annotation** must match the parent context:
  - If the parent node has a type annotation, compare to that.
  - Otherwise compare to the parent node name.
- Errors are raised for unknown templates, duplicate template names, invalid forms, or type mismatches.
- Template recursion is rejected.

Example type mismatch:
```
(source)template "code" { ... }
vectors "default" {
  use "code" // ERROR: template type is "source", parent is "vectors"
}
```

## Signals / Modifiers

Node names may be prefixed with a modifier signal:
- `+name`: append semantics
- `-name`: remove semantics
- `!name`: replace semantics

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

### Template-Aware Updates

Round-trip parsing preserves `template` and `use` nodes. When rendering updates:
- The default update path may emit **explicit overrides** (expanded children) and keep templates unchanged.
- A **template-aware** update mode can be used to push changes back into template definitions when it is safe:
  - If **all uses** of a template result in identical updated children, the template definition is updated.
  - Otherwise the template remains unchanged and per-use overrides are emitted after the `use` node.

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
- The `template` node schema uses a required `type_annotation` and resolves its child schema using the referenced definition(s).

## Notes

- This spec intentionally favors clarity over implicit behavior. Defaults aim to be sane but predictable.
- Strictness can be increased with `deny_unknown` and `default_conflict = "error"`.
