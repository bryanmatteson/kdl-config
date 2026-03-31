# kdl-config Spec

> Concise behavioral specification for KDL ↔ Rust mapping via `#[derive(Kdl*)]` macros.

---

## 1. Derives

| Derive | Input | Traits generated |
|---|---|---|
| `KdlNode` | struct, tagged enum | `KdlDecode`, `KdlRender`, `KdlUpdate`, `KdlSchema` |
| `KdlValue` | unit enum, newtype struct | `FromKdlValue`, `KdlSchema` |
| `KdlChoice` | enum (variant = node name) | `KdlDecode`, `KdlRender`, `KdlUpdate`, `KdlSchema` |
| `Kdl` | any | Auto-selects: unit enum→Value, newtype enum→Choice, struct→Node. Override with `#[kdl(node)]` / `#[kdl(value)]` / `#[kdl(choice)]` / `#[kdl(schema)]`. |
| `KdlSchema` | any | `KdlSchema` only |
| `KdlMerge` | struct | `DeepMerge` |
| `KdlPartial` | struct (all `Option` fields) | `PartialConfig<T>` (requires `#[kdl(partial_for = "T")]`) |

---

## 2. Struct-Level Attributes

All inside `#[kdl(...)]` on the struct.

| Attribute | Syntax | Values | Default |
|---|---|---|---|
| `node` | `node = "name"` or bare `node` | string | struct name |
| `rename_all` | `= "strategy"` | `kebab-case` `snake_case` `lowercase` `UPPERCASE` `none` | `none` |
| `default_placement` | `= "mode"` | `exhaustive` `attr` `value` `child` | `exhaustive` |
| `default_bool` | `= "mode"` | `presence+value` `value-only` `presence-only` | `presence+value` |
| `default_flag_style` | `= "style"` | `both` `value\|no` `with\|without` | `both` |
| `default_conflict` | `= "policy"` | `error` `first` `last` `append` | `error` |
| `skip_serialize_none` | bare or `= bool` | — | off |
| `skip_serialize_empty_collections` | bare or `= bool` | — | off |
| `deny_unknown` | bare | — | off |
| `validate` | `validate(func = "path")` or `validate(func("path"))` | fn path | — |
| `post_decode` | `= "path"` or `post_decode(func = "path")` | fn path | — |
| `schema(...)` | nested: `name`, `description`/`desc`, `deny_unknown`, `allow_unknown` | — | — |

**Tagged enum only:**

| Attribute | Syntax | Purpose |
|---|---|---|
| `selector` | `= arg(N)` / `= attr("key")` / `= name` / `= func("path")` / `= any(...)` | Discriminator source |
| `consume` | bare | Remove discriminator before parsing variant |
| `preserve` | bare (default) | Keep discriminator |

**Serde fallback:** `#[serde(rename_all)]` and `#[serde(rename)]` are respected when no `#[kdl]` equivalent exists.

---

## 3. Field-Level Attributes

### 3.1 Placement (mutually exclusive categories)

**Scalar placements** (for String, numbers, bool, PathBuf, and `FromKdlValue` types):

| Attribute | KDL shape | Notes |
|---|---|---|
| `attr` | `node key="value"` | Keyed attribute |
| `keyed` | `node key="value"` | Alias for `attr` |
| `positional = N` | `node val0 val1 …` | 0-indexed positional arg |
| `positional = "rest"` | `node a b c …` | Collects all positional args into `Vec`. `"*"` is alias. |
| `flag` | `node enabled` / `node no-enabled` | Bool only. Bare = default names. |
| `flag = "tok"` | `node tok` | Custom positive token. **Requires** `neg_flag`. |
| `neg_flag = "tok"` | `node tok` | Custom negative token. **Requires** `flag`. |
| `value` | `node { field_name val }` | Value child node |

**Node placements** (for struct-typed fields):

| Attribute | KDL shape | Notes |
|---|---|---|
| `child` | `node { child_name … }` | Single child node |
| `children` | `node { name …\n name … }` | `Vec<T>` of same-name children. Use `name = "…"` for the child node name. |
| `children_any` / `choice` | `node { any_name … }` | Children of any name (choice enum) |
| `registry` | `node { container "key" … }` | `HashMap<String, T>` or `Vec<(String, T)>`. **Requires** `container = "node_name"`. Default key: first positional arg. |
| `children_map` | (see §3.5) | Mapped children collection |
| `flatten` | (fields inline on parent) | Inlines nested struct's fields into parent node |
| `modifier` | `+node` / `-node` / `!node` / `~node` | Captures merge modifier prefix. Type: `Modifier` or `Option<Modifier>`. At most one per struct. |

**No explicit placement → `default_placement` applies.** With `exhaustive` (default): tries attr → positional → value → child, accepts first match.

### 3.2 Naming

| Attribute | Syntax | Purpose |
|---|---|---|
| `name` | `= "kdl-name"` or `= any("a", "b", …)` | Override KDL key. `any(…)` accepts multiple names. |
| `rename` | same as `name` | Alias for `name` |
| `alias` | `= "alt"` or `= any("a", "b")` | Additional accepted names (parse only, not render). Repeatable. |
| `aliases` | same as `alias` | Alias for `alias` |

Name resolution: `rename_all` applies to the Rust field name first, then `name`/`rename` overrides the result.

### 3.3 Defaults

| Attribute | Syntax | Behavior |
|---|---|---|
| `default` | bare | `Default::default()` |
| `default` | `= lit` | Literal: int, negative int, float, bool, string |
| `default_fn` | `= "path"` | Call `fn() -> T` |

Only one of the three. Missing bools default to `false`; missing `Option<T>` to `None`; missing `Vec<T>` to `[]`.

### 3.4 Mode Overrides (per-field)

| Attribute | Values |
|---|---|
| `bool` | `presence+value` `value-only` `presence-only` |
| `flag_style` | `both` `value\|no` `with\|without` |
| `conflict` | `error` `first` `last` `append` |
| `render` | `attr` `value` `child` `children` `registry` |

### 3.5 Children Map

`children_map` collects children into `HashMap<K, V>` or `Vec<(K, V)>`.

| Combined with | Key source | KDL shape |
|---|---|---|
| `map_node = "n"` (no selector) | First positional arg of `n` | `n "key" { … }` |
| (neither) | Child node name | `key_name { … }` |
| `map_node = "n"`, `select(attr("k"))` | Attribute `k` of `n` | `n k="key" { … }` |
| `map_node = "n"`, `select(func("f"))` | Function `f(&KdlNode) -> Option<String>` | (custom) |

### 3.6 Selectors

Used with `registry`, `children_map`, and tagged enum discriminators.

| Selector | Syntax | Extracts from |
|---|---|---|
| `arg(N)` | `select(arg(N))` | Positional argument N |
| `attr("k")` | `select(attr("k"))` | Attribute value |
| `name` | `select(name)` | Node name |
| `func("path")` | `select(func("path"))` | Custom fn: `fn(&KdlNode) -> Option<String>` |
| `any(…)` | `select(any(arg(0), attr("k")))` | First selector that matches |

**Options** (inside `select(…)` or standalone):

| Option | Effect |
|---|---|
| `consume` | Remove the token used for selection before parsing the child |
| `preserve` | Keep it (default) |
| `inject` | Inject selected value into a field (implicit = `attr("name")` only) |
| `inject = "field"` | Inject into named field |

### 3.7 Tagged Child Fields

`child` fields whose type is a tagged enum may opt into parent-driven variant selection and
explicit parent-attr hoisting.

| Attribute | Syntax | Purpose |
|---|---|---|
| `tag_attr` | `= "key"` | Select enum variant from a keyed attribute on the parent node |
| `hoist_attrs` | `= any("a", "b", …)` | Forward selected keyed attrs from parent into the effective child node |

Example:

```rust
#[derive(KdlNode)]
#[kdl(node = "semantic")]
struct SemanticConfig {
    #[kdl(child, tag_attr = "provider", hoist_attrs = any("model"))]
    provider: SemanticProvider,

    #[kdl(child, tag_attr = "backend")]
    backend: VectorBackend,
}
```

Canonical KDL:

```kdl
semantic provider=ollama backend=qdrant model="nomic-embed-code-safe" {
    provider endpoint="http://localhost:11434"
    backend endpoint="http://localhost:6333"
}
```

Decode semantics:

1. Read `tag_attr` from the parent, or use the field default when omitted.
2. Create an effective child node for the field from the authored child node if present, else an empty node with the field's child name.
3. Forward any declared `hoist_attrs` from the parent into the effective child node.
4. Reject duplicates when a hoisted attr is already present on the authored child node.
5. Decode the enum variant selected by `tag_attr` from the effective child node.

Rules:

- `tag_attr` is only valid on `child` fields whose type is a tagged enum (`#[derive(KdlNode)]` on enum with selector-capable tags).
- `hoist_attrs` requires `tag_attr` and only forwards keyed scalar attrs in the initial design.
- Hoisted attrs are considered consumed on the parent for unknown-field checking.
- A hoisted attr must be claimed by at most one tagged child field on the same parent struct.
- If no `tag_attr` is present and the field has no default, the field is missing.
- If the selected variant cannot decode a hoisted attr, parsing fails normally for that variant.
- Child-local discriminators are rejected when `tag_attr` is used; canonical rendering and parsing put variant selection on the parent.

Render semantics:

- Render the selected variant discriminator on the parent as `tag_attr=value`.
- Render hoisted attrs on the parent, not inside the child node.
- Render the child node only for non-hoisted nested config.
- Omit the child node when it would be empty after removing hoisted attrs.

Required errors:

- unknown `tag_attr` value
- missing `tag_attr` when no default variant exists
- duplicate attr between parent hoist and authored child node
- duplicate claim of the same hoisted attr by multiple tagged child fields

### 3.8 Serialization Control

| Attribute | Effect |
|---|---|
| `skip` | Ignore entirely (parse + render). Field must be defaultable. |
| `skip_serializing_if = "path"` | Skip render if `fn(&T) -> bool` returns true |
| `no_skip_serialize` | Override struct-level `skip_serialize_none` / `skip_serialize_empty_collections` |
| `required` | Field must be present (error if missing) |
| `optional` | Field may be absent (explicit; default for `Option`, `Vec`) |

### 3.9 Type Conversion

| Attribute | Syntax | Trait bound (parse) | Trait bound (render) |
|---|---|---|---|
| `from` | `= "Type"` | `FieldType: From<Type>`, `Type: FromKdlValue` | `FieldType: Into<Type>` |
| `try_from` | `= "Type"` | `FieldType: TryFrom<Type, Error=String>`, `Type: FromKdlValue` | `FieldType: Into<Type>` |

Both work with `Option<T>` and `Vec<T>` (conversion applies per element). Cannot use both simultaneously. Implies `scalar`.

### 3.10 Scalar Hints

`scalar` / `value_type` / `value_like` / `kdl_value` — all equivalent. Mark field as scalar (`FromKdlValue`) when the derive can't infer it from the type. Implied by `from`/`try_from`.

### 3.11 Path Re-rooting

| Syntax | Behavior |
|---|---|
| `path = "a.b"` | Parse from `{ a { b <here> } }` relative to current node |
| `path = "/a.b"` | Parse from `{ a { b <here> } }` relative to document root |

---

## 4. Validation

### 4.1 Field-Level

`#[kdl(validate(rule, rule, …))]`

**Numeric:**
`min(N)` `max(N)` `range(A,B)` `multiple_of(N)` `positive` `negative` `non_negative` `non_positive`

**String (byte length):**
`non_empty` `min_len(N)` `max_len(N)` `len(A,B)` `pattern("regex")` `ascii` `alphanumeric`

**String (character count):**
`min_chars(N)` `max_chars(N)` `chars(A,B)`

**Collection:**
`min_items(N)` `max_items(N)`

**Cross-field** (reference other field by Rust name):
`less_than("f")` / `lt("f")` · `lte("f")` · `greater_than("f")` / `gt("f")` · `gte("f")` · `equal_to("f")` / `eq("f")` · `not_equal_to("f")` / `neq("f")` · `exists_in("f")` · `subset_of("f")`

**Custom:**
`func = "path"` or `func("path")` — signature: `fn(&T) -> Result<(), String>`

### 4.2 Struct-Level

`#[kdl(validate(func = "path"))]` or `#[kdl(validate(func("path")))]`

Signature: `fn(&Self) -> Result<(), String>`. Runs after all field validations.

### 4.3 Option Semantics

`Option<T>` fields skip all validation when `None`. Cross-field validation skips when either side is `None`.

---

## 5. Enum Variants

### 5.1 Tagged Enum Variants (KdlNode on enum)

| Attribute | Syntax | Purpose |
|---|---|---|
| `name` / `rename` | `= "variant_name"` | Override variant name for selector matching |

### 5.2 Value Enum Variants (KdlValue)

| Attribute | Syntax | Purpose |
|---|---|---|
| `tag` | `= "string"` / `= 42` / `= 1.5` / `= #true` | KDL value this variant maps to |

### 5.3 Choice Enum Variants (KdlChoice)

| Attribute | Syntax | Purpose |
|---|---|---|
| `name` / `rename` | `= "node_name"` | Override the node name for this variant |

---

## 6. Schema Overrides

**Struct-level** `schema(…)`: `name = "…"` · `description = "…"` / `desc = "…"` · `deny_unknown` · `allow_unknown`

**Field-level** `schema(…)`: `skip` · `name = "…"` / `rename = "…"` · `kind = "type"` · `required` / `optional` · `description = "…"` / `desc = "…"`

`kind` values: `string` · `integer`/`int` · `float`/`number` · `boolean`/`bool` · `null`

---

## 7. Merge

### 7.1 KdlMerge

`#[derive(KdlMerge)]` generates `DeepMerge`. Per-field policies:

| Policy | Syntax | Behavior |
|---|---|---|
| `deep` (default) | — | Recursive merge; `other` wins for scalars, `Some` wins for options |
| `keep` | `merge = "keep"` | Preserve `self`, ignore `other` |
| `replace` | `merge = "replace"` | `other` always replaces `self` |
| `append` | `merge = "append"` | Concatenate collections |
| custom | `merge(func = "path")` | `fn(Self, Other) -> Merged` |

### 7.2 KdlPartial

`#[derive(KdlPartial)]` with `#[kdl(partial_for = "BaseType")]`. All fields must be `Option<T>`. `apply_to(base)`: `Some` replaces, `None` preserves.

---

## 8. Built-in Types

| Type | KDL value | Constraint |
|---|---|---|
| `Duration` | `"500ms"` `"30s"` `"5m"` `"2h"` `"1d"` or raw ms integer | > 0 |
| `Weight` | float | `[0.0, 1.0]` |
| `PositiveCount` | integer | `> 0` (NonZeroU32) |
| `Scalar<T>` | positional value | Generic scalar wrapper for map values |

---

## 9. Fragments

| Directive | Syntax | Effect |
|---|---|---|
| `fragment` | `fragment "key" <type> { … }` | Register reusable template |
| `with` | `with "key"` | Expand template into parent's children |
| `from` | `from "key" <type> …` | Materialize new node from template |

Fragments expand **before** typed parsing. Override values by placing children after `with`.

---

## 10. Layered Loading

`KdlLoader::new().add_layer("a.kdl").add_layer("b.kdl").load::<T>()`

Later layers override earlier via deep merge. Child-node merge modifiers:

| Prefix | Effect |
|---|---|
| `+` | Append new sibling |
| `-` | Remove all with this name |
| `!` | Replace all with this name |
| `~` | Flatten (merge by name into existing) |

---

## 11. Conflict Resolution Order

When a field accepts from multiple placements (`attr, value, conflict = "…"`):

- **first**: attr → positional → value → child (first found wins)
- **last**: child → value → positional → attr (last found wins)
- **error**: reject if found in more than one placement
- **append**: concatenate all found values (Vec only)

Duplicate attributes (`key=1 key=2`) also follow the field's conflict policy.

---

## 12. Bool Behavior Matrix

| Mode | `enabled` | `no-enabled` | `enabled=#true` | `enabled=#false` | missing |
|---|---|---|---|---|---|
| `presence+value` | true | false | true | false | false |
| `value-only` | **error** | **error** | true | false | false |
| `presence-only` | true | **error** | **error** | **error** | false |

**Flag style** controls rendering:

| Style | true renders as | false renders as |
|---|---|---|
| `both` | `enabled` | `no-enabled` |
| `value\|no` | `enabled=#true` | `enabled=#false` |
| `with\|without` | `with-enabled` | `without-enabled` |

Custom flags (`flag = "on", neg_flag = "off"`) replace the generated names entirely.

---

## 13. Render Order

Within a node, fields render in this order:

1. Positional arguments (by index)
2. Keyed attributes (by struct field definition order)
3. Flags
4. Value child nodes (by field definition order)
5. Child nodes (by field definition order)

Modifiers prefix the node name: `+node`, `-node`, `!node`.

---

## 14. Implicit Defaults

| Type | Missing field yields | `required` overrides to error |
|---|---|---|
| `bool` | `false` | yes |
| `Option<T>` | `None` | yes |
| `Vec<T>` | `[]` | yes |
| scalar without `default` | **error** | — |
| struct without `default` | **error** | — |

---

## 15. Type → KDL Value Mapping

| Rust type | KDL value type |
|---|---|
| `String` | string |
| `i8`–`i128`, `u8`–`u64`, `usize` | integer |
| `f32`, `f64` | float |
| `bool` | boolean |
| `PathBuf` | string |
| `Vec<T>` | repeated values/nodes |
| `Option<T>` | present or absent |
| `HashMap<K, V>` | registry/children_map nodes |

---

## 16. Compile-Time Rejections

- `flag` on non-`bool` field
- `children` on non-`Vec` field
- `flag = "…"` without `neg_flag`
- `neg_flag` without `flag`
- `conflict = "append"` on scalar field
- Multiple defaults (`default` + `default_fn`)
- `from` + `try_from` simultaneously
- `select(…)` + `selector = …` simultaneously
- Invalid enum string for any mode attribute
- `default = <non-literal>` (only int, float, bool, string accepted)
- `positional = "bad"` (only integer or `"rest"`/`"*"`)
