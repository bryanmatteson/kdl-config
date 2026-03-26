# Validation

kdl-config validates field values at parse time. Add `#[kdl(validate(...))]` to any field and the generated `KdlDecode` implementation checks every rule after the value is decoded, collecting all constraint violations and returning them together.

**Key behavior:** Validation **collects errors**. All field-level, count, func, and cross-field validation failures across all fields are accumulated into a single `KdlConfigError`. If multiple fields fail, you see every violation — not just the first. Structural errors (missing required fields, type mismatches) still short-circuit immediately since the field didn't decode at all.

## Quick Start

```rust
#[derive(KdlNode)]
#[kdl(node = "server")]
struct ServerConfig {
    #[kdl(validate(non_empty, max_len = 255))]
    host: String,

    #[kdl(validate(min = 1, max = 65535))]
    port: u32,

    #[kdl(validate(positive))]
    workers: i32,
}
```

```kdl
server host="example.com" port=8080 workers=4   // ok
server host="" port=0 workers=-1                 // 3 validation errors reported together
```

## Syntax

Rules go inside `#[kdl(validate(...))]`. They can be mixed freely with other `#[kdl(...)]` attributes on the same field:

```rust
#[kdl(attr, validate(min = 0, max = 100))]
score: i64,
```

Two equivalent syntaxes:

| Form | Example |
| --- | --- |
| Nested meta | `#[kdl(validate(min = 1, max = 100))]` |
| DSL string | `#[kdl(validate = "min(1) max(100)")]` |

The nested meta form uses `key = value` for single-argument rules and `key(a, b)` for multi-argument rules. The DSL string form always uses `key(value)` syntax, space-separated.

Multiple `validate` attributes on the same field **merge** (rules accumulate):

```rust
#[kdl(validate(min = 0))]
#[kdl(validate(max = 100))]
score: i64,
// equivalent to: validate(min = 0, max = 100)
```

Similarly, `validate` inside `schema(validate(...))` merges with top-level `validate(...)` on the same field.

## Rules Reference

### Numeric

Apply to integer and float types: `i8` through `i128`, `u8` through `u128`, `f32`, `f64`, `isize`, `usize`. All comparisons operate on `f64` internally. NaN values are rejected with a clear error.

| Rule | Example | Meaning | Boundary |
| --- | --- | --- | --- |
| `min` | `min = 1` | value >= 1 | inclusive |
| `max` | `max = 100` | value <= 100 | inclusive |
| `range` | `range(1, 100)` | 1 <= value <= 100 | both inclusive |
| `multiple_of` | `multiple_of = 5` | value is a multiple of 5 | uses relative tolerance for large values |
| `positive` | `positive` | value > 0 | **excludes zero** |
| `negative` | `negative` | value < 0 | **excludes zero** |
| `non_negative` | `non_negative` | value >= 0 | includes zero |
| `non_positive` | `non_positive` | value <= 0 | includes zero |

> **Agent note — `positive` vs `non_negative`:** `positive` rejects zero. If a field should accept zero, use `non_negative` or `min = 0`, not `positive`.

> **`multiple_of(0)` is an error.** Specifying a divisor of zero produces a runtime validation error (`"multiple_of divisor must not be zero"`).

> **`bool` fields:** Validation rules on `bool` fields are silently ignored (no-op). Do not add numeric or string rules to bools.

> **`PathBuf` fields:** Validated as a string via `to_string_lossy()`. String rules (`non_empty`, `max_len`, `ascii`, etc.) work on PathBuf.

### String

Apply to `String` and `&str` fields (and `PathBuf` — see above).

| Rule | Example | Meaning |
| --- | --- | --- |
| `non_empty` | `non_empty` | string is not `""` |
| `min_len` | `min_len = 3` | byte length >= 3 |
| `max_len` | `max_len = 255` | byte length <= 255 |
| `len` | `len(3, 255)` | 3 <= byte length <= 255 |
| `min_chars` | `min_chars = 3` | character count >= 3 |
| `max_chars` | `max_chars = 255` | character count <= 255 |
| `chars` | `chars(3, 255)` | 3 <= character count <= 255 |
| `ascii` | `ascii` | all characters are ASCII |
| `alphanumeric` | `alphanumeric` | all characters are alphanumeric |
| `pattern` | `pattern = "^[a-z]+$"` | regex match (requires `regex` feature) |

> **Byte length vs character count:** `min_len` / `max_len` / `len` measure `.len()` (byte length). `min_chars` / `max_chars` / `chars` measure `.chars().count()` (Unicode scalar count). For ASCII-only strings there is no difference. For multi-byte UTF-8 strings like `"héllo"` (5 chars, 6 bytes), the results diverge. Use `min_chars` / `max_chars` / `chars` when the constraint is about visible characters. Use `min_len` / `max_len` / `len` when the constraint is about storage size or wire format.

> **`pattern` runtime behavior depends on features.** With the `regex` feature enabled (default), `pattern` performs runtime regex matching against the field value. Without the feature, `pattern` is schema metadata only. The regex is always recorded in the generated `KdlSchema` regardless of feature flags.

### Collection

Apply to `Vec<T>`, `Option<Vec<T>>`, and `HashMap<K, V>` fields.

| Rule | Example | Meaning |
| --- | --- | --- |
| `min_items` | `min_items = 1` | at least 1 element |
| `max_items` | `max_items = 10` | at most 10 elements |

Using `min_items` or `max_items` on a non-collection field is a **compile-time error**.

> **`BTreeMap` is not supported** by count validation. Only `Vec<T>`, `Option<Vec<T>>`, and `HashMap<K, V>` implement `KdlValidateCount`. If you need count validation on a `BTreeMap`, use a `func` validator.

> **`Option<Vec<T>>` counts as 0 when `None`.** This means `min_items = 1` will reject `None` — use this deliberately or add `default` to ensure the field is always present.

### Custom Function

Call any function with signature `fn(&T) -> Result<(), String>`:

```rust
fn validate_even(val: &i32) -> Result<(), String> {
    if val % 2 != 0 {
        Err(format!("{} is not even", val))
    } else {
        Ok(())
    }
}

#[derive(KdlNode)]
#[kdl(node = "cfg")]
struct Cfg {
    #[kdl(validate(func = "validate_even"))]
    count: i32,
}
```

Two equivalent syntax forms:

```rust
#[kdl(validate(func = "validate_even"))]   // key = value form
#[kdl(validate(func("validate_even")))]     // call-style form
```

The function path can be a module-qualified path like `"crate::validators::check_port"`. Invalid function paths are caught at compile time with a clear `compile_error!` message. The function receives a reference to the decoded field value after any `from`/`try_from` conversion has been applied.

### Cross-Field

Cross-field rules reference another field by its **Rust field name** (not the KDL key name).

Numeric comparison rules:

| Rule | Aliases | Meaning |
| --- | --- | --- |
| `less_than = "other"` | `lt` | this < other |
| `less_than_or_equal = "other"` | `lte` | this <= other |
| `greater_than = "other"` | `gt` | this > other |
| `greater_than_or_equal = "other"` | `gte` | this >= other |
| `equal_to = "other"` | `eq` | this = other |
| `not_equal_to = "other"` | `neq` | this != other |

Membership rules:

| Rule | Meaning |
| --- | --- |
| `exists_in = "other"` | this scalar value must exist in the other collection |
| `subset_of = "other"` | this collection must be a subset of the other collection |

Supported runtime collection shapes:
- `exists_in`: `Vec<T>`, `HashSet<T>`, `HashMap<K, V>` (checks keys)
- `subset_of`: `Vec<T>` subset of `Vec<T>`, `HashSet<T>` subset of `HashSet<T>`, `Vec<K>` subset of `HashMap<K, V>`, `HashMap<K, _>` subset of `HashMap<K, _>` (by keys)

```rust
#[derive(KdlNode)]
#[kdl(node = "range")]
struct Range {
    #[kdl(validate(less_than = "end"))]
    start: f64,
    end: f64,
}
```

```kdl
range start=1.0 end=10.0   // ok
range start=10.0 end=10.0  // error: "10 must be less than 'end' (10)"
range start=20.0 end=10.0  // error: "20 must be less than 'end' (10)"
```

Membership examples:

```rust
#[derive(KdlNode)]
#[kdl(node = "membership")]
struct Membership {
    #[kdl(attr, validate(exists_in = "allowed"))]
    item: String,
    #[kdl(attr, value, conflict = "append")]
    allowed: Vec<String>,
}

#[derive(KdlNode)]
#[kdl(node = "subset")]
struct SubsetConfig {
    #[kdl(attr, value, conflict = "append", validate(subset_of = "allowed"))]
    selected: Vec<String>,
    #[kdl(attr, value, conflict = "append")]
    allowed: Vec<String>,
}
```

```kdl
membership item="beta" allowed="alpha" allowed="beta"  // ok: item exists in allowed
membership item="gamma" allowed="alpha" allowed="beta" // error: "value must exist in 'allowed'"
```

**Compile-time safety:** Referencing a field that doesn't exist is a compile-time error (the generated code references the field variable directly). Numeric comparison rules require both fields to be numeric types that implement `AsF64`.

> **Agent note — floating-point tolerance:** `equal_to` and `not_equal_to` use a relative tolerance: `2 × f64::EPSILON × max(|a|, |b|, 1.0)`. This means two values that differ by less than about 2 ULP are considered equal. For typical config values (small integers, reasonable floats) this is invisible. For extremely large magnitudes the tolerance scales proportionally. `less_than`, `greater_than`, and their `_or_equal` variants use exact comparison with no tolerance.

**Option semantics for cross-field rules:** If either side of a cross-field rule is `Option<T>` and is `None`, the rule is **skipped entirely**. All four combinations are handled:
- Both `None` -> skipped
- This `None`, other `Some` -> skipped
- This `Some`, other `None` -> skipped
- Both `Some` -> rule runs normally

## Struct-Level Validation

Use `#[kdl(validate(func = "path"))]` on a struct to validate the fully constructed value. The function receives `&Self`:

```rust
fn check_invariants(cfg: &AppConfig) -> Result<(), String> {
    if cfg.min_workers > cfg.max_workers {
        return Err(format!(
            "min_workers ({}) must not exceed max_workers ({})",
            cfg.min_workers, cfg.max_workers
        ));
    }
    Ok(())
}

#[derive(KdlNode)]
#[kdl(node = "app", validate(func = "check_invariants"))]
struct AppConfig {
    min_workers: i64,
    max_workers: i64,
    name: String,
}
```

Both syntax forms work:

```rust
#[kdl(validate(func = "check_invariants"))]      // key = value
#[kdl(validate(func("check_invariants")))]        // call-style
```

This is the right place for invariants that involve multiple fields, conditional logic, or anything beyond what cross-field comparison rules can express. Multiple `func` validators on the same struct are allowed and run in declaration order.

## Post-Decode Hooks

Use `#[kdl(post_decode = "path")]` or `#[kdl(post_decode(func = "path"))]` to mutate or reject the decoded struct **before** struct-level `validate(func = ...)` runs.

Hook signature: `fn(&mut Self) -> Result<(), String>`

```rust
fn normalize_name(cfg: &mut AppConfig) -> Result<(), String> {
    cfg.name = cfg.name.trim().to_string();
    if cfg.name.is_empty() {
        return Err("name must not be empty".to_string());
    }
    Ok(())
}

#[derive(KdlNode)]
#[kdl(node = "app", post_decode(func = "normalize_name"))]
struct AppConfig {
    #[kdl(attr)]
    name: String,
}
```

Use `post_decode` when you need to normalize, transform, or canonicalize values before validation runs. It is the only hook that receives `&mut Self`.

## Optional Fields

When a field is `Option<T>` and the value is `None`, **all** field-level validation rules are skipped (scalar, string, count, and func):

```rust
#[derive(KdlNode)]
#[kdl(node = "cfg")]
struct Cfg {
    #[kdl(validate(min = 0, max = 100))]
    score: Option<i32>,
}
```

```kdl
cfg                 // ok: score is None, validation skipped
cfg score=50        // ok: Some(50), passes
cfg score=200       // error: Some(200), "exceeds maximum 100"
```

## Execution Order

Validation runs in a strict, deterministic order during `KdlDecode`:

1. **Field decode + inline validation** — each field is decoded in struct definition order. Immediately after decoding, its **scalar/string rules**, **count rules**, and **func rules** run. Failures are collected, not short-circuited.
2. **Cross-field validation** — after all fields are decoded (but **before** the struct is constructed), cross-field comparison rules run. Failures are added to the same collection.
3. **Error flush** — if any validation errors were collected in steps 1-2, they are returned together as a single `KdlConfigError` (with `ErrorKind::Multiple` if more than one). Decoding stops here.
4. **Struct construction** — `Self { field1, field2, ... }` is built.
5. **Post-decode hook** — `post_decode` runs with `&mut Self`. Can mutate fields and reject. If it returns `Err`, decoding stops.
6. **Struct-level validation** — `validate(func = ...)` runs with `&Self`, in declaration order. If any fail, decoding stops.

> **Agent note:** Steps 1-2 collect errors; steps 5-6 still short-circuit because they require the struct to be fully constructed and potentially mutated.

## Error Messages

Validation errors produce `KdlConfigError` with `ErrorKind::InvalidValue` or `ErrorKind::Multiple`. The full `Display` format for a single error is:

```
error parsing {StructName} field '{field_name}' at line {L}, column {C}: invalid value '{rule}': {message}
```

- Location (`at line L, column C`) appears when the parser has source position information (always present for `parse_str` and file-based loading; absent for programmatic construction without source context).
- `rule` is the human-readable validation rule (e.g., `min(5)`, `positive`, `non_empty`).
- `message` is the failure reason (e.g., `"0 is less than minimum 1"`, `"value must not be empty"`).

For multiple errors, the outer wrapper shows the count and each sub-error is numbered on its own line. Each sub-error carries the precise source location of the field's KDL entry:

```
error parsing ServerConfig at line 1, column 1: 3 validation errors:
  [1] error parsing ServerConfig field 'host' at line 1, column 8: invalid value 'non_empty': value must not be empty
  [2] error parsing ServerConfig field 'port' at line 1, column 17: invalid value 'min(1)': 0 is less than minimum 1
  [3] error parsing ServerConfig field 'workers' at line 1, column 24: invalid value 'positive': -1 is not positive
```

> **Agent note — per-field location tracking:** For scalar fields (attributes, positional args, value children), each sub-error carries the precise location of the KDL entry that produced the value — not the node-level offset. This means `field 'a' at line 1, column 7` and `field 'b' at line 1, column 12` point to their actual positions in the source. For non-scalar fields (child structs, collections), the sub-error falls back to the containing node's offset.

## Schema Integration

Validation rules are written into the generated `KdlSchema` metadata. Tools that consume schemas can display constraints, generate documentation, or perform static analysis.

The `pattern` rule is always recorded in the schema. Runtime matching depends on the `regex` feature (enabled by default).

## Choosing the Right Validation Layer

Use this decision tree when adding validation to a config type:

| Need | Use | Why |
| --- | --- | --- |
| Simple range, length, or sign check on one field | `validate(min = ..., max_len = ...)` | Declarative, zero boilerplate, shows up in schema |
| One field must relate to another numerically | `validate(less_than = "other")` | Expresses the constraint directly on the field that must satisfy it |
| A value must be a member of a collection field | `validate(exists_in = "other")` | Clearer than a custom func for membership checks |
| Complex multi-field invariant | `validate(func = "check_fn")` on the **struct** | Has access to all fields via `&Self`; runs after construction |
| Need to normalize/trim/canonicalize before checking | `post_decode(func = "normalize_fn")` | Runs before struct-level validation; gets `&mut Self` |
| Need a regex match | `validate(pattern = "^[a-z]+$")` | Runtime check with `regex` feature; always in schema |
| Need an external lookup or expensive check | `validate(func = "custom_fn")` on the **field** | Full flexibility |

### Post-decode vs validate

Use `post_decode` when you want to normalize or mutate after parse.
Use `validate` when you want to reject invalid final state.

## Common Mistakes

1. **Using `positive` when you mean `non_negative`** — `positive` rejects zero. For a port number that can be zero, use `min = 0`.

2. **Putting `min_items` on a scalar** — `min_items` and `max_items` only work on `Vec<T>`, `Option<Vec<T>>`, and `HashMap<K, V>`. Any other field type is a compile-time error.

3. **Referencing KDL key names in cross-field rules** — Cross-field rules reference the **Rust field name**, not the renamed KDL key. If a field is `my_field` renamed to `my-field`, use `less_than = "my_field"`.

4. **Forgetting `Option` semantics** — All validation is skipped when an `Option<T>` field is `None`. If you need to require a field's presence, mark it `required` separately.

5. **Adding validation rules to `bool` fields** — Compiles fine but does nothing. Bool validation is a no-op.

6. **Assuming byte length = character count** — `min_len`, `max_len`, and `len` use `.len()` (byte length). For multi-byte UTF-8 strings, byte length exceeds character count. Use `min_chars`, `max_chars`, or `chars` when the constraint is about visible characters.

7. **Using `multiple_of = 0`** — This is a runtime error, not silently ignored. Always use a positive divisor.
