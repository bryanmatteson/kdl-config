# Validation

kdl-config can validate field values at parse time. Add `#[kdl(validate(...))]` to any field and the generated `KdlDecode` implementation will check every rule after the value is decoded, returning a clear error if any constraint is violated.

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
server host="example.com" port=8080 workers=4   // ✓ ok
server host="" port=0 workers=-1                 // ✗ three errors
```

## Syntax

Rules are written inside `#[kdl(validate(...))]`. You can mix them freely with other kdl attributes:

```rust
#[kdl(attr, validate(min = 0, max = 100))]
score: i64,
```

Two syntaxes are supported and produce identical results:

| Form | Example |
| --- | --- |
| Nested meta | `#[kdl(validate(min = 1, max = 100))]` |
| DSL string | `#[kdl(validate = "min(1) max(100)")]` |

Multiple `validate` attributes on the same field are merged:

```rust
#[kdl(validate(min = 0))]
#[kdl(validate(max = 100))]
score: i64,
// equivalent to: validate(min = 0, max = 100)
```

## Rules Reference

### Numeric

These rules apply to integer and float types (`i32`, `u64`, `f64`, etc.).

| Rule | Example | Meaning |
| --- | --- | --- |
| `min` | `min = 1` | value ≥ 1 |
| `max` | `max = 100` | value ≤ 100 |
| `range` | `range(1, 100)` | 1 ≤ value ≤ 100 |
| `multiple_of` | `multiple_of = 5` | value is a multiple of 5 |
| `positive` | `positive` | value > 0 |
| `negative` | `negative` | value < 0 |
| `non_negative` | `non_negative` | value ≥ 0 |
| `non_positive` | `non_positive` | value ≤ 0 |

### String

These rules apply to `String` fields.

| Rule | Example | Meaning |
| --- | --- | --- |
| `non_empty` | `non_empty` | string is not `""` |
| `min_len` | `min_len = 3` | byte length ≥ 3 |
| `max_len` | `max_len = 255` | byte length ≤ 255 |
| `len` | `len(3, 255)` | 3 ≤ byte length ≤ 255 |
| `ascii` | `ascii` | all characters are ASCII |
| `alphanumeric` | `alphanumeric` | all characters are alphanumeric |
| `pattern` | `pattern = "^[a-z]+$"` | schema metadata only (no runtime regex) |

### Collection

These rules apply to `Vec<T>`, `Option<Vec<T>>`, and `HashMap<K, V>` fields.

| Rule | Example | Meaning |
| --- | --- | --- |
| `min_items` | `min_items = 1` | at least 1 element |
| `max_items` | `max_items = 10` | at most 10 elements |

Using `min_items` or `max_items` on a non-collection field is a **compile-time error**.

### Custom Function

Call any function with the signature `fn(&T) -> Result<(), String>`:

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

The function path can be a module-qualified path like `"crate::validators::check_port"`.

### Cross-Field

Compare two numeric fields. The referenced field is identified by its Rust name or KDL key.

| Rule | Aliases | Meaning |
| --- | --- | --- |
| `less_than = "other"` | `lt` | this < other |
| `less_than_or_equal = "other"` | `lte` | this ≤ other |
| `greater_than = "other"` | `gt` | this > other |
| `greater_than_or_equal = "other"` | `gte` | this ≥ other |
| `equal_to = "other"` | `eq` | this = other |
| `not_equal_to = "other"` | `neq` | this ≠ other |

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
range start=1.0 end=10.0   // ✓ ok
range start=10.0 end=10.0  // ✗ "must be less than 'end'"
range start=20.0 end=10.0  // ✗ "must be less than 'end'"
```

Referencing a field that doesn't exist is a **compile-time error**. Both fields must be numeric types. If either field is `Option<T>` and is `None`, the cross-field check is skipped.

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

This is the right place for invariants that involve multiple fields, conditional logic, or anything beyond what cross-field comparison rules can express. Multiple `func` validators on the same struct are allowed and run in declaration order.

## Optional Fields

When a field is `Option<T>` and the value is `None`, all validation rules are skipped:

```rust
#[derive(KdlNode)]
#[kdl(node = "cfg")]
struct Cfg {
    #[kdl(validate(min = 0, max = 100))]
    score: Option<i32>,
}
```

```kdl
cfg                 // ✓ score is None, validation skipped
cfg score=50        // ✓ Some(50), passes
cfg score=200       // ✗ Some(200), "exceeds maximum 100"
```

## Execution Order

Validation runs in a deterministic order during `KdlDecode`:

1. Each field is decoded, then its **scalar/string**, **count**, and **func** rules run immediately.
2. After all fields are decoded, **cross-field** rules run.
3. The struct is constructed, then **struct-level** `func` validators run.
4. If any rule fails, decoding short-circuits and returns a `KdlConfigError`.

## Error Messages

Validation errors use `ErrorKind::InvalidValue` and include the struct name, field name, KDL key, and a human-readable message from the rule:

```
error parsing ServerConfig field 'port': invalid value 'Min(1.0)': 0 is less than minimum 1
```

## Schema Integration

Validation rules are also written into the generated `KdlSchema` metadata. Tools that consume schemas can display constraints, generate documentation, or perform static analysis. The `pattern` rule is schema-only — it records the regex in the schema but does not perform runtime matching (avoiding a `regex` crate dependency).
