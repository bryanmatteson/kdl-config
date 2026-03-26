# kdl-config

Strongly-typed configuration from KDL documents.

`kdl-config` allows you to define your configuration structure using Rust structs and automatically parse KDL documents into those structs. It supports attributes, arguments, children, and more, with highly customizable mappings. Built on [KDL 6.5.0](https://kdl.dev/).

## Installation

Add the following to your `Cargo.toml`:

```toml
[dependencies]
kdl-config = "0.1.0"
```

Optional features:

- `regex` (default) — enables runtime `pattern` validation via the `regex` crate.
- `serde` — enables `Serialize` and `Deserialize` derives on built-in newtypes (`Duration`, `Weight`, `PositiveCount`, `Scalar`).

## Quick Start

```rust
use kdl_config::{KdlNode, parse_str};

#[derive(Debug, KdlNode)]
#[kdl(node = "config")]
struct MyConfig {
    #[kdl(attr)]
    name: String,
    #[kdl(attr)]
    count: i32,
}

fn main() {
    let config: MyConfig = parse_str(r#"config name="demo" count=10"#).unwrap();
    println!("{:?}", config);
}
```

## Parse Config and Root Wrapping

`parse_str_with_config` accepts a `ParseConfig` with `root_mode`:

- `RootMode::Strict` (default): input must contain exactly one top-level node.
- `RootMode::WrapExpectedNode { name }`: if parsing fails due to multiple top-level nodes or root name mismatch, input is retried as `name { ... }`.

```rust
use kdl_config::{ParseConfig, RootMode, parse_str_with_config};

#[derive(kdl_config::KdlNode)]
#[kdl(node = "config")]
struct EmbeddedConfig {
    name: String,
    count: i32,
}

let cfg: EmbeddedConfig = parse_str_with_config(
    r#"
name "demo"
count 10
"#,
    &ParseConfig {
        root_mode: RootMode::WrapExpectedNode {
            name: "config".to_string(),
        },
        ..ParseConfig::default()
    },
)
.unwrap();
```

## Crate Architecture

The workspace contains two crates:

| Crate | Purpose |
| --- | --- |
| `kdl-config` | Runtime library with traits, parsing, rendering, round-trip, schema, fragments, and layered config loading. Re-exports derive macros. |
| `kdl-config-derive` | Proc-macro crate providing `#[derive(KdlNode)]`, `#[derive(KdlValue)]`, `#[derive(KdlChoice)]`, `#[derive(KdlSchema)]`, `#[derive(KdlMerge)]`, `#[derive(KdlPartial)]`, and the unified `#[derive(Kdl)]`. |

Users only need to depend on `kdl-config`; derive macros are re-exported automatically.

## Derive Macros

| Macro | Use Case |
| --- | --- |
| `KdlNode` | Structs and tagged enums: maps KDL nodes to Rust types. |
| `KdlValue` | Unit enums and newtype structs: maps KDL scalar values to Rust types via `FromKdlValue`. |
| `KdlChoice` | Choice enums: selects variant by node name. |
| `KdlSchema` | Schema generation: registers schema definitions for types. |
| `KdlMerge` | Generates `DeepMerge` impls for typed overlay merging with per-field merge policy. |
| `KdlPartial` | Generates `PartialConfig<T>` impls for `Option`-wrapped partial structs. |
| `Kdl` | Unified macro: dispatches to the above based on `#[kdl(choice)]`, `#[kdl(value)]`, or `#[kdl(schema)]` attributes. |

## Core Traits

| Trait | Description |
| --- | --- |
| `KdlDecode` | Parse a typed config from a `KdlNode`. |
| `KdlRender` | Render a value into canonical KDL text. |
| `KdlUpdate` | Update an existing `KdlNode` AST in-place (round-trip preserving). |
| `KdlEncode` | Encode a value into a new `KdlNode`. |
| `KdlSchema` | Describe the KDL schema for a type. |
| `FromKdlValue` | Convert a KDL scalar value to a Rust type. |
| `DeepMerge` | Merge one typed value onto another (`self.deep_merge(other)`). |
| `PartialConfig<T>` | Apply an optional overlay shape to a complete base config (`apply_to`). |

## Features

- **Declarative Mapping**: Use derive macros to map KDL nodes to Rust structs and enums.
- **Attributes & Arguments**: Map KDL keyed attributes, positional arguments, and flags to struct fields.
- **Naming & Aliasing**: Rename fields with `rename_all`, override names with `name = "..."` or `name = any("a", "b")`, and add aliases with `alias`.
- **Children**: Collect child nodes into `Vec`, `HashMap`, or nested structs — including `registry` and `children_map` patterns.
- **Flattening**: Inline nested struct fields into the parent node with `flatten`.
- **Defaults**: Supply defaults with `default`, `default = value`, or `default_fn = "path"`.
- **Type Conversion**: Decode through intermediate types with `from = "Type"` or `try_from = "Type"` for newtypes, validated wrappers, and domain types.
- **Validation**: Declare constraints on fields and structs — numeric ranges, string rules (byte length and character count), collection sizes, cross-field comparisons, regex patterns, and custom functions — all enforced at decode time with per-field source locations in error messages.
- **Post-Decode Hooks**: Normalize or reject fully-decoded structs with `#[kdl(post_decode = "path")]`.
- **Conflict Resolution**: Control duplicate handling with `conflict = "error" | "first" | "last" | "append"`.
- **Selectors**: Advanced key/discriminator extraction with `select(...)` (supports `arg(N)`, `attr("name")`, `name`, `func("path")`, and `any(...)`).
- **Fragments**: Reusable configuration templates with insert nodes and `~` merge patches, expanded before parsing.
- **Layered Configs**: Load and merge multiple KDL files with `KdlLoader`, using modifier signals (`+`, `-`, `!`) for merge control.
- **Typed Overlay Merging**: Derive `KdlMerge` and `KdlPartial` to remove manual merge boilerplate.
- **Round-trip Rendering**: Parse, modify, and re-serialize KDL while preserving original formatting via `RoundTripAst`.
- **Schema Generation**: Generate KDL schema metadata for your types with `KdlSchema`.
- **Deny Unknown**: Reject unrecognized attributes and children with `deny_unknown`.
- **Path Re-rooting**: Decode fields from nested paths with `path = "a.b"` or `path = "/a.b"`.
- **Built-in Newtypes**: `Duration`, `Weight`, `PositiveCount`, and `Scalar<T>` for common config value patterns.
- **Serde Integration**: Optional `serde` feature for serialization/deserialization of built-in newtypes.

## Naming and Aliasing

Rename all fields on a struct with `rename_all`, override individual field names, and add aliases:

```rust
use kdl_config::KdlNode;

#[derive(KdlNode)]
#[kdl(node = "config", rename_all = "kebab-case")]
struct Config {
    // KDL key: "log-level" (from rename_all)
    log_level: String,

    // KDL key: "output" (explicit override)
    #[kdl(name = "output")]
    output_path: String,

    // accepts "mode", "run-mode", or "m" in KDL
    #[kdl(name = any("mode", "run-mode"), alias = "m")]
    mode: String,
}
```

```kdl
config log-level="info" output="/var/log" mode="fast"
```

## Booleans

Boolean fields support three modes:

1. `presence+value` (default): accepts flags (e.g., `enabled`, `no-enabled`) and explicit values (`enabled=#true`).
2. `value-only`: accepts only explicit values (`enabled=#true` or `enabled=#false`), no flags.
3. `presence-only`: accepts only the **positive** flag token (e.g., `enabled`); negative flags are rejected.

Example:

```rust
use kdl_config::KdlNode;

#[derive(KdlNode)]
#[kdl(node = "config")]
struct Config {
    #[kdl(attr)]
    enabled: bool,
    #[kdl(attr, bool = "value-only")]
    strict: bool,
    #[kdl(attr, bool = "presence-only")]
    fast: bool,
}
```

KDL example for the struct above:

```kdl
config enabled no-enabled strict=#true fast
```

## Positional Lists

Collect all positional arguments into a `Vec<T>` with `positional = "rest"`:

```rust
use kdl_config::KdlNode;

#[derive(KdlNode)]
#[kdl(node = "config")]
struct Config {
    #[kdl(attr, positional = "rest")]
    sources: Vec<String>,
}
```

```kdl
config "main" "docs" "api"
```

## Children Maps

Collect map-like child nodes into `HashMap` or `Vec<(K, V)>` with `children_map`.

Key from child name:

```rust
use std::collections::HashMap;
use kdl_config::KdlNode;

#[derive(KdlNode)]
#[kdl(node = "indexing")]
struct IndexingConfig {
    #[kdl(attr)]
    chunk_size: i64,
}

#[derive(KdlNode)]
struct CategoryOverrides {
    #[kdl(child)]
    indexing: IndexingConfig,
}

#[derive(KdlNode)]
#[kdl(node = "config")]
struct Config {
    #[kdl(children_map)]
    categories: HashMap<String, CategoryOverrides>,
}
```

```kdl
config {
    docs { indexing chunk_size=3000 }
    code { indexing chunk_size=2000 }
}
```

Key from first arg (or `key_attr`/`key_fn`) with `map_node`:

```rust
#[derive(KdlNode)]
#[kdl(node = "config")]
struct Config {
    #[kdl(children_map, map_node = "category")]
    categories: Vec<(String, CategoryOverrides)>,
}
```

```kdl
config {
    category "docs" { indexing chunk_size=3000 }
    category "code" { indexing chunk_size=2000 }
}
```

Notes:
- `key_arg`/`key_attr`/`key_fn` are only valid when `map_node` is set.
- `HashMap` renders deterministically (sorted by key); `Vec<(K, V)>` preserves insertion order.

## Selectors

Advanced key/discriminator selection uses `select(...)` with a selector and optional options.
Selectors are supported on `registry`, `children_map`, and choice enums.

Selector forms:
- `arg(N)` uses positional arg `N`
- `attr("name")` uses an attribute value
- `name` uses the child node name
- `func("path::to::fn")` calls a helper function
- `any(...)` tries multiple selectors in order

Options:
- `consume` removes the selected token before parsing the child
- `preserve` keeps the selected token (default)
- `inject`/`inject="field"` injects the selected value into a field (where supported)

Example (children map key from attribute, consuming it before parsing the child):

```rust
#[derive(KdlNode)]
#[kdl(node = "config")]
struct Config {
    #[kdl(children_map, map_node = "category", select(attr("name"), consume))]
    categories: HashMap<String, CategoryOverrides>,
}
```

Example (enum discriminator from attr or arg):

```rust
#[derive(Kdl)]
#[kdl(node = "choice", select = any(attr("type"), arg(0)))]
enum Choice {
    #[kdl(tag = "alpha")]
    Alpha,
    #[kdl(tag = "beta")]
    Beta,
}
```

## Fragments

Fragments provide reusable, type-scoped templates that are expanded before parsing.

```kdl
fragment "code" source local {
    chunking { max-size 1500; overlap 200 }
    include "src/**"
}

source "app" {
    with "code"
    include "app/**"
}

from "code" source "docs"
```

Rules:
- `fragment "<key>" <type> ...` registers template defaults for `(type, key)`.
- `with "<key>"` applies a template inside an existing parent node.
- `from "<key>" <type> ...` materializes a new node from a template (top-level or sibling context).
- Names are scoped by type, so the same key can be reused for different node types.

Fragments are fully expanded before `#[derive(Kdl)]` parsing runs – the application never sees `fragment`, `with`, or `from` nodes.

## Schema Generation

Derive `KdlSchema` (or use `#[derive(Kdl)]` with `#[kdl(schema)]`) to register schema definitions.

```rust
use kdl_config::{KdlNode, KdlSchema};
use kdl_config::schema::SchemaRegistry;

#[derive(KdlNode)]
#[kdl(node = "config")]
struct Config {
    #[kdl(attr)]
    name: String,
}

let mut registry = SchemaRegistry::default();
Config::register_definitions(&mut registry);
```

### Schema Overrides

Override schema metadata per struct or field with `#[kdl(schema(...))]`:

```rust
use kdl_config::Kdl;

#[derive(Kdl)]
#[kdl(node = "config", schema(name = "config_schema", description = "Config schema", deny_unknown))]
struct Config {
    #[kdl(attr)]
    #[kdl(schema(name = "title", description = "Title", optional))]
    name: String,

    #[kdl(positional = 0)]
    #[kdl(schema(kind = "integer", description = "Primary count"))]
    count: i64,

    #[kdl(attr)]
    #[kdl(schema(skip))]
    ignored: String,
}
```

## Validation

Add `#[kdl(validate(...))]` to fields or structs to enforce constraints at decode time. All field-level errors are collected and reported together with per-field source locations.

```rust
use kdl_config::KdlNode;

#[derive(KdlNode)]
#[kdl(node = "server", validate(func = "check_server"))]
struct ServerConfig {
    #[kdl(validate(non_empty, max_len = 255))]
    host: String,

    #[kdl(validate(min = 1, max = 65535))]
    port: i64,

    #[kdl(validate(positive))]
    workers: i64,

    #[kdl(validate(less_than = "max_conns"))]
    min_conns: i64,
    max_conns: i64,
}

fn check_server(s: &ServerConfig) -> Result<(), String> {
    if s.host == "localhost" && s.workers > 4 {
        return Err("localhost limited to 4 workers".into());
    }
    Ok(())
}
```

**Supported rules:**

| Category | Rules |
| --- | --- |
| Numeric | `min`, `max`, `range(a,b)`, `multiple_of`, `positive`, `negative`, `non_negative`, `non_positive` |
| String (byte length) | `non_empty`, `min_len`, `max_len`, `len(a,b)`, `ascii`, `alphanumeric`, `pattern` |
| String (char count) | `min_chars`, `max_chars`, `chars(a,b)` |
| Collection | `min_items`, `max_items` |
| Cross-field (numeric) | `less_than`/`lt`, `lte`, `greater_than`/`gt`, `gte`, `equal_to`/`eq`, `not_equal_to`/`neq` |
| Cross-field (membership) | `exists_in`, `subset_of` |
| Custom | `func = "path::to::fn"` (field-level: `fn(&T) -> Result<(), String>`, struct-level: `fn(&Self) -> Result<(), String>`) |

`Option<T>` fields skip validation when `None`. `pattern` requires the `regex` feature (enabled by default).

### Post-Decode Hooks

Use `#[kdl(post_decode = "path")]` (or `#[kdl(post_decode(func = "path"))]`) to run a hook right after struct construction and before struct-level `validate(func = ...)` checks.

Hook signature:
- `fn(&mut Self) -> Result<(), String>`

```rust
fn normalize(cfg: &mut AppConfig) -> Result<(), String> {
    cfg.name = cfg.name.trim().to_string();
    if cfg.name.is_empty() {
        return Err("name must not be empty".to_string());
    }
    Ok(())
}

#[derive(KdlNode)]
#[kdl(node = "app", post_decode(func = "normalize"))]
struct AppConfig {
    #[kdl(attr)]
    name: String,
}
```

### Type Conversion

Decode through an intermediate type with `from` or `try_from` for newtypes and validated wrappers:

```rust
use kdl_config::KdlNode;

struct Port(u16);

impl TryFrom<i64> for Port {
    type Error = String;
    fn try_from(v: i64) -> Result<Self, String> {
        u16::try_from(v).map(Port).map_err(|_| format!("{v} is not a valid port"))
    }
}
impl From<Port> for i64 {
    fn from(p: Port) -> i64 { p.0 as i64 }
}

#[derive(KdlNode)]
#[kdl(node = "server")]
struct Server {
    #[kdl(try_from = "i64")]
    port: Port,
}
```

### Typed Overlay Merging

`KdlMerge` generates a `DeepMerge` implementation for structs and supports per-field policy:
- `#[kdl(merge = "deep")]` (default)
- `#[kdl(merge = "keep")]`
- `#[kdl(merge = "replace")]`
- `#[kdl(merge = "append")]`
- `#[kdl(merge(func = "path"))]`

`KdlPartial` generates `PartialConfig<T>` for partial (usually `Option`-wrapped) overlay structs:

```rust
use kdl_config::merge::PartialConfig;
use kdl_config::{KdlMerge, KdlPartial};

#[derive(Clone, KdlMerge)]
struct AppConfig {
    name: String,
    retries: u32,
}

#[derive(KdlPartial)]
#[kdl(partial_for = "AppConfig")]
struct AppConfigPartial {
    name: Option<String>,
    retries: Option<u32>,
}

let merged = AppConfigPartial { name: None, retries: Some(5) }.apply_to(AppConfig {
    name: "stag".to_string(),
    retries: 1,
});
assert_eq!(merged.retries, 5);
```

### Custom Scalar Types

If you derive `KdlValue` (or implement `FromKdlValue`) for a custom scalar type and want to use it
in `attr`/`positional`/`value` placements, mark the field as scalar:

```rust
use kdl_config::{KdlNode, KdlValue};

#[derive(Clone, Debug, KdlValue)]
enum Mode {
    Fast,
    Safe,
}

#[derive(KdlNode)]
#[kdl(node = "config")]
struct Config {
    #[kdl(attr, scalar)]
    mode: Mode,
}
```

## Built-in Newtypes

| Type | Description |
| --- | --- |
| `Scalar<T>` | Generic wrapper for map value nodes and scalar shapes. Implements `KdlDecode`, `KdlRender`, `KdlUpdate`, and `FromKdlValue`. |
| `Duration` | Parses duration strings (e.g., `"500ms"`, `"30s"`, `"5m"`, `"2h"`, `"1d"`) or raw millisecond integers. |
| `Weight` | A float in the range `0.0..=1.0`. |
| `PositiveCount` | A non-zero `u32` (`NonZeroU32`). |

All newtypes implement `KdlDecode`, `KdlRender`, `FromKdlValue`, and `KdlSchema`. When the `serde` feature is enabled, they also derive `Serialize` and `Deserialize`.

## Layered Configs

Load and merge multiple KDL layers (later layers override earlier ones) with `KdlLoader`.
Modifiers on node names control merge behavior: `+name` appends, `-name` removes, and `!name` replaces.

```rust
use kdl_config::loader::KdlLoader;

let loader = KdlLoader::new()
    .add_layer("config/base.kdl")
    .add_layer("config/override.kdl");

// Merge into a single root node.
let merged = loader.load_merged().unwrap();

// Or parse directly into a typed config.
let config: Config = loader.load().unwrap();
```

## Round-trip Rendering

Use `RoundTripAst` to parse, modify, and re-serialize KDL while preserving the original formatting:

```rust
use kdl_config::round_trip::parse_str_roundtrip;

#[derive(kdl_config::KdlNode)]
#[kdl(node = "config")]
struct Config {
    #[kdl(attr)]
    name: String,
}

let src = r#"config name="demo""#;
let mut parsed = parse_str_roundtrip::<Config>(src).unwrap();

// If no changes are made, to_kdl() returns the original string unchanged.
assert_eq!(parsed.to_kdl().unwrap(), src);

// Mutate and re-render:
parsed.value_mut().name = "updated".to_string();
let updated = parsed.to_kdl().unwrap();
```

Fragment-aware round-trip updates are also supported via `to_kdl_fragment_aware()`, which pushes changes back into fragment definitions when safe.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
