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

## Crate Architecture

The workspace contains two crates:

| Crate | Purpose |
| --- | --- |
| `kdl-config` | Runtime library with traits, parsing, rendering, round-trip, schema, fragments, and layered config loading. Re-exports derive macros. |
| `kdl-config-derive` | Proc-macro crate providing `#[derive(KdlNode)]`, `#[derive(KdlValue)]`, `#[derive(KdlChoice)]`, `#[derive(KdlSchema)]`, and the unified `#[derive(Kdl)]`. |

Users only need to depend on `kdl-config`; derive macros are re-exported automatically.

## Derive Macros

| Macro | Use Case |
| --- | --- |
| `KdlNode` | Structs and tagged enums: maps KDL nodes to Rust types. |
| `KdlValue` | Unit enums and newtype structs: maps KDL scalar values to Rust types via `FromKdlValue`. |
| `KdlChoice` | Choice enums: selects variant by node name. |
| `KdlSchema` | Schema generation: registers schema definitions for types. |
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

## Features

- **Declarative Mapping**: Use derive macros to map KDL nodes to Rust structs and enums.
- **Attributes & Arguments**: Map KDL keyed attributes, positional arguments, and flags to struct fields.
- **Children**: Collect child nodes into `Vec`, `HashMap`, or nested structs — including `registry` and `children_map` patterns.
- **Selectors**: Advanced key/discriminator extraction with `select(...)` (supports `arg(N)`, `attr("name")`, `name`, `func("path")`, and `any(...)`).
- **Fragments**: Reusable configuration templates with insert nodes and `~` merge patches, expanded before parsing.
- **Layered Configs**: Load and merge multiple KDL files with `KdlLoader`, using modifier signals (`+`, `-`, `!`) for merge control.
- **Round-trip Rendering**: Parse, modify, and re-serialize KDL while preserving original formatting via `RoundTripAst`.
- **Schema Generation**: Generate KDL schema metadata for your types with `KdlSchema`.
- **Built-in Newtypes**: `Duration`, `Weight`, `PositiveCount`, and `Scalar<T>` for common config value patterns.
- **Serde Integration**: Optional `serde` feature for serialization/deserialization of built-in newtypes.

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

Fragments provide reusable, optionally typed configuration templates that are expanded before parsing. They support two kinds of entries:

- **Insert nodes** — normal KDL nodes copied as children into the target.
- **Merge patches** (`~`) — nodes that flatten into the target using deep-merge semantics.

```kdl
fragment "local-defaults" {
    ~source local {
        chunking { max-size 1500; overlap 200 }
        enrichment { symbols; hover; definitions }
        embed "code-model"
    }
    language "rust" { server "rust-analyzer" }
}

source "app" local "." {
    with "local-defaults"
    include "src/**"
}
```

After expansion:

```kdl
source "app" local "." {
    language "rust" { server "rust-analyzer" }
    chunking { max-size 1500; overlap 200 }
    enrichment { symbols; hover; definitions }
    embed "code-model"
    include "src/**"
}
```

Fragments are fully expanded before `#[derive(Kdl)]` parsing runs – the application never sees `fragment` or `with` nodes.

## Schema Generation

Derive `KdlSchema` (or use `#[derive(Kdl)]` with `#[kdl(schema)]`) to register schema definitions.

```rust
use kdl_config::{KdlNode, KdlSchema};
use kdl_config::schema::SchemaRegistry;

#[derive(KdlNode, KdlSchema)]
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
    #[kdl(schema(type = "integer", description = "Primary count"))]
    count: i64,

    #[kdl(attr)]
    #[kdl(schema(skip))]
    ignored: String,
}
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
