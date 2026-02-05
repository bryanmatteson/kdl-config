# kdl-config

Strongly-typed configuration from KDL documents.

`kdl-config` allows you to define your configuration structure using Rust structs and automatically parse KDL documents into those structs. It supports attributes, arguments, children, and more, with highly customizable mappings.

## Installation

Add the following to your `Cargo.toml`:

```toml
[dependencies]
kdl-config = "0.1.0"
```

## Usage

```rust
use kdl_config::{KdlNode, parse_str};

#[derive(Debug, KdlNode)]
#[kdl(node = "config")]
struct MyConfig {
    name: String,
    #[kdl(attr)]
    count: i32,
}

fn main() {
    let config: MyConfig = parse_str(r#"config name="demo" count=10"#).unwrap();
    println!("{:?}", config);
}
```

Note: For `bool` fields using `#[kdl(bool = "presence-only")]`, only the positive flag token is valid; negative flags are rejected.

## Features

- **Declarative Mapping**: Use `#[derive(KdlNode)]` to map KDL nodes to Rust structs.
- **Attributes & Arguments**: Easily map KDL attributes and arguments to struct fields.
- **Children**: Automatically collect children nodes into `Vec`, `HashMap`, or nested structs.
- **Customization**: Renaming, default values, and type conversion support.
- **Schema Generation**: Generate KDL schema metadata for your types.
- **Round-trip Rendering**: Preserve original KDL when no changes are made.

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

Notes:
- `schema(type = ...)` is only valid for scalar value fields.
- `schema(skip)` excludes a field from schema generation only (parsing/rendering still apply).

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

### Union Schemas

Unions can generate schema-only `choice` nodes:

```rust
use kdl_config::KdlSchema;

#[derive(KdlSchema)]
union Choice {
    #[kdl(schema(name = "count", type = "integer"))]
    count: i64,
}
```

## Layered Configs

Load and merge multiple KDL layers (later layers override earlier ones) with `KdlLoader`.
Modifiers on node names control merge behavior: `+name` appends, `-name` removes, and `!name` replaces.

```rust
use kdl_config::KdlLoader;

let loader = KdlLoader::new()
    .add_layer("config/base.kdl")
    .add_layer("config/override.kdl");

// Merge into a single root node.
let merged = loader.load_merged().unwrap();

// Or parse directly into a typed config.
let config: Config = loader.load().unwrap();
```

## Round-trip Rendering

Use round-trip helpers to preserve the original KDL when the canonical rendering matches the input:

```rust
use kdl_config::round_trip::parse_str_roundtrip;

#[derive(kdl_config::KdlNode)]
#[kdl(node = "config")]
struct Config {
    #[kdl(attr)]
    name: String,
}

let src = r#"config name="demo""#;
let parsed = parse_str_roundtrip::<Config>(src).unwrap();
assert_eq!(parsed.to_kdl(), src);
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
