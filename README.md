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

## Features

- **Declarative Mapping**: Use `#[derive(KdlNode)]` to map KDL nodes to Rust structs.
- **Attributes & Arguments**: Easily map KDL attributes and arguments to struct fields.
- **Children**: Automatically collect children nodes into `Vec`, `HashMap`, or nested structs.
- **Customization**: Renaming, default values, and type conversion support.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
