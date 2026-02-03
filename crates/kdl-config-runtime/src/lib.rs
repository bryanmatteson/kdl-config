#![allow(clippy::result_large_err)]

pub mod config;
pub mod convert;
pub mod error;
pub mod helpers;
pub mod parse;
pub mod render;
pub mod types;

pub use config::{BoolMode, ConflictPolicy, DefaultPlacement, FlagStyle, ParseConfig, StructOverrides, FieldOverrides, EffectiveConfig, resolve_struct, resolve_field};
pub use convert::{FromKdlValue, convert_value, convert_value_checked};
pub use error::{ErrorKind, KdlConfigError, Placement};
pub use parse::parse_config;
pub use render::{NodeRenderer, escape_string, is_valid_identifier, render_key, render_value, render_value_node, render_value_node_scalar, write_indent, insert_arg};
pub use types::{Modifier, Node, Value};

/// Trait for parsing a typed configuration from a KDL Node.
pub trait KdlParse: Sized {
    fn from_node(node: &Node, config: &ParseConfig) -> Result<Self, KdlConfigError>;
}

/// Trait for rendering a typed configuration to KDL format.
pub trait KdlRender {
    fn render<W: std::fmt::Write>(&self, w: &mut W, name: &str, indent: usize) -> std::fmt::Result;
}

/// Convenience to render a type to a KDL string.
pub fn to_kdl<T: KdlRender + ?Sized>(value: &T, name: &str) -> String {
    let mut s = String::new();
    let _ = value.render(&mut s, name, 0);
    s
}

/// Extension trait with helpers for rendering.
pub trait KdlRenderExt: KdlRender {
    fn to_kdl_string(&self) -> String
    where
        Self: Sized,
    {
        to_kdl(self, "value")
    }

    fn to_kdl_named(&self, name: &str) -> String
    where
        Self: Sized,
    {
        to_kdl(self, name)
    }
}

impl<T: KdlRender> KdlRenderExt for T {}

/// Parse a KDL string into a typed configuration using default config.
pub fn parse_str<T: KdlParse>(contents: &str) -> Result<T, KdlConfigError> {
    parse_str_with_config(contents, &ParseConfig::default())
}

/// Parse a KDL string into a typed configuration using the provided config.
pub fn parse_str_with_config<T: KdlParse>(contents: &str, config: &ParseConfig) -> Result<T, KdlConfigError> {
    let root = parse_config(contents)?;
    T::from_node(&root, config)
}
