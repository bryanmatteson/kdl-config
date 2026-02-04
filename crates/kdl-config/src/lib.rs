#![allow(clippy::result_large_err)]

pub mod config;
pub mod convert;
pub mod error;
pub mod formatter;
pub mod helpers;
pub mod layer;
pub mod loader;
pub mod merge;
pub mod newtypes;
pub mod parse;
pub mod render;
pub mod round_trip;
pub mod schema;
pub mod types;

pub use config::{
    BoolMode, ConflictPolicy, DefaultPlacement, EffectiveConfig, FieldOverrides, FlagStyle,
    ParseConfig, StructOverrides, resolve_field, resolve_struct,
};
pub use convert::{
    ConvertContext, FromKdlValue, ValueConvertExt, convert_value, convert_value_checked,
    convert_value_checked_ctx, convert_value_ctx,
};
pub use error::{ErrorKind, KdlConfigError, Placement};
pub use formatter::KdlFormatter;
pub use layer::{
    LayerMerge, merge_layers, merge_layers_with, parse_layered, parse_layered_with_config,
};
pub use loader::{KdlLoader, LoadError, load_kdl_file};
pub use merge::{DeepMerge, MergeOption, PartialConfig};
pub use newtypes::{Duration, DurationParseError, Weight, WeightError};
pub use parse::parse_config;
pub use render::{
    NodeRenderer, escape_string, insert_arg, is_valid_identifier, render_key, render_key_with_repr,
    render_value, render_value_node, render_value_node_scalar, write_indent,
};
pub use round_trip::{
    RoundTrip, RoundTripMut, parse_str_roundtrip, parse_str_with_config_roundtrip,
};
pub use types::{MergeModifierPolicy, Modifier, Node, Value};

/// Trait for parsing a typed configuration from a KDL Node.
pub trait KdlParse: Sized {
    fn from_node(node: &Node, config: &ParseConfig) -> Result<Self, KdlConfigError>;
}

/// Trait for rendering a typed configuration to KDL format.
pub trait KdlRender {
    fn render<W: std::fmt::Write>(&self, w: &mut W, name: &str, indent: usize) -> std::fmt::Result;
}

impl<T: KdlParse> KdlParse for Box<T> {
    fn from_node(node: &Node, config: &ParseConfig) -> Result<Self, KdlConfigError> {
        T::from_node(node, config).map(Box::new)
    }
}

impl KdlParse for String {
    fn from_node(node: &Node, _config: &ParseConfig) -> Result<Self, KdlConfigError> {
        Ok(node.name.as_str().to_string())
    }
}

impl<T: KdlRender> KdlRender for Box<T> {
    fn render<W: std::fmt::Write>(&self, w: &mut W, name: &str, indent: usize) -> std::fmt::Result {
        (**self).render(w, name, indent)
    }
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
pub fn parse_str_with_config<T: KdlParse>(
    contents: &str,
    config: &ParseConfig,
) -> Result<T, KdlConfigError> {
    let root = parse_config(contents)?;
    let children = root.children();
    match children.len() {
        0 => Err(KdlConfigError::custom(
            "KDL Document",
            "expected a single top-level node, found none",
        )),
        1 => T::from_node(&children[0], config),
        count => Err(KdlConfigError::custom(
            "KDL Document",
            format!("expected a single top-level node, found {count}"),
        )),
    }
}

// Re-export derive macros so users only need to depend on `kdl-config`
pub use kdl_config_derive::{Kdl, KdlChoice, KdlNode, KdlSchema, KdlValue};
