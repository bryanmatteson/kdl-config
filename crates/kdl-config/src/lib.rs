#![allow(clippy::result_large_err)]

pub mod config;
pub mod context;
pub mod convert;
pub mod error;
pub mod formatter;
pub mod helpers;
pub mod layer;
pub mod loader;
pub mod merge;
pub mod newtypes;
pub mod node_ext;
pub mod node_path;
pub mod parse;
pub mod render;
pub mod round_trip;
pub mod schema;
pub mod selector;
pub mod fragments;
pub mod types;
mod primitive_decode;

pub use config::{
    BoolMode, ConflictPolicy, DefaultPlacement, EffectiveConfig, FieldOverrides, FlagStyle,
    ParseConfig, StructOverrides, resolve_field, resolve_struct,
};
pub use context::{DecodeContext, Source, UpdateContext};
pub use convert::{
    ConvertContext, FromKdlValue, ValueConvertExt, convert_value, convert_value_checked,
    convert_value_checked_ctx, convert_value_ctx,
};
pub use error::{ErrorKind, KdlConfigError, NodeLocation, Placement};
pub use formatter::KdlFormatter;
pub use kdl::{KdlDocument, KdlEntry, KdlNode, KdlValue};
pub use node_ext::{
    KdlNodeExt, arg_entry_index, attr_entry_indices, remove_attr_entries, remove_entry_indices,
    remove_positional_entry, update_entry_value, update_or_insert_attr,
    update_or_insert_positional,
};
pub use node_path::NodePath;
pub use parse::{parse_config, parse_node};
pub use render::{
    NodeRenderer, escape_string, insert_arg, is_valid_identifier, render_child_node,
    render_flatten, render_key, render_key_with_repr, render_node, render_value, render_value_node,
    render_value_node_scalar, render_value_with_repr, value_node, value_to_kdl,
    value_to_kdl_values, write_indent,
};
pub use round_trip::{RoundTripAst, parse_str_roundtrip};
pub use selector::{CollectMode, CollectionSpec, InjectOpt, SelectOpts, SelectSpec, SelectorAst};
pub use types::{MergeModifierPolicy, Modifier, Node, NodeLocation as NodeLocationLegacy, Value};
pub use fragments::{FragmentExpansion, expand_fragments};

/// Trait for decoding a typed configuration from a KDL Node.
pub trait KdlDecode: Sized {
    fn decode(node: &KdlNode, ctx: &DecodeContext) -> Result<Self, KdlConfigError>;
}

/// Trait for updating an existing KDL Node in-place (AST-preserving).
pub trait KdlUpdate {
    fn update(&self, node: &mut KdlNode, ctx: &UpdateContext) -> Result<(), KdlConfigError>;
}

/// Trait for rendering a value into KDL.
pub trait KdlRender {
    fn render<W: std::fmt::Write>(&self, w: &mut W, name: &str, indent: usize) -> std::fmt::Result;
    fn render_node(&self, name: &str) -> Node {
        let rendered = crate::to_kdl(self, name);
        crate::parse_node(&rendered).expect("rendered node is invalid KDL")
    }
}

/// Trait for encoding a value into a new KDL Node (canonical rendering).
pub trait KdlEncode {
    fn encode(&self, name: &str) -> KdlNode;
}

impl<T: KdlDecode> KdlDecode for Box<T> {
    fn decode(node: &KdlNode, ctx: &DecodeContext) -> Result<Self, KdlConfigError> {
        T::decode(node, ctx).map(Box::new)
    }
}

/// Render a KDL value into a string.
pub fn to_kdl<T: KdlRender + ?Sized>(value: &T, name: &str) -> String {
    let mut rendered = String::new();
    value
        .render(&mut rendered, name, 0)
        .expect("render to string");
    rendered
}

/// Parse a KDL string into a typed configuration using default config.
pub fn parse_str<T: KdlDecode>(contents: &str) -> Result<T, KdlConfigError> {
    parse_str_with_config(contents, &ParseConfig::default())
}

/// Parse a KDL string into a typed configuration using the provided config.
pub fn parse_str_with_config<T: KdlDecode>(
    contents: &str,
    config: &ParseConfig,
) -> Result<T, KdlConfigError> {
    let mut doc: KdlDocument = contents
        .parse()
        .map_err(|e: kdl::KdlError| KdlConfigError::custom("KDL Document", e.to_string()))?;
    let source = Source::new(contents.to_string());
    crate::fragments::expand_fragments(&mut doc, Some(&source))?;
    let ctx = DecodeContext::new(config, Some(&source));
    let nodes = doc.nodes();
    match nodes.len() {
        0 => Err(KdlConfigError::custom(
            "KDL Document",
            "expected a single top-level node, found none",
        )),
        1 => T::decode(&nodes[0], &ctx.with_root_node(&nodes[0], 0)),
        count => Err(KdlConfigError::custom(
            "KDL Document",
            format!("expected a single top-level node, found {count}"),
        )),
    }
}

// Re-export derive macros so users only need to depend on `kdl-config`
pub use kdl_config_derive::{Kdl, KdlChoice, KdlNode, KdlSchema, KdlValue};
