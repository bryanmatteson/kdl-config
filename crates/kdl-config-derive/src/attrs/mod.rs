//! Attribute parsing for KDL derive macros.
//!
//! This module provides modular attribute definitions and manual `parse_nested_meta`
//! parsing, replacing the previous ~2500 lines of monolithic attribute handling.
//!
//! # Module Structure
//!
//! - `types` - Enum definitions for attribute values
//! - `container` - Struct-level attribute definitions
//! - `field` - Field-level attribute definitions
//! - `field_info` - Processed field information for codegen
//! - `type_utils` - Type analysis utilities
//! - `parse` - Manual parsing entry points using `parse_nested_meta`

mod container;
mod field;
mod field_info;
mod parse;
mod type_utils;
mod types;

// Re-export main types for public API
pub use container::StructAttrs;
pub use field_info::{has_child_placement, has_value_placement, FieldInfo};
pub use parse::{
    parse_field_attrs, parse_struct_attrs, serde_rename_all_from_attrs, serde_rename_from_attrs,
};
pub use type_utils::{
    extract_children_map_types, extract_hashmap_types, extract_inner_type,
    extract_registry_vec_value, is_bool_type, is_numeric_type, is_option_type, is_string_type,
    is_value_type,
};
#[allow(unused_imports)]
pub use types::{
    BoolMode, ChildrenMapKind, CollectionMode, CollectionSpec, ConflictPolicy, DefaultLiteral,
    DefaultPlacement, DefaultSpec, FlagStyle, InjectOpt, RenameStrategy, RenderPlacement,
    SchemaTypeOverride, SelectOpts, SelectSpec, SelectorAst,
};
