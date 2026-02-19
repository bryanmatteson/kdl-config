//! Container-level (struct/enum) attribute definitions.

use super::types::{
    BoolMode, ConflictPolicy, DefaultPlacement, FlagStyle, RenameStrategy, SelectorAst,
    ValidationRule,
};

/// Schema overrides for struct-level attributes.
///
/// Populated by manual `parse_nested_meta` parsing in `parse.rs`.
#[derive(Debug, Clone, Default)]
pub struct StructSchemaOverride {
    /// Override the schema name.
    pub name: Option<String>,
    /// Schema description (set by both `description` and `desc` in attributes).
    pub description: Option<String>,
    /// Whether to deny unknown fields in schema.
    pub deny_unknown: Option<bool>,
    /// Inverse of deny_unknown.
    pub allow_unknown: Option<bool>,
    /// Validation rules parsed from `#[kdl(validate(...))]` or `#[kdl(validate = "...")]`.
    pub validations: Vec<ValidationRule>,
}

impl StructSchemaOverride {
    /// Get the resolved description.
    pub fn resolved_description(&self) -> Option<&String> {
        self.description.as_ref()
    }

    /// Get the resolved deny_unknown setting (also checks `allow_unknown` inverse).
    pub fn resolved_deny_unknown(&self) -> Option<bool> {
        self.deny_unknown.or(self.allow_unknown.map(|v| !v))
    }
}

/// Struct-level KDL attributes.
///
/// Parsed from `#[kdl(...)]` on structs.
#[derive(Debug, Default)]
pub struct StructAttrs {
    /// Explicit node name override.
    pub node_name: Option<String>,
    /// Whether `node` was specified without a value (use type name).
    pub node_name_default: bool,
    /// Rename strategy for all fields.
    pub rename_all: RenameStrategy,
    /// Whether rename_all was explicitly set.
    pub rename_all_explicit: bool,
    /// Default field placement strategy.
    pub default_placement: Option<DefaultPlacement>,
    /// Default boolean parsing mode.
    pub default_bool: Option<BoolMode>,
    /// Default flag rendering style.
    pub default_flag_style: Option<FlagStyle>,
    /// Default conflict resolution policy.
    pub default_conflict: Option<ConflictPolicy>,
    /// Selector for enum discriminators (tagged enums).
    pub selector: Option<SelectorAst>,
    /// Selector options for enum discriminators (tagged enums).
    pub selector_opts: super::types::SelectOpts,
    /// Selector spec for enum discriminators (tagged enums).
    pub selector_spec: Option<super::types::SelectSpec>,
    /// Whether to deny unknown fields/attributes.
    pub deny_unknown: Option<bool>,
    /// Schema overrides.
    pub schema: StructSchemaOverride,
    /// Validation rules parsed from `#[kdl(validate(...))]` or `#[kdl(validate = "...")]`.
    pub validations: Vec<ValidationRule>,
    /// Optional post-decode hook path, called as `fn(&mut Self) -> Result<(), String>`.
    pub post_decode: Option<String>,
}

impl StructAttrs {
    /// Get the resolved node name, using type name if `node` was specified without value.
    pub fn resolved_node_name(&self, type_name: &str) -> Option<String> {
        if let Some(name) = &self.node_name {
            Some(name.clone())
        } else if self.node_name_default {
            Some(type_name.to_string())
        } else {
            None
        }
    }
}
