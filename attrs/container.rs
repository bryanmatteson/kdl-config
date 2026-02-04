//! Container-level (struct/enum) attribute definitions.

use darling::FromMeta;

use super::types::{BoolMode, ConflictPolicy, DefaultPlacement, FlagStyle, RenameStrategy};

/// Schema overrides for struct-level attributes.
#[derive(Debug, Clone, Default)]
pub struct StructSchemaOverride {
    /// Override the schema name.
    pub name: Option<String>,
    /// Schema description.
    pub description: Option<String>,
    /// Alias for description.
    pub desc: Option<String>,
    /// Whether to deny unknown fields in schema.
    pub deny_unknown: Option<bool>,
    /// Inverse of deny_unknown.
    pub allow_unknown: Option<bool>,
}

impl FromMeta for StructSchemaOverride {
    fn from_word() -> darling::Result<Self> {
        // #[kdl(schema)] - enable schema with defaults
        Ok(Self::default())
    }

    fn from_list(items: &[darling::ast::NestedMeta]) -> darling::Result<Self> {
        let mut result = Self::default();

        for item in items {
            match item {
                darling::ast::NestedMeta::Meta(meta) => {
                    let ident = match meta {
                        syn::Meta::Path(p) => p.get_ident().map(|i| i.to_string()),
                        syn::Meta::NameValue(nv) => nv.path.get_ident().map(|i| i.to_string()),
                        syn::Meta::List(l) => l.path.get_ident().map(|i| i.to_string()),
                    };

                    match ident.as_deref() {
                        Some("name") | Some("rename") => {
                            if let syn::Meta::NameValue(nv) = meta {
                                if let syn::Expr::Lit(syn::ExprLit {
                                    lit: syn::Lit::Str(s),
                                    ..
                                }) = &nv.value
                                {
                                    result.name = Some(s.value());
                                }
                            }
                        }
                        Some("description") | Some("desc") => {
                            if let syn::Meta::NameValue(nv) = meta {
                                if let syn::Expr::Lit(syn::ExprLit {
                                    lit: syn::Lit::Str(s),
                                    ..
                                }) = &nv.value
                                {
                                    result.description = Some(s.value());
                                }
                            }
                        }
                        Some("deny_unknown") => {
                            result.deny_unknown =
                                Some(parse_optional_bool_meta(meta).unwrap_or(true));
                        }
                        Some("allow_unknown") => {
                            result.allow_unknown =
                                Some(parse_optional_bool_meta(meta).unwrap_or(true));
                        }
                        _ => {}
                    }
                }
                darling::ast::NestedMeta::Lit(_) => {}
            }
        }

        Ok(result)
    }
}

fn parse_optional_bool_meta(meta: &syn::Meta) -> Option<bool> {
    match meta {
        syn::Meta::Path(_) => Some(true),
        syn::Meta::NameValue(nv) => {
            if let syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Bool(b),
                ..
            }) = &nv.value
            {
                Some(b.value())
            } else {
                Some(true)
            }
        }
        syn::Meta::List(_) => Some(true),
    }
}

impl StructSchemaOverride {
    /// Get the resolved description (checking both `description` and `desc`).
    pub fn resolved_description(&self) -> Option<&String> {
        self.description.as_ref().or(self.desc.as_ref())
    }

    /// Get the resolved deny_unknown setting.
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
    /// Whether to deny unknown fields/attributes.
    pub deny_unknown: Option<bool>,
    /// Schema overrides.
    pub schema: StructSchemaOverride,
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

/// Raw parsed struct attributes from darling.
///
/// This is the intermediate representation before processing.
/// NOTE: Currently unused as we use manual parsing, but kept for potential future use with darling.
#[allow(dead_code)]
#[derive(Debug, Default, FromMeta)]
#[darling(default)]
pub struct RawStructAttrs {
    /// Node name - can be `node` (flag) or `node = "name"`.
    pub node: Option<NodeAttr>,

    /// Rename all fields using this strategy.
    pub rename_all: Option<RenameStrategy>,

    /// Default placement for fields without explicit placement.
    pub default_placement: Option<DefaultPlacement>,

    /// Default bool parsing mode.
    pub default_bool: Option<BoolMode>,

    /// Default flag style.
    pub default_flag_style: Option<FlagStyle>,

    /// Default conflict resolution.
    pub default_conflict: Option<ConflictPolicy>,

    /// Deny unknown fields/attributes.
    #[darling(default)]
    pub deny_unknown: bool,

    /// Schema overrides.
    #[darling(default)]
    pub schema: StructSchemaOverride,

    // Ignored attributes (parsed but not used at struct level)
    #[darling(default)]
    pub meta: darling::util::Ignored,
    #[darling(default)]
    pub group: darling::util::Ignored,
    #[darling(default)]
    pub choice: darling::util::Ignored,
    #[darling(default)]
    pub value: darling::util::Ignored,
}

/// Node attribute which can be a flag or have a string value.
#[derive(Debug, Clone)]
pub enum NodeAttr {
    /// `node` - use type name.
    Flag,
    /// `node = "name"` - explicit name.
    Name(String),
}

impl Default for NodeAttr {
    fn default() -> Self {
        NodeAttr::Flag
    }
}

impl FromMeta for NodeAttr {
    fn from_word() -> darling::Result<Self> {
        Ok(NodeAttr::Flag)
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        Ok(NodeAttr::Name(value.to_string()))
    }

    fn from_value(value: &syn::Lit) -> darling::Result<Self> {
        match value {
            syn::Lit::Str(s) => Ok(NodeAttr::Name(s.value())),
            _ => Err(darling::Error::unexpected_lit_type(value)),
        }
    }
}

impl RawStructAttrs {
    /// Convert raw parsed attributes to processed form.
    pub fn into_struct_attrs(self, serde_rename_all: Option<RenameStrategy>) -> StructAttrs {
        let rename_all_explicit = self.rename_all.is_some();
        let rename_all = self
            .rename_all
            .or(serde_rename_all)
            .unwrap_or(RenameStrategy::None);

        let (node_name, node_name_default) = match self.node {
            Some(NodeAttr::Name(name)) => (Some(name), false),
            Some(NodeAttr::Flag) => (None, true),
            None => (None, false),
        };

        StructAttrs {
            node_name,
            node_name_default,
            rename_all,
            rename_all_explicit,
            default_placement: self.default_placement,
            default_bool: self.default_bool,
            default_flag_style: self.default_flag_style,
            default_conflict: self.default_conflict,
            deny_unknown: if self.deny_unknown { Some(true) } else { None },
            schema: self.schema,
        }
    }
}
