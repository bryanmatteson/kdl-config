//! Field-level attribute definitions.

use darling::FromMeta;
use proc_macro2::Span;
use syn::{Expr, ExprLit, ExprUnary, Lit, UnOp};

use super::types::{
    BoolMode, ConflictPolicy, DefaultLiteral, DefaultSpec, FlagStyle, RegistryKey, RenderPlacement,
    SchemaTypeOverride,
};

/// Field placement specification.
#[derive(Debug, Clone, Default)]
pub struct FieldPlacement {
    pub attr: bool,
    pub keyed: bool,
    pub positional: Option<usize>,
    pub positional_list: bool,
    pub flag: Option<(Option<String>, Option<String>)>,
    pub value: bool,
    pub child: bool,
    pub children: bool,
    pub children_any: bool,
    pub registry: bool,
    pub modifier: bool,
}

/// Schema overrides for field-level attributes.
#[derive(Debug, Clone, Default, FromMeta)]
#[darling(default)]
pub struct FieldSchemaOverride {
    /// Skip this field in schema generation.
    #[darling(default)]
    pub skip: bool,
    /// Override the schema field name.
    pub name: Option<String>,
    /// Alias for name.
    pub rename: Option<String>,
    /// Override required status.
    pub required: Option<bool>,
    /// Override optional status (inverse of required).
    pub optional: Option<bool>,
    /// Override the schema type (use `kind` instead of `type` since `type` is a Rust keyword).
    pub kind: Option<SchemaTypeOverride>,
    /// Schema description.
    pub description: Option<String>,
    /// Alias for description.
    pub desc: Option<String>,
}

impl FieldSchemaOverride {
    /// Get the resolved name.
    pub fn resolved_name(&self) -> Option<&String> {
        self.name.as_ref().or(self.rename.as_ref())
    }

    /// Get the resolved description.
    pub fn resolved_description(&self) -> Option<&String> {
        self.description.as_ref().or(self.desc.as_ref())
    }

    /// Get the resolved required status.
    pub fn resolved_required(&self) -> Option<bool> {
        self.required.or(self.optional.map(|v| !v))
    }

    /// Get the resolved type override.
    pub fn resolved_kind(&self) -> Option<SchemaTypeOverride> {
        self.kind.map(|t| t.canonical())
    }
}

/// Processed field attributes.
#[derive(Debug)]
pub struct FieldAttrs {
    pub span: Span,
    pub placement: FieldPlacement,
    pub required: Option<bool>,
    pub name: Option<String>,
    pub container: Option<String>,
    pub children_map: bool,
    pub flatten: bool,
    pub map_node: Option<String>,
    pub registry_key: Option<RegistryKey>,
    pub default: Option<DefaultSpec>,
    pub skip: bool,
    pub skip_serializing_if: Option<String>,
    pub bool_mode: Option<BoolMode>,
    pub flag_style: Option<FlagStyle>,
    pub conflict: Option<ConflictPolicy>,
    pub render: Option<RenderPlacement>,
    pub scalar: bool,
    pub schema: FieldSchemaOverride,
}

/// Positional argument specification (can be index or "rest"/"*").
#[derive(Debug, Clone)]
pub enum PositionalArg {
    Index(usize),
    Rest,
}

impl Default for PositionalArg {
    fn default() -> Self {
        PositionalArg::Index(0)
    }
}

impl FromMeta for PositionalArg {
    fn from_word() -> darling::Result<Self> {
        Ok(PositionalArg::Rest)
    }

    fn from_value(value: &syn::Lit) -> darling::Result<Self> {
        match value {
            syn::Lit::Int(i) => {
                let idx: usize = i.base10_parse().map_err(darling::Error::custom)?;
                Ok(PositionalArg::Index(idx))
            }
            syn::Lit::Str(s) => {
                let val = s.value();
                if val == "*" || val == "rest" {
                    Ok(PositionalArg::Rest)
                } else {
                    Err(darling::Error::custom(format!(
                        "expected integer or \"rest\"/\"*\", got \"{}\"",
                        val
                    )))
                }
            }
            _ => Err(darling::Error::unexpected_lit_type(value)),
        }
    }
}

/// Flag attribute - can be bare, or have a string value.
#[derive(Debug, Clone, Default)]
pub struct FlagAttr(pub Option<String>);

impl FromMeta for FlagAttr {
    fn from_word() -> darling::Result<Self> {
        Ok(FlagAttr(None))
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        Ok(FlagAttr(Some(value.to_string())))
    }

    fn from_value(value: &syn::Lit) -> darling::Result<Self> {
        match value {
            syn::Lit::Str(s) => Ok(FlagAttr(Some(s.value()))),
            _ => Err(darling::Error::unexpected_lit_type(value)),
        }
    }
}

/// Default attribute - can be bare, have a literal value, or be absent.
#[derive(Debug, Clone, Default)]
pub enum DefaultAttr {
    #[default]
    Absent,
    Derive,
    Literal(DefaultLiteral),
}

impl FromMeta for DefaultAttr {
    fn from_word() -> darling::Result<Self> {
        Ok(DefaultAttr::Derive)
    }

    fn from_value(value: &syn::Lit) -> darling::Result<Self> {
        parse_default_literal(value)
            .map(DefaultAttr::Literal)
            .map_err(darling::Error::custom)
    }

    fn from_expr(expr: &Expr) -> darling::Result<Self> {
        parse_default_expr(expr)
            .map(DefaultAttr::Literal)
            .map_err(darling::Error::custom)
    }
}

/// Raw parsed field attributes from darling.
#[derive(Debug, Default, FromMeta)]
#[darling(default)]
pub struct RawFieldAttrs {
    // === Placement flags ===
    #[darling(default)]
    pub attr: bool,
    #[darling(default)]
    pub keyed: bool,
    pub positional: Option<PositionalArg>,
    #[darling(default)]
    pub positional_list: bool,
    pub flag: Option<FlagAttr>,
    pub neg_flag: Option<String>,
    #[darling(default)]
    pub value: bool,
    #[darling(default)]
    pub child: bool,
    #[darling(default)]
    pub children: bool,
    #[darling(default)]
    pub children_any: bool,
    #[darling(default)]
    pub choice: bool, // alias for children_any
    #[darling(default)]
    pub registry: bool,
    #[darling(default)]
    pub modifier: bool,

    // === Configuration ===
    pub name: Option<String>,
    pub rename: Option<String>, // alias for name
    pub container: Option<String>,
    #[darling(default)]
    pub children_map: bool,
    pub map_node: Option<String>,
    pub flatten: Option<bool>,

    // === Required/optional ===
    #[darling(default)]
    pub required: bool,
    #[darling(default)]
    pub optional: bool,

    // === Registry keys ===
    pub key_arg: Option<usize>,
    pub key_attr: Option<String>,
    pub key_fn: Option<String>,

    // === Defaults ===
    #[darling(default)]
    pub default: DefaultAttr,
    pub default_fn: Option<String>,

    // === Skip ===
    #[darling(default)]
    pub skip: bool,
    pub skip_serializing_if: Option<String>,

    // === Mode overrides ===
    #[darling(rename = "bool")]
    pub bool_mode: Option<BoolMode>,
    pub flag_style: Option<FlagStyle>,
    pub conflict: Option<ConflictPolicy>,
    pub render: Option<RenderPlacement>,

    // === Type hints ===
    #[darling(default)]
    pub scalar: bool,
    #[darling(default)]
    pub value_type: bool, // alias for scalar
    #[darling(default)]
    pub value_like: bool, // alias for scalar
    #[darling(default)]
    pub kdl_value: bool, // alias for scalar

    // === Schema ===
    #[darling(default)]
    pub schema: FieldSchemaOverride,

    // === Ignored groupings ===
    #[darling(default)]
    pub meta: darling::util::Ignored,
    #[darling(default)]
    pub group: darling::util::Ignored,
}

impl RawFieldAttrs {
    /// Convert raw parsed attributes to processed form.
    pub fn into_field_attrs(self, span: Span) -> syn::Result<FieldAttrs> {
        // Build placement
        let mut placement = FieldPlacement::default();
        placement.attr = self.attr;
        placement.keyed = self.keyed;
        placement.value = self.value;
        placement.child = self.child;
        placement.children = self.children;
        placement.children_any = self.children_any || self.choice;
        placement.registry = self.registry;
        placement.modifier = self.modifier;

        // Handle positional
        match self.positional {
            Some(PositionalArg::Index(idx)) => placement.positional = Some(idx),
            Some(PositionalArg::Rest) => placement.positional_list = true,
            None => {}
        }
        if self.positional_list {
            placement.positional_list = true;
        }

        // Handle flag
        if let Some(flag_attr) = self.flag {
            let neg = self.neg_flag.clone();
            placement.flag = Some((flag_attr.0, neg));
        } else if self.neg_flag.is_some() {
            placement.flag = Some((None, self.neg_flag.clone()));
        }

        // Resolve required
        let required = if self.required {
            Some(true)
        } else if self.optional {
            Some(false)
        } else {
            None
        };

        // Resolve name
        let name = self.name.or(self.rename);

        // Resolve flatten
        let flatten = self.flatten.unwrap_or(false);

        // Resolve registry key
        let registry_key = if let Some(idx) = self.key_arg {
            Some(RegistryKey::Arg(idx))
        } else if let Some(attr) = self.key_attr {
            Some(RegistryKey::Attr(attr))
        } else if let Some(func) = self.key_fn {
            Some(RegistryKey::Function(func))
        } else {
            None
        };

        // Resolve default
        let mut default_count = 0u8;
        let default = match self.default {
            DefaultAttr::Absent => {
                if let Some(func) = self.default_fn {
                    default_count += 1;
                    Some(DefaultSpec::Function(func))
                } else {
                    None
                }
            }
            DefaultAttr::Derive => {
                default_count += 1;
                if self.default_fn.is_some() {
                    default_count += 1;
                }
                Some(DefaultSpec::Derive)
            }
            DefaultAttr::Literal(lit) => {
                default_count += 1;
                if self.default_fn.is_some() {
                    default_count += 1;
                }
                Some(DefaultSpec::Literal(lit))
            }
        };

        if default_count > 1 {
            return Err(syn::Error::new(
                span,
                "field cannot have multiple default specifications (`default`, `default = ...`, `default_fn`)",
            ));
        }

        // Resolve scalar
        let scalar = self.scalar || self.value_type || self.value_like || self.kdl_value;

        Ok(FieldAttrs {
            span,
            placement,
            required,
            name,
            container: self.container,
            children_map: self.children_map,
            flatten,
            map_node: self.map_node,
            registry_key,
            default,
            skip: self.skip,
            skip_serializing_if: self.skip_serializing_if,
            bool_mode: self.bool_mode,
            flag_style: self.flag_style,
            conflict: self.conflict,
            render: self.render,
            scalar,
            schema: self.schema,
        })
    }
}

// ============================================================================
// Default value parsing helpers
// ============================================================================

fn parse_default_expr(expr: &Expr) -> syn::Result<DefaultLiteral> {
    match expr {
        Expr::Lit(ExprLit { lit, .. }) => parse_default_literal(lit),
        Expr::Unary(ExprUnary {
            op: UnOp::Neg(_),
            expr,
            ..
        }) => match &**expr {
            Expr::Lit(ExprLit { lit, .. }) => parse_negated_default_literal(lit),
            other => Err(syn::Error::new_spanned(
                other,
                "default value must be a literal or negated literal",
            )),
        },
        _ => Err(syn::Error::new_spanned(
            expr,
            "default value must be a literal or negated literal",
        )),
    }
}

fn parse_default_literal(lit: &Lit) -> syn::Result<DefaultLiteral> {
    match lit {
        Lit::Int(lit_int) => Ok(DefaultLiteral::Int(lit_int.base10_parse()?)),
        Lit::Float(lit_float) => Ok(DefaultLiteral::Float(lit_float.base10_parse()?)),
        Lit::Bool(lit_bool) => Ok(DefaultLiteral::Bool(lit_bool.value())),
        Lit::Str(lit_str) => Ok(DefaultLiteral::String(lit_str.value())),
        _ => Err(syn::Error::new_spanned(
            lit,
            "default value must be an integer, float, boolean, or string literal",
        )),
    }
}

fn parse_negated_default_literal(lit: &Lit) -> syn::Result<DefaultLiteral> {
    match lit {
        Lit::Int(lit_int) => Ok(DefaultLiteral::Int(-lit_int.base10_parse::<i128>()?)),
        Lit::Float(lit_float) => Ok(DefaultLiteral::Float(-lit_float.base10_parse::<f64>()?)),
        _ => Err(syn::Error::new_spanned(
            lit,
            "negated default value must be a numeric literal",
        )),
    }
}
