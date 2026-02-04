//! Attribute parsing functions.
//!
//! Uses manual `parse_nested_meta` for consistent parsing of all KDL attributes.

use syn::{Attribute, Field};

use super::container::StructAttrs;
use super::field::{FieldAttrs, RawFieldAttrs};
use super::types::RenameStrategy;

/// Parse struct-level attributes from `#[kdl(...)]`.
pub fn parse_struct_attrs(attrs: &[Attribute]) -> syn::Result<StructAttrs> {
    let serde_rename_all = serde_rename_all_from_attrs(attrs)?;
    let serde_rename = serde_rename_from_attrs(attrs)?;

    let mut result = StructAttrs::default();

    for attr in attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }

        // Use manual parsing for consistency and to handle edge cases
        attr.parse_nested_meta(|meta| parse_struct_meta(&meta, &mut result))?;
    }

    // Apply serde rename_all as fallback
    if result.rename_all == super::types::RenameStrategy::None {
        if let Some(strategy) = serde_rename_all {
            result.rename_all = strategy;
        }
    }

    // Apply serde rename as fallback for node name
    if result.node_name_default && result.node_name.is_none() {
        if let Some(rename) = serde_rename {
            result.node_name = Some(rename);
            result.node_name_default = false;
        }
    }

    Ok(result)
}

/// Parse a single struct meta item.
fn parse_struct_meta(
    meta: &syn::meta::ParseNestedMeta,
    result: &mut StructAttrs,
) -> syn::Result<()> {
    use syn::{Expr, ExprLit, Lit};

    let ident = meta.path.get_ident().map(|i| i.to_string());

    match ident.as_deref() {
        Some("node") => {
            if meta.input.peek(syn::Token![=]) {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                }) = value
                {
                    result.node_name = Some(s.value());
                    result.node_name_default = false;
                }
            } else if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
                // node(...) - ignore nested for now
                meta.parse_nested_meta(|_| Ok(()))?;
            } else {
                // bare `node` - does NOT use type name for validation
                // This is different from not specifying `node` at all
                result.node_name_default = false;
            }
        }
        Some("rename_all") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                result.rename_all =
                    parse_rename_strategy(&s.value()).unwrap_or(super::types::RenameStrategy::None);
                result.rename_all_explicit = true;
            }
        }
        Some("default_placement") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                result.default_placement = parse_default_placement(&s.value());
            }
        }
        Some("default_bool") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                result.default_bool = parse_bool_mode(&s.value());
            }
        }
        Some("default_flag_style") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                result.default_flag_style = parse_flag_style(&s.value());
            }
        }
        Some("default_conflict") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                result.default_conflict = parse_conflict_policy(&s.value());
            }
        }
        Some("deny_unknown") => {
            result.deny_unknown = Some(true);
        }
        Some("schema") => {
            if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
                meta.parse_nested_meta(|nested| {
                    parse_struct_schema_meta(&nested, &mut result.schema)
                })?;
            }
        }
        // Ignored at struct level
        Some("choice") | Some("value") | Some("meta") | Some("group") => {
            if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
                if meta.input.peek(syn::Token![=]) {
                    let _: Expr = meta.value()?.parse()?;
                } else {
                    meta.parse_nested_meta(|_| Ok(()))?;
                }
            }
        }
        _ => {}
    }

    Ok(())
}

/// Parse struct schema meta items.
fn parse_struct_schema_meta(
    meta: &syn::meta::ParseNestedMeta,
    schema: &mut super::container::StructSchemaOverride,
) -> syn::Result<()> {
    use syn::{Expr, ExprLit, Lit};

    let ident = meta.path.get_ident().map(|i| i.to_string());

    match ident.as_deref() {
        Some("name") | Some("rename") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                schema.name = Some(s.value());
            }
        }
        Some("description") | Some("desc") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                schema.description = Some(s.value());
            }
        }
        Some("deny_unknown") => {
            schema.deny_unknown = Some(if meta.input.peek(syn::Token![=]) {
                let lit: syn::LitBool = meta.value()?.parse()?;
                lit.value()
            } else {
                true
            });
        }
        Some("allow_unknown") => {
            schema.allow_unknown = Some(if meta.input.peek(syn::Token![=]) {
                let lit: syn::LitBool = meta.value()?.parse()?;
                lit.value()
            } else {
                true
            });
        }
        _ => {}
    }

    Ok(())
}

fn parse_rename_strategy(s: &str) -> Option<super::types::RenameStrategy> {
    use super::types::RenameStrategy;
    match s {
        "kebab-case" => Some(RenameStrategy::KebabCase),
        "snake_case" => Some(RenameStrategy::SnakeCase),
        "lowercase" => Some(RenameStrategy::Lowercase),
        "UPPERCASE" => Some(RenameStrategy::Uppercase),
        "none" => Some(RenameStrategy::None),
        _ => None,
    }
}

fn parse_default_placement(s: &str) -> Option<super::types::DefaultPlacement> {
    use super::types::DefaultPlacement;
    match s {
        "exhaustive" => Some(DefaultPlacement::Exhaustive),
        "attr" => Some(DefaultPlacement::Attr),
        "value" => Some(DefaultPlacement::Value),
        "child" => Some(DefaultPlacement::Child),
        _ => None,
    }
}

/// Parse field-level attributes from `#[kdl(...)]`.
pub fn parse_field_attrs(field: &Field) -> syn::Result<Option<FieldAttrs>> {
    let mut raw = RawFieldAttrs::default();
    let mut has_kdl_attr = false;
    let mut primary_span = field
        .ident
        .as_ref()
        .map(|i| i.span())
        .unwrap_or_else(|| proc_macro2::Span::call_site());

    for attr in &field.attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }
        has_kdl_attr = true;
        primary_span = attr.bracket_token.span.join();

        // Use manual parsing to handle `schema(kind = ...)` where `type` is a keyword
        attr.parse_nested_meta(|meta| parse_field_meta(&meta, &mut raw))?;
    }

    // Check for serde rename fallback
    let name = if raw.name.is_none() && raw.rename.is_none() && field.ident.is_some() {
        serde_rename_from_attrs(&field.attrs)?
    } else {
        None
    };
    if let Some(n) = name {
        raw.name = Some(n);
    }

    if !has_kdl_attr {
        // Return default attrs for fields without #[kdl(...)]
        return Ok(Some(raw.into_field_attrs(primary_span)?));
    }

    Ok(Some(raw.into_field_attrs(primary_span)?))
}

/// Parse a single field meta item using manual parsing.
/// This handles keywords like `type` that darling can't parse.
fn parse_field_meta(meta: &syn::meta::ParseNestedMeta, raw: &mut RawFieldAttrs) -> syn::Result<()> {
    use syn::{Expr, ExprLit, Lit};

    let ident = meta.path.get_ident().map(|i| i.to_string());

    match ident.as_deref() {
        // Placement flags
        Some("attr") => raw.attr = true,
        Some("keyed") => raw.keyed = true,
        Some("positional") => {
            if meta.input.peek(syn::Token![=]) {
                let value: Expr = meta.value()?.parse()?;
                match value {
                    Expr::Lit(ExprLit {
                        lit: Lit::Int(i), ..
                    }) => {
                        raw.positional =
                            Some(super::field::PositionalArg::Index(i.base10_parse()?));
                    }
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) => {
                        let val = s.value();
                        if val == "*" || val == "rest" {
                            raw.positional = Some(super::field::PositionalArg::Rest);
                        } else {
                            return Err(syn::Error::new_spanned(
                                s,
                                format!("expected integer or \"rest\"/\"*\", got \"{}\"", val),
                            ));
                        }
                    }
                    _ => {
                        return Err(syn::Error::new_spanned(
                            value,
                            "positional index must be an integer or \"rest\"/\"*\"",
                        ));
                    }
                }
            } else {
                raw.positional = Some(super::field::PositionalArg::Rest);
            }
        }
        Some("positional_list") => raw.positional_list = true,
        Some("flag") => {
            if meta.input.peek(syn::Token![=]) {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                }) = value
                {
                    raw.flag = Some(super::field::FlagAttr(Some(s.value())));
                }
            } else {
                raw.flag = Some(super::field::FlagAttr(None));
            }
        }
        Some("neg_flag") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                raw.neg_flag = Some(s.value());
            }
        }
        Some("value") => raw.value = true,
        Some("child") => raw.child = true,
        Some("children") => raw.children = true,
        Some("children_any") | Some("choice") => raw.children_any = true,
        Some("registry") => raw.registry = true,
        Some("modifier") => raw.modifier = true,

        // Configuration
        Some("name") | Some("rename") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                raw.name = Some(s.value());
            }
        }
        Some("container") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                raw.container = Some(s.value());
            }
        }
        Some("children_map") => raw.children_map = true,
        Some("map_node") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                raw.map_node = Some(s.value());
            }
        }
        Some("flatten") => {
            raw.flatten = Some(if meta.input.peek(syn::Token![=]) {
                let lit: syn::LitBool = meta.value()?.parse()?;
                lit.value()
            } else {
                true
            });
        }
        Some("required") => raw.required = true,
        Some("optional") => raw.optional = true,

        // Registry keys
        Some("key_arg") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Int(i), ..
            }) = value
            {
                raw.key_arg = Some(i.base10_parse()?);
            }
        }
        Some("key_attr") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                raw.key_attr = Some(s.value());
            }
        }
        Some("key_fn") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                raw.key_fn = Some(s.value());
            }
        }

        // Defaults
        Some("default") => {
            if meta.input.peek(syn::Token![=]) {
                let _ = meta.input.parse::<syn::Token![=]>()?;
                let expr: Expr = meta.input.parse()?;
                match parse_default_expr(&expr) {
                    Some(lit) => raw.default = super::field::DefaultAttr::Literal(lit),
                    None => {
                        return Err(syn::Error::new_spanned(
                            expr,
                            "default value must be a literal (integer, float, boolean, or string)",
                        ));
                    }
                }
            } else {
                raw.default = super::field::DefaultAttr::Derive;
            }
        }
        Some("default_fn") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                raw.default_fn = Some(s.value());
            }
        }

        // Skip
        Some("skip") => raw.skip = true,
        Some("skip_serializing_if") => {
            if meta.input.peek(syn::Token![=]) {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                }) = value
                {
                    raw.skip_serializing_if = Some(s.value());
                }
            }
        }

        // Mode overrides
        Some("bool") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                raw.bool_mode = parse_bool_mode(&s.value());
            }
        }
        Some("flag_style") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                raw.flag_style = parse_flag_style(&s.value());
            }
        }
        Some("conflict") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                raw.conflict = parse_conflict_policy(&s.value());
            }
        }
        Some("render") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                let val = s.value();
                match parse_render_placement(&val) {
                    Some(placement) => raw.render = Some(placement),
                    None => {
                        return Err(syn::Error::new_spanned(
                            s,
                            format!(
                                "invalid render placement \"{}\", expected one of: attr, value, child, children, registry",
                                val
                            ),
                        ));
                    }
                }
            }
        }

        // Type hints
        Some("scalar") | Some("value_type") | Some("value_like") | Some("kdl_value") => {
            raw.scalar = if meta.input.peek(syn::Token![=]) {
                let lit: syn::LitBool = meta.value()?.parse()?;
                lit.value()
            } else {
                true
            };
        }

        // Schema - handle specially because `type` is a keyword
        Some("schema") => {
            if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
                meta.parse_nested_meta(|nested| parse_schema_meta(&nested, &mut raw.schema))?;
            }
        }

        // Meta/group - for recursive attribute groups
        Some("meta") | Some("group") => {
            if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
                meta.parse_nested_meta(|nested| parse_field_meta(&nested, raw))?;
            }
        }

        Some(name) => {
            return Err(syn::Error::new_spanned(
                &meta.path,
                format!("unknown kdl attribute: `{}`", name),
            ));
        }

        None => {
            return Err(syn::Error::new_spanned(
                &meta.path,
                "expected identifier for kdl attribute",
            ));
        }
    }

    Ok(())
}

/// Parse schema meta items (handles `type` keyword specially).
fn parse_schema_meta(
    meta: &syn::meta::ParseNestedMeta,
    schema: &mut super::field::FieldSchemaOverride,
) -> syn::Result<()> {
    use syn::{Expr, ExprLit, Lit};

    let ident = meta.path.get_ident().map(|i| i.to_string());

    match ident.as_deref() {
        Some("skip") => schema.skip = true,
        Some("name") | Some("rename") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                schema.name = Some(s.value());
            }
        }
        Some("kind") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                schema.kind = parse_schema_type(&s.value());
            }
        }
        Some("required") => {
            schema.required = Some(if meta.input.peek(syn::Token![=]) {
                let lit: syn::LitBool = meta.value()?.parse()?;
                lit.value()
            } else {
                true
            });
        }
        Some("optional") => {
            schema.optional = Some(if meta.input.peek(syn::Token![=]) {
                let lit: syn::LitBool = meta.value()?.parse()?;
                lit.value()
            } else {
                true
            });
        }
        Some("description") | Some("desc") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                schema.description = Some(s.value());
            }
        }
        _ => {}
    }

    Ok(())
}

// Helper functions for parsing enum values
fn parse_bool_mode(s: &str) -> Option<super::types::BoolMode> {
    use super::types::BoolMode;
    match s {
        "presence+value" => Some(BoolMode::PresenceAndValue),
        "value-only" => Some(BoolMode::ValueOnly),
        "presence-only" => Some(BoolMode::PresenceOnly),
        _ => None,
    }
}

fn parse_flag_style(s: &str) -> Option<super::types::FlagStyle> {
    use super::types::FlagStyle;
    match s {
        "both" => Some(FlagStyle::Both),
        "value|no" => Some(FlagStyle::ValueNo),
        "with|without" => Some(FlagStyle::WithWithout),
        _ => None,
    }
}

fn parse_conflict_policy(s: &str) -> Option<super::types::ConflictPolicy> {
    use super::types::ConflictPolicy;
    match s {
        "error" => Some(ConflictPolicy::Error),
        "first" => Some(ConflictPolicy::First),
        "last" => Some(ConflictPolicy::Last),
        "append" => Some(ConflictPolicy::Append),
        _ => None,
    }
}

fn parse_render_placement(s: &str) -> Option<super::types::RenderPlacement> {
    use super::types::RenderPlacement;
    match s {
        "attr" => Some(RenderPlacement::Attr),
        "value" => Some(RenderPlacement::Value),
        "child" => Some(RenderPlacement::Child),
        "children" => Some(RenderPlacement::Children),
        "registry" => Some(RenderPlacement::Registry),
        _ => None,
    }
}

fn parse_schema_type(s: &str) -> Option<super::types::SchemaTypeOverride> {
    use super::types::SchemaTypeOverride;
    match s {
        "string" => Some(SchemaTypeOverride::String),
        "integer" | "int" => Some(SchemaTypeOverride::Integer),
        "float" | "number" => Some(SchemaTypeOverride::Float),
        "boolean" | "bool" => Some(SchemaTypeOverride::Boolean),
        "null" => Some(SchemaTypeOverride::Null),
        _ => None,
    }
}

fn parse_default_expr(expr: &syn::Expr) -> Option<super::types::DefaultLiteral> {
    use super::types::DefaultLiteral;
    use syn::{Expr, ExprLit, ExprUnary, Lit, UnOp};

    match expr {
        Expr::Lit(ExprLit { lit, .. }) => match lit {
            Lit::Int(i) => i.base10_parse().ok().map(DefaultLiteral::Int),
            Lit::Float(f) => f.base10_parse().ok().map(DefaultLiteral::Float),
            Lit::Bool(b) => Some(DefaultLiteral::Bool(b.value())),
            Lit::Str(s) => Some(DefaultLiteral::String(s.value())),
            _ => None,
        },
        Expr::Unary(ExprUnary {
            op: UnOp::Neg(_),
            expr,
            ..
        }) => {
            if let Expr::Lit(ExprLit { lit, .. }) = &**expr {
                match lit {
                    Lit::Int(i) => i
                        .base10_parse::<i128>()
                        .ok()
                        .map(|v| DefaultLiteral::Int(-v)),
                    Lit::Float(f) => f
                        .base10_parse::<f64>()
                        .ok()
                        .map(|v| DefaultLiteral::Float(-v)),
                    _ => None,
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

// ============================================================================
// Serde compatibility helpers
// ============================================================================

/// Extract `rename_all` from `#[serde(rename_all = "...")]`.
pub fn serde_rename_all_from_attrs(attrs: &[Attribute]) -> syn::Result<Option<RenameStrategy>> {
    let mut rename_all: Option<RenameStrategy> = None;

    for attr in attrs {
        if !attr.path().is_ident("serde") {
            continue;
        }

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("rename_all") {
                if let Some(value) = parse_serde_string_value(meta)? {
                    if let Some(strategy) = serde_rename_strategy(&value) {
                        rename_all = Some(strategy);
                    }
                }
            }
            Ok(())
        })?;
    }

    Ok(rename_all)
}

/// Extract `rename` from `#[serde(rename = "...")]`.
pub fn serde_rename_from_attrs(attrs: &[Attribute]) -> syn::Result<Option<String>> {
    let mut rename: Option<String> = None;

    for attr in attrs {
        if !attr.path().is_ident("serde") {
            continue;
        }

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("rename") {
                if let Some(value) = parse_serde_string_value(meta)? {
                    rename.get_or_insert(value);
                }
            }
            Ok(())
        })?;
    }

    Ok(rename)
}

fn parse_serde_string_value(meta: syn::meta::ParseNestedMeta) -> syn::Result<Option<String>> {
    use syn::{Expr, ExprLit, Lit};

    if meta.input.peek(syn::Token![=]) {
        let value: Expr = meta.value()?.parse()?;
        if let Expr::Lit(ExprLit {
            lit: Lit::Str(lit), ..
        }) = value
        {
            return Ok(Some(lit.value()));
        }
        return Ok(None);
    }

    if meta.input.is_empty() {
        return Ok(None);
    }

    let mut value: Option<String> = None;
    meta.parse_nested_meta(|m| {
        let is_serialize = m.path.is_ident("serialize");
        let is_deserialize = m.path.is_ident("deserialize");
        if is_serialize || is_deserialize {
            let rename = parse_serde_string_value(m)?;
            if let Some(rename) = rename {
                if value.is_none() || is_serialize {
                    value = Some(rename);
                }
            }
        }
        Ok(())
    })?;

    Ok(value)
}

fn serde_rename_strategy(value: &str) -> Option<RenameStrategy> {
    match value {
        "kebab-case" => Some(RenameStrategy::KebabCase),
        "snake_case" => Some(RenameStrategy::SnakeCase),
        "lowercase" => Some(RenameStrategy::Lowercase),
        "UPPERCASE" => Some(RenameStrategy::Uppercase),
        "none" => Some(RenameStrategy::None),
        _ => None,
    }
}
