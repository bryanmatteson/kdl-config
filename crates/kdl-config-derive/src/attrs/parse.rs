//! Attribute parsing functions.
//!
//! Uses manual `parse_nested_meta` for consistent parsing of all KDL attributes.

use syn::spanned::Spanned;
use syn::{Attribute, Field};

use super::container::StructAttrs;
use super::field::{FieldAttrs, RawFieldAttrs};
use super::types::{InjectOpt, RenameStrategy, SelectOpts, SelectSpec, SelectorAst};

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
                match parse_rename_strategy(&s.value()) {
                    Some(strategy) => {
                        result.rename_all = strategy;
                        result.rename_all_explicit = true;
                    }
                    None => {
                        return Err(syn::Error::new_spanned(
                            s,
                            "invalid rename_all value, expected one of: kebab-case, snake_case, lowercase, UPPERCASE, none",
                        ));
                    }
                }
            }
        }
        Some("default_placement") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                result.default_placement = match parse_default_placement(&s.value()) {
                    Some(placement) => Some(placement),
                    None => {
                        return Err(syn::Error::new_spanned(
                            s,
                            "invalid default_placement, expected one of: exhaustive, attr, value, child",
                        ));
                    }
                };
            }
        }
        Some("default_bool") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                result.default_bool = match parse_bool_mode(&s.value()) {
                    Some(mode) => Some(mode),
                    None => {
                        return Err(syn::Error::new_spanned(
                            s,
                            "invalid default_bool, expected one of: presence+value, value-only, presence-only",
                        ));
                    }
                };
            }
        }
        Some("default_flag_style") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                result.default_flag_style = match parse_flag_style(&s.value()) {
                    Some(style) => Some(style),
                    None => {
                        return Err(syn::Error::new_spanned(
                            s,
                            "invalid default_flag_style, expected one of: both, value|no, with|without",
                        ));
                    }
                };
            }
        }
        Some("default_conflict") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                result.default_conflict = match parse_conflict_policy(&s.value()) {
                    Some(policy) => Some(policy),
                    None => {
                        return Err(syn::Error::new_spanned(
                            s,
                            "invalid default_conflict, expected one of: error, first, last, append",
                        ));
                    }
                };
            }
        }
        Some("skip_serialize_none") => {
            result.skip_serialize_none = Some(if meta.input.peek(syn::Token![=]) {
                let lit: syn::LitBool = meta.value()?.parse()?;
                lit.value()
            } else {
                true
            });
        }
        Some("skip_serialize_empty_collections") => {
            result.skip_serialize_empty_collections = Some(if meta.input.peek(syn::Token![=]) {
                let lit: syn::LitBool = meta.value()?.parse()?;
                lit.value()
            } else {
                true
            });
        }
        Some("selector") => {
            let value: syn::Meta = meta.value()?.parse()?;
            let selector = parse_selector_meta(&value)?;
            if result.selector.is_some() || result.selector_spec.is_some() {
                return Err(syn::Error::new(
                    meta.path.span(),
                    "selector is already specified for this enum",
                ));
            }
            result.selector = Some(selector);
        }
        Some("consume") => {
            if result.selector_opts.consume.is_some() {
                return Err(syn::Error::new(
                    meta.path.span(),
                    "consume/preserve already specified for this enum",
                ));
            }
            result.selector_opts.consume = Some(true);
        }
        Some("preserve") => {
            if result.selector_opts.consume.is_some() {
                return Err(syn::Error::new(
                    meta.path.span(),
                    "consume/preserve already specified for this enum",
                ));
            }
            result.selector_opts.consume = Some(false);
        }
        Some("select") => {
            let spec = parse_select_spec(meta)?;
            if spec.opts.inject.is_some() {
                return Err(syn::Error::new(
                    meta.path.span(),
                    "select(...) inject is not supported on enums",
                ));
            }
            if result.selector.is_some() || result.selector_spec.is_some() {
                return Err(syn::Error::new(
                    meta.path.span(),
                    "selector is already specified for this enum",
                ));
            }
            result.selector_spec = Some(spec);
        }
        Some("deny_unknown") => {
            result.deny_unknown = Some(true);
        }
        Some("validate") => {
            if meta.input.peek(syn::Token![=]) {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                }) = value
                {
                    let rules = parse_validation_dsl_str(&s.value(), s.span())?;
                    result.validations.extend(rules);
                }
            } else if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
                meta.parse_nested_meta(|nested| {
                    let rule = parse_validation_rule_meta(&nested)?;
                    result.validations.push(rule);
                    Ok(())
                })?;
            }
        }
        Some("post_decode") => {
            if meta.input.peek(syn::Token![=]) {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                }) = value
                {
                    result.post_decode = Some(s.value());
                }
            } else if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
                meta.parse_nested_meta(|nested| {
                    if nested.path.is_ident("func") {
                        let value: Expr = nested.value()?.parse()?;
                        if let Expr::Lit(ExprLit {
                            lit: Lit::Str(s), ..
                        }) = value
                        {
                            result.post_decode = Some(s.value());
                        }
                    }
                    Ok(())
                })?;
            }
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
        Some("validate") => {
            if meta.input.peek(syn::Token![=]) {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                }) = value
                {
                    let rules = parse_validation_dsl_str(&s.value(), s.span())?;
                    schema.validations.extend(rules);
                }
            } else if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
                meta.parse_nested_meta(|nested| {
                    let rule = parse_validation_rule_meta(&nested)?;
                    schema.validations.push(rule);
                    Ok(())
                })?;
            }
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
/// This handles keywords like `type` which is a Rust keyword.
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
            let names = parse_string_or_any_names_expr(value, meta.path.span(), "name")?;
            if let Some(first) = names.first() {
                raw.name = Some(first.clone());
            }
            if names.len() > 1 {
                raw.aliases.extend(names.into_iter().skip(1));
            }
        }
        Some("alias") => {
            let value: Expr = meta.value()?.parse()?;
            let names = parse_string_or_any_names_expr(value, meta.path.span(), "alias")?;
            raw.aliases.extend(names);
        }
        Some("aliases") => {
            let value: Expr = meta.value()?.parse()?;
            let names = parse_string_or_any_names_expr(value, meta.path.span(), "aliases")?;
            raw.aliases.extend(names);
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
        Some("path") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                raw.path = Some(super::field::FieldPath::parse(&s.value(), s.span())?);
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

        // Selectors
        Some("selector") => {
            let value: syn::Meta = meta.value()?.parse()?;
            let selector = parse_selector_meta(&value)?;
            if raw.selector.is_some() {
                return Err(syn::Error::new_spanned(
                    value,
                    "selector is already specified for this field",
                ));
            }
            raw.selector = Some(selector);
        }
        Some("select") => {
            if raw.select.is_some() {
                return Err(syn::Error::new(
                    meta.path.span(),
                    "select(...) is already specified for this field",
                ));
            }
            let spec = parse_select_spec(meta)?;
            raw.select = Some(spec);
        }
        Some("consume") => {
            if raw.consume.is_some() {
                return Err(syn::Error::new(
                    meta.path.span(),
                    "consume/preserve already specified for this field",
                ));
            }
            raw.consume = Some(true);
        }
        Some("preserve") => {
            if raw.consume.is_some() {
                return Err(syn::Error::new(
                    meta.path.span(),
                    "consume/preserve already specified for this field",
                ));
            }
            raw.consume = Some(false);
        }

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
        Some("no_skip_serialize") => {
            raw.no_skip_serialize = if meta.input.peek(syn::Token![=]) {
                let lit: syn::LitBool = meta.value()?.parse()?;
                lit.value()
            } else {
                true
            };
        }
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
                raw.bool_mode = match parse_bool_mode(&s.value()) {
                    Some(mode) => Some(mode),
                    None => {
                        return Err(syn::Error::new_spanned(
                            s,
                            "invalid bool mode, expected one of: presence+value, value-only, presence-only",
                        ));
                    }
                };
            }
        }
        Some("flag_style") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                raw.flag_style = match parse_flag_style(&s.value()) {
                    Some(style) => Some(style),
                    None => {
                        return Err(syn::Error::new_spanned(
                            s,
                            "invalid flag_style, expected one of: both, value|no, with|without",
                        ));
                    }
                };
            }
        }
        Some("conflict") => {
            let value: Expr = meta.value()?.parse()?;
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                raw.conflict = match parse_conflict_policy(&s.value()) {
                    Some(policy) => Some(policy),
                    None => {
                        return Err(syn::Error::new_spanned(
                            s,
                            "invalid conflict policy, expected one of: error, first, last, append",
                        ));
                    }
                };
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

        // Validation
        Some("validate") => {
            if meta.input.peek(syn::Token![=]) {
                // String DSL: #[kdl(validate = "range(1, 65535) non_empty")]
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                }) = value
                {
                    let rules = parse_validation_dsl_str(&s.value(), s.span())?;
                    raw.validations.extend(rules);
                }
            } else if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
                // Nested meta: #[kdl(validate(range(1, 65535), non_empty))]
                meta.parse_nested_meta(|nested| {
                    let rule = parse_validation_rule_meta(&nested)?;
                    raw.validations.push(rule);
                    Ok(())
                })?;
            }
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

fn parse_string_or_any_names_expr(
    value: syn::Expr,
    span: proc_macro2::Span,
    label: &str,
) -> syn::Result<Vec<String>> {
    use syn::{Expr, ExprCall, ExprLit, ExprPath, Lit};

    match value {
        Expr::Lit(ExprLit {
            lit: Lit::Str(s), ..
        }) => Ok(vec![s.value()]),
        Expr::Call(ExprCall { func, args, .. }) => {
            let is_any =
                matches!(*func, Expr::Path(ExprPath { ref path, .. }) if path.is_ident("any"));
            if !is_any {
                return Err(syn::Error::new(
                    span,
                    format!("{label} requires a string literal or any(\"a\", \"b\", ...)"),
                ));
            }

            if args.is_empty() {
                return Err(syn::Error::new(
                    span,
                    format!("{label} any(...) requires at least one string literal"),
                ));
            }

            let mut names = Vec::with_capacity(args.len());
            for arg in args {
                match arg {
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) => names.push(s.value()),
                    _ => {
                        return Err(syn::Error::new(
                            span,
                            format!("{label} any(...) expects only string literals"),
                        ));
                    }
                }
            }

            Ok(names)
        }
        _ => Err(syn::Error::new(
            span,
            format!("{label} requires a string literal or any(\"a\", \"b\", ...)"),
        )),
    }
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
                schema.kind = match parse_schema_type(&s.value()) {
                    Some(kind) => Some(kind),
                    None => {
                        return Err(syn::Error::new_spanned(
                            s,
                            "invalid schema kind, expected one of: string, integer, int, float, number, boolean, bool, null",
                        ));
                    }
                };
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
        Some("validate") => {
            if meta.input.peek(syn::Token![=]) {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                }) = value
                {
                    let rules = parse_validation_dsl_str(&s.value(), s.span())?;
                    schema.validations.extend(rules);
                }
            } else if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
                meta.parse_nested_meta(|nested| {
                    let rule = parse_validation_rule_meta(&nested)?;
                    schema.validations.push(rule);
                    Ok(())
                })?;
            }
        }
        _ => {}
    }

    Ok(())
}

fn parse_select_spec(meta: &syn::meta::ParseNestedMeta) -> syn::Result<SelectSpec> {
    use syn::parse::discouraged::Speculative;

    // DEBUG TEMP
    // return Err(syn::Error::new(
    //     meta.path.span(),
    //     format!("select tokens: {}", meta.input.to_string()),
    // ));

    let fork = meta.input.fork();
    if let Ok(spec) = parse_select_spec_meta(&fork) {
        meta.input.advance_to(&fork);
        return Ok(spec);
    }

    let fork = meta.input.fork();
    if let Ok(spec) = parse_select_spec_expr(&fork) {
        meta.input.advance_to(&fork);
        return Ok(spec);
    }

    let fork = meta.input.fork();
    let err = parse_select_spec_meta(&fork).unwrap_err();
    Err(err)
}

fn parse_select_spec_meta(input: syn::parse::ParseStream) -> syn::Result<SelectSpec> {
    use syn::parse::Parse;
    use syn::{Meta, Token, parenthesized, parse::ParseStream};

    if input.peek(syn::Token![=]) {
        let _eq: syn::Token![=] = input.parse()?;
        let value: syn::Meta = input.parse()?;
        let selector = parse_selector_meta(&value)?;
        return Ok(SelectSpec {
            selector,
            opts: SelectOpts::default(),
        });
    }

    fn parse_meta_list(input: ParseStream) -> syn::Result<Vec<Meta>> {
        Ok(input
            .parse_terminated(Meta::parse, Token![,])?
            .into_iter()
            .collect::<Vec<_>>())
    }

    let args = if input.peek(syn::token::Paren) {
        let content;
        parenthesized!(content in input);
        parse_meta_list(&content)?
    } else {
        parse_meta_list(input)?
    };

    parse_select_spec_from_meta(args, input.span())
}

fn parse_select_spec_expr(input: syn::parse::ParseStream) -> syn::Result<SelectSpec> {
    use syn::parse::Parse;
    use syn::{Expr, Token, parenthesized, parse::ParseStream};

    if input.peek(syn::Token![=]) {
        let _eq: syn::Token![=] = input.parse()?;
        let value: Expr = input.parse()?;
        let selector = parse_selector_expr(&value)?;
        return Ok(SelectSpec {
            selector,
            opts: SelectOpts::default(),
        });
    }

    fn parse_expr_list(input: ParseStream) -> syn::Result<Vec<Expr>> {
        Ok(input
            .parse_terminated(Expr::parse, Token![,])?
            .into_iter()
            .collect::<Vec<_>>())
    }

    let args = if input.peek(syn::token::Paren) {
        let content;
        parenthesized!(content in input);
        parse_expr_list(&content)?
    } else {
        parse_expr_list(input)?
    };

    parse_select_spec_from_exprs(args, input.span())
}

fn parse_select_spec_from_meta(
    args: Vec<syn::Meta>,
    span: proc_macro2::Span,
) -> syn::Result<SelectSpec> {
    use syn::Meta;

    if args.is_empty() {
        return Err(syn::Error::new(span, "select(...) requires a selector"));
    }

    let mut selector: Option<SelectorAst> = None;
    let mut opts = SelectOpts::default();

    for meta in args {
        match meta {
            Meta::Path(path) if path.is_ident("consume") => {
                if opts.consume.is_some() {
                    return Err(syn::Error::new(
                        path.span(),
                        "consume/preserve already specified",
                    ));
                }
                opts.consume = Some(true);
            }
            Meta::Path(path) if path.is_ident("preserve") => {
                if opts.consume.is_some() {
                    return Err(syn::Error::new(
                        path.span(),
                        "consume/preserve already specified",
                    ));
                }
                opts.consume = Some(false);
            }
            Meta::Path(path) if path.is_ident("inject") => {
                if opts.inject.is_some() {
                    return Err(syn::Error::new(path.span(), "inject already specified"));
                }
                opts.inject = Some(InjectOpt::Implicit);
            }
            Meta::NameValue(nv) if nv.path.is_ident("inject") => {
                if opts.inject.is_some() {
                    return Err(syn::Error::new(nv.path.span(), "inject already specified"));
                }
                let field = match nv.value {
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(s),
                        ..
                    }) => s.value(),
                    other => {
                        return Err(syn::Error::new_spanned(
                            other,
                            "inject must be a string literal",
                        ));
                    }
                };
                opts.inject = Some(InjectOpt::Field(field));
            }
            other => {
                let selector_ast = parse_selector_meta(&other)?;
                if selector.is_some() {
                    return Err(syn::Error::new_spanned(
                        other,
                        "multiple selectors in select(...)",
                    ));
                }
                selector = Some(selector_ast);
            }
        }
    }

    let selector =
        selector.ok_or_else(|| syn::Error::new(span, "select(...) requires a selector"))?;

    Ok(SelectSpec { selector, opts })
}

fn parse_select_spec_from_exprs(
    args: Vec<syn::Expr>,
    span: proc_macro2::Span,
) -> syn::Result<SelectSpec> {
    use syn::{Expr, ExprAssign, ExprPath, Lit};

    if args.is_empty() {
        return Err(syn::Error::new(span, "select(...) requires a selector"));
    }

    let mut selector: Option<SelectorAst> = None;
    let mut opts = SelectOpts::default();

    for expr in args {
        match expr {
            Expr::Path(ExprPath { path, .. }) if path.is_ident("consume") => {
                if opts.consume.is_some() {
                    return Err(syn::Error::new(
                        path.span(),
                        "consume/preserve already specified",
                    ));
                }
                opts.consume = Some(true);
            }
            Expr::Path(ExprPath { path, .. }) if path.is_ident("preserve") => {
                if opts.consume.is_some() {
                    return Err(syn::Error::new(
                        path.span(),
                        "consume/preserve already specified",
                    ));
                }
                opts.consume = Some(false);
            }
            Expr::Path(ExprPath { path, .. }) if path.is_ident("inject") => {
                if opts.inject.is_some() {
                    return Err(syn::Error::new(path.span(), "inject already specified"));
                }
                opts.inject = Some(InjectOpt::Implicit);
            }
            Expr::Assign(ExprAssign { left, right, .. }) => {
                let (left_ident, left_span) = match left.as_ref() {
                    Expr::Path(p) => (p.path.get_ident().map(|i| i.to_string()), p.path.span()),
                    other => {
                        return Err(syn::Error::new_spanned(other, "invalid select option"));
                    }
                };
                if matches!(left_ident.as_deref(), Some("inject")) {
                    if opts.inject.is_some() {
                        return Err(syn::Error::new(left_span, "inject already specified"));
                    }
                    let field = match *right {
                        Expr::Lit(syn::ExprLit {
                            lit: Lit::Str(s), ..
                        }) => s.value(),
                        other => {
                            return Err(syn::Error::new_spanned(
                                other,
                                "inject must be a string literal",
                            ));
                        }
                    };
                    opts.inject = Some(InjectOpt::Field(field));
                } else {
                    let selector_expr = Expr::Assign(ExprAssign {
                        attrs: Vec::new(),
                        left,
                        right,
                        eq_token: <syn::Token![=]>::default(),
                    });
                    let selector_ast = parse_selector_expr(&selector_expr)?;
                    if selector.is_some() {
                        return Err(syn::Error::new(
                            left_span,
                            "multiple selectors in select(...)",
                        ));
                    }
                    selector = Some(selector_ast);
                }
            }
            other => {
                let selector_ast = parse_selector_expr(&other)?;
                if selector.is_some() {
                    return Err(syn::Error::new_spanned(
                        other,
                        "multiple selectors in select(...)",
                    ));
                }
                selector = Some(selector_ast);
            }
        }
    }

    let selector =
        selector.ok_or_else(|| syn::Error::new(span, "select(...) requires a selector"))?;

    Ok(SelectSpec { selector, opts })
}

fn parse_selector_meta(meta: &syn::Meta) -> syn::Result<SelectorAst> {
    use syn::punctuated::Punctuated;
    use syn::{Lit, Meta, MetaList, MetaNameValue, Token};

    fn parse_selector_name_value(nv: &MetaNameValue) -> syn::Result<SelectorAst> {
        let ident = nv.path.get_ident().map(|i| i.to_string());
        match ident.as_deref() {
            Some("arg") => match &nv.value {
                syn::Expr::Lit(syn::ExprLit {
                    lit: Lit::Int(lit), ..
                }) => Ok(SelectorAst::Arg(lit.base10_parse()?)),
                other => Err(syn::Error::new_spanned(
                    other,
                    "arg selector requires an integer literal",
                )),
            },
            Some("attr") => match &nv.value {
                syn::Expr::Lit(syn::ExprLit {
                    lit: Lit::Str(lit), ..
                }) => Ok(SelectorAst::Attr(lit.value())),
                other => Err(syn::Error::new_spanned(
                    other,
                    "attr selector requires a string literal",
                )),
            },
            Some("func") => match &nv.value {
                syn::Expr::Lit(syn::ExprLit {
                    lit: Lit::Str(lit), ..
                }) => Ok(SelectorAst::Func(lit.value())),
                other => Err(syn::Error::new_spanned(
                    other,
                    "func selector requires a string literal",
                )),
            },
            Some("name") => Err(syn::Error::new_spanned(
                &nv.path,
                "name() selector does not take arguments",
            )),
            _ => Err(syn::Error::new_spanned(
                &nv.path,
                "unknown selector assignment",
            )),
        }
    }

    fn parse_selector_list(list: &MetaList) -> syn::Result<SelectorAst> {
        let ident = list.path.get_ident().map(|i| i.to_string());
        match ident.as_deref() {
            Some("arg") => {
                if let Ok(lit) = list.parse_args::<syn::LitInt>() {
                    return Ok(SelectorAst::Arg(lit.base10_parse()?));
                }
                let nested: Punctuated<Meta, Token![,]> =
                    list.parse_args_with(Punctuated::parse_terminated)?;
                if nested.len() != 1 {
                    return Err(syn::Error::new_spanned(
                        list,
                        "arg(...) expects a single integer",
                    ));
                }
                match &nested[0] {
                    Meta::NameValue(nv) if nv.path.is_ident("index") || nv.path.is_ident("arg") => {
                        match &nv.value {
                            syn::Expr::Lit(syn::ExprLit {
                                lit: Lit::Int(lit), ..
                            }) => Ok(SelectorAst::Arg(lit.base10_parse()?)),
                            other => Err(syn::Error::new_spanned(
                                other,
                                "arg(...) expects an integer literal",
                            )),
                        }
                    }
                    other => Err(syn::Error::new_spanned(
                        other,
                        "arg(...) expects a single integer",
                    )),
                }
            }
            Some("attr") => {
                if let Ok(lit) = list.parse_args::<syn::LitStr>() {
                    return Ok(SelectorAst::Attr(lit.value()));
                }
                let nested: Punctuated<Meta, Token![,]> =
                    list.parse_args_with(Punctuated::parse_terminated)?;
                if nested.len() != 1 {
                    return Err(syn::Error::new_spanned(
                        list,
                        "attr(...) expects a single string",
                    ));
                }
                match &nested[0] {
                    Meta::NameValue(nv) if nv.path.is_ident("name") || nv.path.is_ident("key") => {
                        match &nv.value {
                            syn::Expr::Lit(syn::ExprLit {
                                lit: Lit::Str(lit), ..
                            }) => Ok(SelectorAst::Attr(lit.value())),
                            other => Err(syn::Error::new_spanned(
                                other,
                                "attr(...) expects a string literal",
                            )),
                        }
                    }
                    other => Err(syn::Error::new_spanned(
                        other,
                        "attr(...) expects a single string",
                    )),
                }
            }
            Some("func") => {
                if let Ok(lit) = list.parse_args::<syn::LitStr>() {
                    return Ok(SelectorAst::Func(lit.value()));
                }
                let nested: Punctuated<Meta, Token![,]> =
                    list.parse_args_with(Punctuated::parse_terminated)?;
                if nested.len() != 1 {
                    return Err(syn::Error::new_spanned(
                        list,
                        "func(...) expects a single string",
                    ));
                }
                match &nested[0] {
                    Meta::NameValue(nv) if nv.path.is_ident("path") || nv.path.is_ident("name") => {
                        match &nv.value {
                            syn::Expr::Lit(syn::ExprLit {
                                lit: Lit::Str(lit), ..
                            }) => Ok(SelectorAst::Func(lit.value())),
                            other => Err(syn::Error::new_spanned(
                                other,
                                "func(...) expects a string literal",
                            )),
                        }
                    }
                    other => Err(syn::Error::new_spanned(
                        other,
                        "func(...) expects a single string",
                    )),
                }
            }
            Some("name") => {
                if !list.tokens.is_empty() {
                    return Err(syn::Error::new_spanned(
                        list,
                        "name() does not take arguments",
                    ));
                }
                Ok(SelectorAst::Name)
            }
            Some("any") => {
                let nested: Punctuated<Meta, Token![,]> =
                    list.parse_args_with(Punctuated::parse_terminated)?;
                if nested.is_empty() {
                    return Err(syn::Error::new_spanned(
                        list,
                        "any(...) requires at least one selector",
                    ));
                }
                let mut selectors = Vec::new();
                for meta in nested {
                    match parse_selector_meta(&meta)? {
                        SelectorAst::Any(nested) => selectors.extend(nested),
                        other => selectors.push(other),
                    }
                }
                Ok(SelectorAst::Any(selectors))
            }
            _ => Err(syn::Error::new_spanned(list, "unknown selector function")),
        }
    }

    match meta {
        Meta::Path(path) if path.is_ident("name") => Ok(SelectorAst::Name),
        Meta::NameValue(nv) => parse_selector_name_value(nv),
        Meta::List(list) => parse_selector_list(list),
        other => Err(syn::Error::new_spanned(
            other,
            "invalid selector expression",
        )),
    }
}

fn parse_selector_expr(expr: &syn::Expr) -> syn::Result<SelectorAst> {
    use syn::{Expr, ExprAssign, ExprCall, ExprLit, ExprPath, Lit};

    match expr {
        Expr::Assign(ExprAssign { left, right, .. }) => {
            let path = match left.as_ref() {
                Expr::Path(ExprPath { path, .. }) => path,
                other => {
                    return Err(syn::Error::new_spanned(
                        other,
                        "selector assignment must be an identifier",
                    ));
                }
            };
            let ident = path.get_ident().map(|i| i.to_string());
            match ident.as_deref() {
                Some("arg") => match right.as_ref() {
                    Expr::Lit(ExprLit {
                        lit: Lit::Int(lit), ..
                    }) => Ok(SelectorAst::Arg(lit.base10_parse()?)),
                    other => Err(syn::Error::new_spanned(
                        other,
                        "arg selector requires an integer literal",
                    )),
                },
                Some("attr") => match right.as_ref() {
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(lit), ..
                    }) => Ok(SelectorAst::Attr(lit.value())),
                    other => Err(syn::Error::new_spanned(
                        other,
                        "attr selector requires a string literal",
                    )),
                },
                Some("func") => match right.as_ref() {
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(lit), ..
                    }) => Ok(SelectorAst::Func(lit.value())),
                    other => Err(syn::Error::new_spanned(
                        other,
                        "func selector requires a string literal",
                    )),
                },
                Some("name") => Err(syn::Error::new_spanned(
                    left,
                    "name() selector does not take arguments",
                )),
                _ => Err(syn::Error::new_spanned(left, "unknown selector assignment")),
            }
        }
        Expr::Call(ExprCall { func, args, .. }) => {
            let path = match &**func {
                Expr::Path(ExprPath { path, .. }) => path,
                other => {
                    return Err(syn::Error::new_spanned(
                        other,
                        "selector function must be an identifier",
                    ));
                }
            };
            let ident = path.get_ident().map(|i| i.to_string());
            match ident.as_deref() {
                Some("arg") => {
                    if args.len() != 1 {
                        return Err(syn::Error::new_spanned(
                            args,
                            "arg(...) expects a single integer",
                        ));
                    }
                    let arg = args.first().unwrap();
                    let idx = match arg {
                        Expr::Lit(ExprLit {
                            lit: Lit::Int(i), ..
                        }) => i.base10_parse::<u32>()?,
                        other => {
                            return Err(syn::Error::new_spanned(
                                other,
                                "arg(...) expects an integer literal",
                            ));
                        }
                    };
                    Ok(SelectorAst::Arg(idx))
                }
                Some("attr") => {
                    if args.len() != 1 {
                        return Err(syn::Error::new_spanned(
                            args,
                            "attr(...) expects a single string",
                        ));
                    }
                    let arg = args.first().unwrap();
                    let name = match arg {
                        Expr::Lit(ExprLit {
                            lit: Lit::Str(s), ..
                        }) => s.value(),
                        other => {
                            return Err(syn::Error::new_spanned(
                                other,
                                "attr(...) expects a string literal",
                            ));
                        }
                    };
                    Ok(SelectorAst::Attr(name))
                }
                Some("name") => {
                    if !args.is_empty() {
                        return Err(syn::Error::new_spanned(
                            args,
                            "name() does not take arguments",
                        ));
                    }
                    Ok(SelectorAst::Name)
                }
                Some("func") => {
                    if args.len() != 1 {
                        return Err(syn::Error::new_spanned(
                            args,
                            "func(...) expects a single string",
                        ));
                    }
                    let arg = args.first().unwrap();
                    let name = match arg {
                        Expr::Lit(ExprLit {
                            lit: Lit::Str(s), ..
                        }) => s.value(),
                        other => {
                            return Err(syn::Error::new_spanned(
                                other,
                                "func(...) expects a string literal",
                            ));
                        }
                    };
                    Ok(SelectorAst::Func(name))
                }
                Some("any") => {
                    if args.is_empty() {
                        return Err(syn::Error::new_spanned(
                            args,
                            "any(...) requires at least one selector",
                        ));
                    }
                    let mut selectors = Vec::new();
                    for arg in args {
                        match parse_selector_expr(&arg)? {
                            SelectorAst::Any(nested) => selectors.extend(nested),
                            other => selectors.push(other),
                        }
                    }
                    Ok(SelectorAst::Any(selectors))
                }
                _ => Err(syn::Error::new_spanned(path, "unknown selector function")),
            }
        }
        Expr::Path(ExprPath { path, .. }) if path.is_ident("name") => Ok(SelectorAst::Name),
        other => Err(syn::Error::new_spanned(
            other,
            "invalid selector expression",
        )),
    }
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
// Validation DSL parsing
// ============================================================================

/// Parse a validation DSL string into rules.
fn parse_validation_dsl_str(
    input: &str,
    span: proc_macro2::Span,
) -> syn::Result<Vec<super::types::ValidationRule>> {
    let input = input.trim();
    if input.is_empty() {
        return Ok(vec![]);
    }

    let mut rules = Vec::new();
    let mut chars = input.chars().peekable();

    while chars.peek().is_some() {
        while chars
            .peek()
            .map_or(false, |c| c.is_whitespace() || *c == ',')
        {
            chars.next();
        }
        if chars.peek().is_none() {
            break;
        }

        let mut ident = String::new();
        while chars
            .peek()
            .map_or(false, |c| c.is_alphanumeric() || *c == '_')
        {
            ident.push(chars.next().unwrap());
        }

        if ident.is_empty() {
            return Err(syn::Error::new(
                span,
                format!("unexpected character in validation DSL: {:?}", chars.peek()),
            ));
        }

        let rule = if chars.peek() == Some(&'(') {
            chars.next();
            let mut depth = 1;
            let mut args = String::new();
            while let Some(c) = chars.next() {
                match c {
                    '(' => {
                        depth += 1;
                        args.push(c);
                    }
                    ')' => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        args.push(c);
                    }
                    _ => args.push(c),
                }
            }
            if depth != 0 {
                return Err(syn::Error::new(
                    span,
                    "unclosed parenthesis in validation DSL",
                ));
            }
            parse_validation_rule_with_args(&ident, &args, span)?
        } else {
            parse_validation_rule_bare(&ident, span)?
        };

        rules.push(rule);
    }

    Ok(rules)
}

fn parse_validation_f64(s: &str, span: proc_macro2::Span) -> syn::Result<f64> {
    s.trim()
        .parse::<f64>()
        .map_err(|_| syn::Error::new(span, format!("expected number, got {:?}", s.trim())))
}

fn parse_validation_usize(s: &str, span: proc_macro2::Span) -> syn::Result<usize> {
    s.trim().parse::<usize>().map_err(|_| {
        syn::Error::new(
            span,
            format!("expected non-negative integer, got {:?}", s.trim()),
        )
    })
}

fn parse_validation_string_arg(s: &str) -> String {
    let s = s.trim();
    if (s.starts_with('"') && s.ends_with('"')) || (s.starts_with('\'') && s.ends_with('\'')) {
        s[1..s.len() - 1].to_string()
    } else {
        s.to_string()
    }
}

fn parse_validation_rule_with_args(
    name: &str,
    args: &str,
    span: proc_macro2::Span,
) -> syn::Result<super::types::ValidationRule> {
    use super::types::ValidationRule;
    match name {
        "min" => Ok(ValidationRule::Min(parse_validation_f64(args, span)?)),
        "max" => Ok(ValidationRule::Max(parse_validation_f64(args, span)?)),
        "range" => {
            let parts: Vec<&str> = args.split(',').collect();
            if parts.len() != 2 {
                return Err(syn::Error::new(
                    span,
                    "range() requires exactly 2 arguments",
                ));
            }
            Ok(ValidationRule::Range(
                parse_validation_f64(parts[0], span)?,
                parse_validation_f64(parts[1], span)?,
            ))
        }
        "multiple_of" => Ok(ValidationRule::MultipleOf(parse_validation_f64(
            args, span,
        )?)),
        "min_len" => Ok(ValidationRule::MinLen(parse_validation_usize(args, span)?)),
        "max_len" => Ok(ValidationRule::MaxLen(parse_validation_usize(args, span)?)),
        "len" => {
            let parts: Vec<&str> = args.split(',').collect();
            if parts.len() != 2 {
                return Err(syn::Error::new(span, "len() requires exactly 2 arguments"));
            }
            Ok(ValidationRule::Len(
                parse_validation_usize(parts[0], span)?,
                parse_validation_usize(parts[1], span)?,
            ))
        }
        "pattern" => Ok(ValidationRule::Pattern(parse_validation_string_arg(args))),
        "min_items" => Ok(ValidationRule::MinItems(parse_validation_usize(
            args, span,
        )?)),
        "max_items" => Ok(ValidationRule::MaxItems(parse_validation_usize(
            args, span,
        )?)),
        "func" => Ok(ValidationRule::Func(parse_validation_string_arg(args))),
        "less_than" | "lt" => Ok(ValidationRule::LessThan(parse_validation_string_arg(args))),
        "less_than_or_equal" | "lte" => Ok(ValidationRule::LessThanOrEqual(
            parse_validation_string_arg(args),
        )),
        "greater_than" | "gt" => Ok(ValidationRule::GreaterThan(parse_validation_string_arg(
            args,
        ))),
        "greater_than_or_equal" | "gte" => Ok(ValidationRule::GreaterThanOrEqual(
            parse_validation_string_arg(args),
        )),
        "equal_to" | "eq" => Ok(ValidationRule::EqualTo(parse_validation_string_arg(args))),
        "not_equal_to" | "neq" => Ok(ValidationRule::NotEqualTo(parse_validation_string_arg(
            args,
        ))),
        "exists_in" => Ok(ValidationRule::ExistsIn(parse_validation_string_arg(args))),
        "subset_of" => Ok(ValidationRule::SubsetOf(parse_validation_string_arg(args))),
        _ => Err(syn::Error::new(
            span,
            format!("unknown validation rule: {}", name),
        )),
    }
}

fn parse_validation_rule_bare(
    name: &str,
    span: proc_macro2::Span,
) -> syn::Result<super::types::ValidationRule> {
    use super::types::ValidationRule;
    match name {
        "positive" => Ok(ValidationRule::Positive),
        "negative" => Ok(ValidationRule::Negative),
        "non_negative" => Ok(ValidationRule::NonNegative),
        "non_positive" => Ok(ValidationRule::NonPositive),
        "non_empty" => Ok(ValidationRule::NonEmpty),
        "ascii" => Ok(ValidationRule::Ascii),
        "alphanumeric" => Ok(ValidationRule::Alphanumeric),
        _ => Err(syn::Error::new(
            span,
            format!("unknown validation rule or missing arguments: {}", name),
        )),
    }
}

/// Parse a single validation rule from nested meta syntax.
/// E.g., `range(1, 65535)` or `non_empty`
fn parse_validation_rule_meta(
    meta: &syn::meta::ParseNestedMeta,
) -> syn::Result<super::types::ValidationRule> {
    use super::types::ValidationRule;
    use syn::parse::Parse;
    use syn::{Expr, ExprLit, Lit};

    let ident = meta
        .path
        .get_ident()
        .map(|i| i.to_string())
        .ok_or_else(|| {
            syn::Error::new_spanned(&meta.path, "expected identifier for validation rule")
        })?;

    // Check for parenthesized arguments: validate(range(1, 65535))
    if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
        if meta.input.peek(syn::token::Paren) {
            // This is a function-style rule with args
            let content;
            syn::parenthesized!(content in meta.input);
            let args: syn::punctuated::Punctuated<Expr, syn::Token![,]> =
                content.parse_terminated(Expr::parse, syn::Token![,])?;

            return parse_validation_rule_from_exprs(&ident, &args, meta.path.span());
        }

        // Check for `= value` style
        if meta.input.peek(syn::Token![=]) {
            let value: Expr = meta.value()?.parse()?;
            match &ident[..] {
                "min" => return Ok(ValidationRule::Min(expr_to_f64(&value)?)),
                "max" => return Ok(ValidationRule::Max(expr_to_f64(&value)?)),
                "multiple_of" => return Ok(ValidationRule::MultipleOf(expr_to_f64(&value)?)),
                "min_len" => return Ok(ValidationRule::MinLen(expr_to_usize(&value)?)),
                "max_len" => return Ok(ValidationRule::MaxLen(expr_to_usize(&value)?)),
                "min_items" => return Ok(ValidationRule::MinItems(expr_to_usize(&value)?)),
                "max_items" => return Ok(ValidationRule::MaxItems(expr_to_usize(&value)?)),
                "pattern" => {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) = value
                    {
                        return Ok(ValidationRule::Pattern(s.value()));
                    }
                    return Err(syn::Error::new(
                        meta.path.span(),
                        "pattern requires a string argument",
                    ));
                }
                "func" => {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) = value
                    {
                        return Ok(ValidationRule::Func(s.value()));
                    }
                    return Err(syn::Error::new(
                        meta.path.span(),
                        "func requires a string argument",
                    ));
                }
                "less_than" | "lt" => {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) = value
                    {
                        return Ok(ValidationRule::LessThan(s.value()));
                    }
                    return Err(syn::Error::new(
                        meta.path.span(),
                        "less_than requires a string field name",
                    ));
                }
                "less_than_or_equal" | "lte" => {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) = value
                    {
                        return Ok(ValidationRule::LessThanOrEqual(s.value()));
                    }
                    return Err(syn::Error::new(
                        meta.path.span(),
                        "less_than_or_equal requires a string field name",
                    ));
                }
                "greater_than" | "gt" => {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) = value
                    {
                        return Ok(ValidationRule::GreaterThan(s.value()));
                    }
                    return Err(syn::Error::new(
                        meta.path.span(),
                        "greater_than requires a string field name",
                    ));
                }
                "greater_than_or_equal" | "gte" => {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) = value
                    {
                        return Ok(ValidationRule::GreaterThanOrEqual(s.value()));
                    }
                    return Err(syn::Error::new(
                        meta.path.span(),
                        "greater_than_or_equal requires a string field name",
                    ));
                }
                "equal_to" | "eq" => {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) = value
                    {
                        return Ok(ValidationRule::EqualTo(s.value()));
                    }
                    return Err(syn::Error::new(
                        meta.path.span(),
                        "equal_to requires a string field name",
                    ));
                }
                "not_equal_to" | "neq" => {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) = value
                    {
                        return Ok(ValidationRule::NotEqualTo(s.value()));
                    }
                    return Err(syn::Error::new(
                        meta.path.span(),
                        "not_equal_to requires a string field name",
                    ));
                }
                "exists_in" => {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) = value
                    {
                        return Ok(ValidationRule::ExistsIn(s.value()));
                    }
                    return Err(syn::Error::new(
                        meta.path.span(),
                        "exists_in requires a string field name",
                    ));
                }
                "subset_of" => {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) = value
                    {
                        return Ok(ValidationRule::SubsetOf(s.value()));
                    }
                    return Err(syn::Error::new(
                        meta.path.span(),
                        "subset_of requires a string field name",
                    ));
                }
                _ => {
                    return Err(syn::Error::new(
                        meta.path.span(),
                        format!("unknown validation rule: {}", ident),
                    ));
                }
            }
        }
    }

    // Bare ident (no args)
    parse_validation_rule_bare(&ident, meta.path.span())
}

fn expr_to_f64(expr: &syn::Expr) -> syn::Result<f64> {
    use syn::{Expr, ExprLit, ExprUnary, Lit, UnOp};
    match expr {
        Expr::Lit(ExprLit {
            lit: Lit::Int(i), ..
        }) => i.base10_parse::<f64>(),
        Expr::Lit(ExprLit {
            lit: Lit::Float(f), ..
        }) => f.base10_parse::<f64>(),
        Expr::Unary(ExprUnary {
            op: UnOp::Neg(_),
            expr,
            ..
        }) => Ok(-expr_to_f64(expr)?),
        _ => Err(syn::Error::new_spanned(expr, "expected numeric literal")),
    }
}

fn expr_to_usize(expr: &syn::Expr) -> syn::Result<usize> {
    use syn::{Expr, ExprLit, Lit};
    match expr {
        Expr::Lit(ExprLit {
            lit: Lit::Int(i), ..
        }) => i.base10_parse::<usize>(),
        _ => Err(syn::Error::new_spanned(
            expr,
            "expected non-negative integer",
        )),
    }
}

fn parse_validation_rule_from_exprs(
    name: &str,
    args: &syn::punctuated::Punctuated<syn::Expr, syn::Token![,]>,
    span: proc_macro2::Span,
) -> syn::Result<super::types::ValidationRule> {
    use super::types::ValidationRule;
    use syn::{Expr, ExprLit, Lit};

    match name {
        "min" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "min() takes 1 argument"));
            }
            Ok(ValidationRule::Min(expr_to_f64(&args[0])?))
        }
        "max" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "max() takes 1 argument"));
            }
            Ok(ValidationRule::Max(expr_to_f64(&args[0])?))
        }
        "range" => {
            if args.len() != 2 {
                return Err(syn::Error::new(span, "range() takes 2 arguments"));
            }
            Ok(ValidationRule::Range(
                expr_to_f64(&args[0])?,
                expr_to_f64(&args[1])?,
            ))
        }
        "multiple_of" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "multiple_of() takes 1 argument"));
            }
            Ok(ValidationRule::MultipleOf(expr_to_f64(&args[0])?))
        }
        "min_len" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "min_len() takes 1 argument"));
            }
            Ok(ValidationRule::MinLen(expr_to_usize(&args[0])?))
        }
        "max_len" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "max_len() takes 1 argument"));
            }
            Ok(ValidationRule::MaxLen(expr_to_usize(&args[0])?))
        }
        "len" => {
            if args.len() != 2 {
                return Err(syn::Error::new(span, "len() takes 2 arguments"));
            }
            Ok(ValidationRule::Len(
                expr_to_usize(&args[0])?,
                expr_to_usize(&args[1])?,
            ))
        }
        "pattern" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "pattern() takes 1 argument"));
            }
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = &args[0]
            {
                Ok(ValidationRule::Pattern(s.value()))
            } else {
                Err(syn::Error::new(
                    span,
                    "pattern() requires a string argument",
                ))
            }
        }
        "min_items" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "min_items() takes 1 argument"));
            }
            Ok(ValidationRule::MinItems(expr_to_usize(&args[0])?))
        }
        "max_items" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "max_items() takes 1 argument"));
            }
            Ok(ValidationRule::MaxItems(expr_to_usize(&args[0])?))
        }
        "func" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "func() takes 1 argument"));
            }
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = &args[0]
            {
                Ok(ValidationRule::Func(s.value()))
            } else {
                Err(syn::Error::new(span, "func() requires a string argument"))
            }
        }
        "less_than" | "lt" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "less_than() takes 1 argument"));
            }
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = &args[0]
            {
                Ok(ValidationRule::LessThan(s.value()))
            } else {
                Err(syn::Error::new(
                    span,
                    "less_than() requires a string field name",
                ))
            }
        }
        "less_than_or_equal" | "lte" => {
            if args.len() != 1 {
                return Err(syn::Error::new(
                    span,
                    "less_than_or_equal() takes 1 argument",
                ));
            }
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = &args[0]
            {
                Ok(ValidationRule::LessThanOrEqual(s.value()))
            } else {
                Err(syn::Error::new(
                    span,
                    "less_than_or_equal() requires a string field name",
                ))
            }
        }
        "greater_than" | "gt" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "greater_than() takes 1 argument"));
            }
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = &args[0]
            {
                Ok(ValidationRule::GreaterThan(s.value()))
            } else {
                Err(syn::Error::new(
                    span,
                    "greater_than() requires a string field name",
                ))
            }
        }
        "greater_than_or_equal" | "gte" => {
            if args.len() != 1 {
                return Err(syn::Error::new(
                    span,
                    "greater_than_or_equal() takes 1 argument",
                ));
            }
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = &args[0]
            {
                Ok(ValidationRule::GreaterThanOrEqual(s.value()))
            } else {
                Err(syn::Error::new(
                    span,
                    "greater_than_or_equal() requires a string field name",
                ))
            }
        }
        "equal_to" | "eq" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "equal_to() takes 1 argument"));
            }
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = &args[0]
            {
                Ok(ValidationRule::EqualTo(s.value()))
            } else {
                Err(syn::Error::new(
                    span,
                    "equal_to() requires a string field name",
                ))
            }
        }
        "not_equal_to" | "neq" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "not_equal_to() takes 1 argument"));
            }
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = &args[0]
            {
                Ok(ValidationRule::NotEqualTo(s.value()))
            } else {
                Err(syn::Error::new(
                    span,
                    "not_equal_to() requires a string field name",
                ))
            }
        }
        "exists_in" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "exists_in() takes 1 argument"));
            }
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = &args[0]
            {
                Ok(ValidationRule::ExistsIn(s.value()))
            } else {
                Err(syn::Error::new(
                    span,
                    "exists_in() requires a string field name",
                ))
            }
        }
        "subset_of" => {
            if args.len() != 1 {
                return Err(syn::Error::new(span, "subset_of() takes 1 argument"));
            }
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = &args[0]
            {
                Ok(ValidationRule::SubsetOf(s.value()))
            } else {
                Err(syn::Error::new(
                    span,
                    "subset_of() requires a string field name",
                ))
            }
        }
        _ => Err(syn::Error::new(
            span,
            format!("unknown validation rule: {}", name),
        )),
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
