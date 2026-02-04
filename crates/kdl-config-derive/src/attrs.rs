use proc_macro2::{Span, TokenStream};
use syn::spanned::Spanned;
use syn::{
    Attribute, Expr, ExprLit, ExprUnary, Field, Ident, Lit, LitStr, Path, Type, TypePath, UnOp,
};

#[derive(Debug, Default)]
pub struct StructAttrs {
    pub node_name: Option<String>,
    pub rename_all: RenameStrategy,
    pub default_placement: Option<DefaultPlacement>,
    pub default_bool: Option<BoolMode>,
    pub default_flag_style: Option<FlagStyle>,
    pub default_conflict: Option<ConflictPolicy>,
    pub deny_unknown: Option<bool>,
    pub schema: StructSchemaOverride,
}

#[derive(Debug, Clone)]
pub enum DefaultPlacement {
    Exhaustive,
    Attr,
    Value,
    Child,
}

#[derive(Debug, Clone)]
pub enum BoolMode {
    PresenceAndValue,
    ValueOnly,
    PresenceOnly,
}

impl quote::ToTokens for BoolMode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            BoolMode::PresenceAndValue => {
                tokens.extend(quote::quote! { BoolMode::PresenceAndValue })
            }
            BoolMode::ValueOnly => tokens.extend(quote::quote! { BoolMode::ValueOnly }),
            BoolMode::PresenceOnly => tokens.extend(quote::quote! { BoolMode::PresenceOnly }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FlagStyle {
    Both,
    ValueNo,
    WithWithout,
}

impl quote::ToTokens for FlagStyle {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            FlagStyle::Both => tokens.extend(quote::quote! { FlagStyle::Both }),
            FlagStyle::ValueNo => tokens.extend(quote::quote! { FlagStyle::ValueNo }),
            FlagStyle::WithWithout => tokens.extend(quote::quote! { FlagStyle::WithWithout }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ConflictPolicy {
    Error,
    First,
    Last,
    Append,
}

impl quote::ToTokens for ConflictPolicy {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            ConflictPolicy::Error => tokens.extend(quote::quote! { ConflictPolicy::Error }),
            ConflictPolicy::First => tokens.extend(quote::quote! { ConflictPolicy::First }),
            ConflictPolicy::Last => tokens.extend(quote::quote! { ConflictPolicy::Last }),
            ConflictPolicy::Append => tokens.extend(quote::quote! { ConflictPolicy::Append }),
        }
    }
}

#[derive(Debug)]
pub struct FieldAttrs {
    pub span: Span,
    pub placement: FieldPlacement,
    pub required: Option<bool>,
    pub name: Option<String>,
    pub container: Option<String>,
    pub children_map: bool,
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

#[derive(Debug, Clone, Default)]
pub struct StructSchemaOverride {
    pub name: Option<String>,
    pub description: Option<String>,
    pub deny_unknown: Option<bool>,
}

#[derive(Debug, Clone, Copy)]
pub enum SchemaTypeOverride {
    String,
    Integer,
    Float,
    Boolean,
    Null,
}

#[derive(Debug, Clone, Default)]
pub struct FieldSchemaOverride {
    pub skip: bool,
    pub name: Option<String>,
    pub required: Option<bool>,
    pub ty: Option<SchemaTypeOverride>,
    pub description: Option<String>,
}

#[derive(Debug, Clone, Copy)]
pub enum RenderPlacement {
    Attr,
    Value,
    Child,
    Children,
    Registry,
}

#[derive(Debug, Clone)]
pub struct FieldPlacement {
    pub attr: bool,
    pub keyed: bool,
    pub positional: Option<usize>,
    pub positional_list: bool,
    pub flag: Option<(Option<String>, Option<String>)>,
    pub value: bool,
    pub child: bool,
    pub children: bool,
    pub registry: bool,
    pub modifier: bool,
}

#[derive(Debug, Clone)]
pub enum RegistryKey {
    Arg(usize),
    Attr(String),
    Function(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChildrenMapKind {
    HashMap,
    Vec,
    OptionVec,
}

impl Default for FieldPlacement {
    fn default() -> Self {
        Self {
            attr: false,
            keyed: false,
            positional: None,
            positional_list: false,
            flag: None,
            value: false,
            child: false,
            children: false,
            registry: false,
            modifier: false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DefaultSpec {
    Derive,
    Literal(DefaultLiteral),
    Function(String),
}

#[derive(Debug, Clone)]
pub enum DefaultLiteral {
    Int(i128),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum RenameStrategy {
    #[default]
    None,
    KebabCase,
    SnakeCase,
    Lowercase,
    Uppercase,
}

impl RenameStrategy {
    pub fn apply(&self, name: &str) -> String {
        match self {
            RenameStrategy::None => name.to_string(),
            RenameStrategy::KebabCase => to_kebab_case(name),
            RenameStrategy::SnakeCase => to_snake_case(name),
            RenameStrategy::Lowercase => name.to_lowercase(),
            RenameStrategy::Uppercase => name.to_uppercase(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::RenameStrategy;

    #[test]
    fn rename_strategy_kebab() {
        assert_eq!(
            RenameStrategy::KebabCase.apply("myFieldName"),
            "my-field-name"
        );
        assert_eq!(
            RenameStrategy::KebabCase.apply("my_field_name"),
            "my-field-name"
        );
    }

    #[test]
    fn rename_strategy_snake() {
        assert_eq!(
            RenameStrategy::SnakeCase.apply("myFieldName"),
            "my_field_name"
        );
        assert_eq!(
            RenameStrategy::SnakeCase.apply("my-field-name"),
            "my_field_name"
        );
    }

    #[test]
    fn rename_strategy_case() {
        assert_eq!(RenameStrategy::Lowercase.apply("MyField"), "myfield");
        assert_eq!(RenameStrategy::Uppercase.apply("myField"), "MYFIELD");
    }
}

fn to_kebab_case(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 4);
    let mut prev_lower = false;

    for c in s.chars() {
        if c == '_' {
            result.push('-');
            prev_lower = false;
        } else if c.is_ascii_uppercase() {
            if prev_lower {
                result.push('-');
            }
            result.push(c.to_ascii_lowercase());
            prev_lower = false;
        } else {
            result.push(c);
            prev_lower = c.is_ascii_lowercase();
        }
    }

    result
}

fn to_snake_case(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut prev_lower = false;

    for c in s.chars() {
        if c == '-' {
            result.push('_');
            prev_lower = false;
        } else if c.is_ascii_uppercase() {
            if prev_lower {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase());
            prev_lower = false;
        } else {
            result.push(c);
            prev_lower = c.is_ascii_lowercase();
        }
    }

    result
}

pub fn parse_struct_attrs(attrs: &[Attribute]) -> syn::Result<StructAttrs> {
    let mut result = StructAttrs::default();

    for attr in attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("node") {
                if meta.input.peek(syn::Token![=]) {
                    let value: Expr = meta.value()?.parse()?;
                    if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                        result.node_name = Some(lit.value());
                    } else {
                        return Err(syn::Error::new(value.span(), "expected string literal for `node`"));
                    }
                } else if !meta.input.is_empty() {
                    meta.parse_nested_meta(|_| Ok(()))?;
                }
            } else if meta.path.is_ident("rename_all") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    result.rename_all = match lit.value().as_str() {
                        "kebab-case" => RenameStrategy::KebabCase,
                        "snake_case" => RenameStrategy::SnakeCase,
                        "lowercase" => RenameStrategy::Lowercase,
                        "UPPERCASE" => RenameStrategy::Uppercase,
                        "none" => RenameStrategy::None,
                        other => {
                            return Err(syn::Error::new(
                                lit.span(),
                                format!("unknown rename_all value: '{other}'. expected 'kebab-case', 'snake_case', 'lowercase', 'UPPERCASE', or 'none'"),
                            ));
                        }
                    };
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `rename_all`"));
                }
            } else if meta.path.is_ident("default_placement") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    result.default_placement = Some(match lit.value().as_str() {
                        "exhaustive" => DefaultPlacement::Exhaustive,
                        "attr" => DefaultPlacement::Attr,
                        "value" => DefaultPlacement::Value,
                        "child" => DefaultPlacement::Child,
                        other => {
                            return Err(syn::Error::new(lit.span(), format!("unknown default_placement: '{other}'")));
                        }
                    });
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `default_placement`"));
                }
            } else if meta.path.is_ident("default_bool") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    result.default_bool = Some(match lit.value().as_str() {
                        "presence+value" => BoolMode::PresenceAndValue,
                        "value-only" => BoolMode::ValueOnly,
                        "presence-only" => BoolMode::PresenceOnly,
                        other => {
                            return Err(syn::Error::new(lit.span(), format!("unknown default_bool: '{other}'")));
                        }
                    });
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `default_bool`"));
                }
            } else if meta.path.is_ident("default_flag_style") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    result.default_flag_style = Some(match lit.value().as_str() {
                        "both" => FlagStyle::Both,
                        "value|no" => FlagStyle::ValueNo,
                        "with|without" => FlagStyle::WithWithout,
                        other => {
                            return Err(syn::Error::new(lit.span(), format!("unknown default_flag_style: '{other}'")));
                        }
                    });
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `default_flag_style`"));
                }
            } else if meta.path.is_ident("default_conflict") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    result.default_conflict = Some(match lit.value().as_str() {
                        "error" => ConflictPolicy::Error,
                        "first" => ConflictPolicy::First,
                        "last" => ConflictPolicy::Last,
                        "append" => ConflictPolicy::Append,
                        other => {
                            return Err(syn::Error::new(lit.span(), format!("unknown default_conflict: '{other}'")));
                        }
                    });
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `default_conflict`"));
                }
            } else if meta.path.is_ident("deny_unknown") {
                result.deny_unknown = Some(true);
            } else if meta.path.is_ident("schema") {
                if meta.input.peek(syn::Token![=]) {
                    let _: Expr = meta.value()?.parse()?;
                    return Ok(());
                }
                if meta.input.is_empty() {
                    return Ok(());
                }
                meta.parse_nested_meta(|meta| {
                    if meta.path.is_ident("name") || meta.path.is_ident("rename") {
                        let value: Expr = meta.value()?.parse()?;
                        if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                            if result.schema.name.is_some() {
                                return Err(syn::Error::new(
                                    lit.span(),
                                    "schema name override already set",
                                ));
                            }
                            result.schema.name = Some(lit.value());
                        } else {
                            return Err(syn::Error::new(
                                value.span(),
                                "expected string literal for `schema(name = ...)`",
                            ));
                        }
                    } else if meta.path.is_ident("description") || meta.path.is_ident("desc") {
                        let value: Expr = meta.value()?.parse()?;
                        if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                            if result.schema.description.is_some() {
                                return Err(syn::Error::new(
                                    lit.span(),
                                    "schema description override already set",
                                ));
                            }
                            result.schema.description = Some(lit.value());
                        } else {
                            return Err(syn::Error::new(
                                value.span(),
                                "expected string literal for `schema(description = ...)`",
                            ));
                        }
                    } else if meta.path.is_ident("deny_unknown") {
                        let value = if meta.input.peek(syn::Token![=]) {
                            let lit: syn::LitBool = meta.value()?.parse()?;
                            lit.value()
                        } else {
                            true
                        };
                        if result.schema.deny_unknown.is_some() {
                            return Err(syn::Error::new(
                                meta.path.span(),
                                "schema deny_unknown override already set",
                            ));
                        }
                        result.schema.deny_unknown = Some(value);
                    } else if meta.path.is_ident("allow_unknown") {
                        let value = if meta.input.peek(syn::Token![=]) {
                            let lit: syn::LitBool = meta.value()?.parse()?;
                            lit.value()
                        } else {
                            true
                        };
                        if result.schema.deny_unknown.is_some() {
                            return Err(syn::Error::new(
                                meta.path.span(),
                                "schema deny_unknown override already set",
                            ));
                        }
                        result.schema.deny_unknown = Some(!value);
                    } else {
                        return Err(syn::Error::new(
                            meta.path.span(),
                            format!(
                                "unknown schema override: {}",
                                path_to_string(&meta.path)
                            ),
                        ));
                    }
                    Ok(())
                })?;
            } else if meta.path.is_ident("choice") || meta.path.is_ident("value") {
                if meta.input.peek(syn::Token![=]) {
                    let _: Expr = meta.value()?.parse()?;
                } else if !meta.input.is_empty() {
                    meta.parse_nested_meta(|_| Ok(()))?;
                }
            } else {
                return Err(syn::Error::new(meta.path.span(), format!("unknown struct attribute: {}", path_to_string(&meta.path))));
            }
            Ok(())
        })?;
    }

    Ok(result)
}

pub fn parse_field_attrs(field: &Field) -> syn::Result<Option<FieldAttrs>> {
    let mut placement = FieldPlacement::default();
    let mut required: Option<bool> = None;
    let mut name: Option<String> = None;
    let mut container: Option<String> = None;
    let mut children_map = false;
    let mut map_node: Option<String> = None;
    let mut registry_key: Option<RegistryKey> = None;
    let mut default_spec: Option<DefaultSpec> = None;
    let mut default_count: u8 = 0;
    let mut skip_serializing_if: Option<String> = None;
    let mut skip = false;
    let mut bool_mode: Option<BoolMode> = None;
    let mut flag_style: Option<FlagStyle> = None;
    let mut conflict: Option<ConflictPolicy> = None;
    let mut render: Option<RenderPlacement> = None;
    let mut scalar = false;
    let mut schema: FieldSchemaOverride = FieldSchemaOverride::default();
    let mut primary_span = field.span();

    let mut has_kdl_attr = false;

    for attr in &field.attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }
        has_kdl_attr = true;
        primary_span = attr.span();

        let mut set_registry_key = |value: RegistryKey, span: Span| -> syn::Result<()> {
            if registry_key.is_some() {
                return Err(syn::Error::new(span, "registry key is already set"));
            }
            registry_key = Some(value);
            Ok(())
        };

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("attr") {
                placement.attr = true;
            } else if meta.path.is_ident("keyed") {
                placement.keyed = true;
            } else if meta.path.is_ident("positional") {
                if placement.positional.is_some() || placement.positional_list {
                    return Err(syn::Error::new(
                        meta.path.span(),
                        "positional placement is already set",
                    ));
                }
                if meta.input.peek(syn::Token![=]) {
                    let value: Expr = meta.value()?.parse()?;
                    match value {
                        Expr::Lit(ExprLit {
                            lit: Lit::Int(lit), ..
                        }) => {
                            placement.positional = Some(lit.base10_parse()?);
                        }
                        Expr::Lit(ExprLit {
                            lit: Lit::Str(lit), ..
                        }) => {
                            let raw = lit.value();
                            if raw == "*" || raw == "rest" {
                                placement.positional_list = true;
                            } else {
                                return Err(syn::Error::new(
                                    lit.span(),
                                    "expected integer literal or \"rest\"/\"*\" for `positional`",
                                ));
                            }
                        }
                        _ => {
                            return Err(syn::Error::new(
                                value.span(),
                                "expected integer literal or \"rest\"/\"*\" for `positional`",
                            ));
                        }
                    }
                } else if meta.input.is_empty() {
                    placement.positional_list = true;
                } else {
                    return Err(meta.error("positional does not accept nested meta"));
                }
            } else if meta.path.is_ident("flag") {
                if meta.input.peek(syn::Token![=]) {
                    let value: Expr = meta.value()?.parse()?;
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(lit), ..
                    }) = value
                    {
                        placement.flag = Some((Some(lit.value()), None));
                    } else {
                        return Err(syn::Error::new(
                            value.span(),
                            "expected string literal for `flag`",
                        ));
                    }
                } else {
                    placement.flag = Some((None, None));
                }
            } else if meta.path.is_ident("neg_flag") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(lit), ..
                }) = value
                {
                    let (pos, _) = placement.flag.clone().unwrap_or((None, None));
                    placement.flag = Some((pos, Some(lit.value())));
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected string literal for `neg_flag`",
                    ));
                }
            } else if meta.path.is_ident("value") {
                placement.value = true;
            } else if meta.path.is_ident("child") {
                placement.child = true;
            } else if meta.path.is_ident("children") {
                placement.children = true;
            } else if meta.path.is_ident("registry") {
                placement.registry = true;
            } else if meta.path.is_ident("modifier") {
                placement.modifier = true;
            } else if meta.path.is_ident("container") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(lit), ..
                }) = value
                {
                    container = Some(lit.value());
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected string literal for `container`",
                    ));
                }
            } else if meta.path.is_ident("children_map") {
                children_map = true;
            } else if meta.path.is_ident("map_node") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(lit), ..
                }) = value
                {
                    if map_node.is_some() {
                        return Err(syn::Error::new(lit.span(), "map_node is already set"));
                    }
                    map_node = Some(lit.value());
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected string literal for `map_node`",
                    ));
                }
            } else if meta.path.is_ident("key_arg") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Int(ref lit),
                    ..
                }) = value
                {
                    set_registry_key(RegistryKey::Arg(lit.base10_parse()?), value.span())?;
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected integer literal for `key_arg`",
                    ));
                }
            } else if meta.path.is_ident("key_attr") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(ref lit),
                    ..
                }) = value
                {
                    set_registry_key(RegistryKey::Attr(lit.value()), value.span())?;
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected string literal for `key_attr`",
                    ));
                }
            } else if meta.path.is_ident("key_fn") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(ref lit),
                    ..
                }) = value
                {
                    set_registry_key(RegistryKey::Function(lit.value()), value.span())?;
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected string literal for `key_fn`",
                    ));
                }
            } else if meta.path.is_ident("required") {
                required = Some(true);
            } else if meta.path.is_ident("optional") {
                required = Some(false);
            } else if meta.path.is_ident("name") || meta.path.is_ident("rename") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(lit), ..
                }) = value
                {
                    name = Some(lit.value());
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected string literal for `name`",
                    ));
                }
            } else if meta.path.is_ident("default") {
                default_count += 1;
                if meta.input.peek(syn::Token![=]) {
                    let _ = meta.input.parse::<syn::Token![=]>()?;
                    let expr: Expr = meta.input.parse()?;
                    default_spec = Some(DefaultSpec::Literal(parse_default_expr(&expr)?));
                } else {
                    default_spec = Some(DefaultSpec::Derive);
                }
            } else if meta.path.is_ident("default_fn") {
                default_count += 1;
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(lit), ..
                }) = value
                {
                    default_spec = Some(DefaultSpec::Function(lit.value()));
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected string literal for `default_fn`",
                    ));
                }
            } else if meta.path.is_ident("skip") {
                skip = true;
            } else if meta.path.is_ident("skip_serializing_if") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(lit), ..
                }) = value
                {
                    skip_serializing_if = Some(lit.value());
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected string literal for `skip_serializing_if`",
                    ));
                }
            } else if meta.path.is_ident("bool") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(lit), ..
                }) = value
                {
                    bool_mode = Some(match lit.value().as_str() {
                        "presence+value" => BoolMode::PresenceAndValue,
                        "value-only" => BoolMode::ValueOnly,
                        "presence-only" => BoolMode::PresenceOnly,
                        other => {
                            return Err(syn::Error::new(
                                lit.span(),
                                format!("unknown bool mode: '{other}'"),
                            ));
                        }
                    });
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected string literal for `bool`",
                    ));
                }
            } else if meta.path.is_ident("flag_style") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(lit), ..
                }) = value
                {
                    flag_style = Some(match lit.value().as_str() {
                        "both" => FlagStyle::Both,
                        "value|no" => FlagStyle::ValueNo,
                        "with|without" => FlagStyle::WithWithout,
                        other => {
                            return Err(syn::Error::new(
                                lit.span(),
                                format!("unknown flag_style: '{other}'"),
                            ));
                        }
                    });
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected string literal for `flag_style`",
                    ));
                }
            } else if meta.path.is_ident("conflict") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(lit), ..
                }) = value
                {
                    conflict = Some(match lit.value().as_str() {
                        "error" => ConflictPolicy::Error,
                        "first" => ConflictPolicy::First,
                        "last" => ConflictPolicy::Last,
                        "append" => ConflictPolicy::Append,
                        other => {
                            return Err(syn::Error::new(
                                lit.span(),
                                format!("unknown conflict policy: '{other}'"),
                            ));
                        }
                    });
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected string literal for `conflict`",
                    ));
                }
            } else if meta.path.is_ident("render") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(lit), ..
                }) = value
                {
                    render = Some(match lit.value().as_str() {
                        "attr" => RenderPlacement::Attr,
                        "value" => RenderPlacement::Value,
                        "child" => RenderPlacement::Child,
                        "children" => RenderPlacement::Children,
                        "registry" => RenderPlacement::Registry,
                        other => {
                            return Err(syn::Error::new(
                                lit.span(),
                                format!("unknown render placement: '{other}'"),
                            ));
                        }
                    });
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected string literal for `render`",
                    ));
                }
            } else if meta.path.is_ident("scalar") || meta.path.is_ident("value_type") {
                let value = if meta.input.peek(syn::Token![=]) {
                    let lit: syn::LitBool = meta.value()?.parse()?;
                    lit.value()
                } else {
                    true
                };
                scalar = value;
            } else if meta.path.is_ident("schema") {
                if meta.input.peek(syn::Token![=]) {
                    let _: Expr = meta.value()?.parse()?;
                    return Ok(());
                }
                if meta.input.is_empty() {
                    return Ok(());
                }
                meta.parse_nested_meta(|meta| {
                    if meta.path.is_ident("skip") {
                        schema.skip = true;
                    } else if meta.path.is_ident("name") || meta.path.is_ident("rename") {
                        let value: Expr = meta.value()?.parse()?;
                        if let Expr::Lit(ExprLit {
                            lit: Lit::Str(lit), ..
                        }) = value
                        {
                            if schema.name.is_some() {
                                return Err(syn::Error::new(
                                    lit.span(),
                                    "schema name override already set",
                                ));
                            }
                            schema.name = Some(lit.value());
                        } else {
                            return Err(syn::Error::new(
                                value.span(),
                                "expected string literal for `schema(name = ...)`",
                            ));
                        }
                    } else if meta.path.is_ident("type") {
                        let value: Expr = meta.value()?.parse()?;
                        if let Expr::Lit(ExprLit {
                            lit: Lit::Str(lit), ..
                        }) = value
                        {
                            if schema.ty.is_some() {
                                return Err(syn::Error::new(
                                    lit.span(),
                                    "schema type override already set",
                                ));
                            }
                            schema.ty = Some(parse_schema_type(&lit)?);
                        } else {
                            return Err(syn::Error::new(
                                value.span(),
                                "expected string literal for `schema(type = ...)`",
                            ));
                        }
                    } else if meta.path.is_ident("required") {
                        let value = if meta.input.peek(syn::Token![=]) {
                            let lit: syn::LitBool = meta.value()?.parse()?;
                            lit.value()
                        } else {
                            true
                        };
                        if schema.required.is_some() {
                            return Err(syn::Error::new(
                                meta.path.span(),
                                "schema required override already set",
                            ));
                        }
                        schema.required = Some(value);
                    } else if meta.path.is_ident("optional") {
                        let value = if meta.input.peek(syn::Token![=]) {
                            let lit: syn::LitBool = meta.value()?.parse()?;
                            lit.value()
                        } else {
                            true
                        };
                        if schema.required.is_some() {
                            return Err(syn::Error::new(
                                meta.path.span(),
                                "schema required override already set",
                            ));
                        }
                        schema.required = Some(!value);
                    } else if meta.path.is_ident("description") || meta.path.is_ident("desc") {
                        let value: Expr = meta.value()?.parse()?;
                        if let Expr::Lit(ExprLit {
                            lit: Lit::Str(lit), ..
                        }) = value
                        {
                            if schema.description.is_some() {
                                return Err(syn::Error::new(
                                    lit.span(),
                                    "schema description override already set",
                                ));
                            }
                            schema.description = Some(lit.value());
                        } else {
                            return Err(syn::Error::new(
                                value.span(),
                                "expected string literal for `schema(description = ...)`",
                            ));
                        }
                    } else {
                        return Err(syn::Error::new(
                            meta.path.span(),
                            format!("unknown schema override: {}", path_to_string(&meta.path)),
                        ));
                    }
                    Ok(())
                })?;
            } else {
                return Err(syn::Error::new(
                    meta.path.span(),
                    format!("unknown field attribute: {}", path_to_string(&meta.path)),
                ));
            }
            Ok(())
        })?;
    }

    if default_count > 1 {
        return Err(syn::Error::new(
            primary_span,
            "field cannot have multiple default specifications (`default`, `default = ...`, `default_fn`)",
        ));
    }

    if !has_kdl_attr {
        return Ok(Some(FieldAttrs {
            span: primary_span,
            placement,
            required,
            name,
            container,
            registry_key,
            default: default_spec,
            skip,
            skip_serializing_if,
            bool_mode,
            flag_style,
            conflict,
            render,
            scalar,
            schema: schema.clone(),
            children_map,
            map_node,
        }));
    }

    Ok(Some(FieldAttrs {
        span: primary_span,
        placement,
        required,
        name,
        container,
        children_map,
        map_node,
        registry_key,
        default: default_spec,
        skip,
        skip_serializing_if,
        bool_mode,
        flag_style,
        conflict,
        render,
        scalar,
        schema,
    }))
}

fn parse_default_expr(expr: &Expr) -> syn::Result<DefaultLiteral> {
    match expr {
        Expr::Lit(ExprLit { lit, .. }) => parse_default_literal(lit),
        Expr::Unary(ExprUnary {
            op: UnOp::Neg(_),
            expr,
            ..
        }) => match &**expr {
            Expr::Lit(ExprLit { lit, .. }) => parse_negated_default_literal(lit),
            other => Err(syn::Error::new(
                other.span(),
                "default value must be a literal or negated literal",
            )),
        },
        _ => Err(syn::Error::new(
            expr.span(),
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
        _ => Err(syn::Error::new(
            lit.span(),
            "default value must be an integer, float, boolean, or string literal",
        )),
    }
}

fn parse_negated_default_literal(lit: &Lit) -> syn::Result<DefaultLiteral> {
    match lit {
        Lit::Int(lit_int) => Ok(DefaultLiteral::Int(-lit_int.base10_parse::<i128>()?)),
        Lit::Float(lit_float) => Ok(DefaultLiteral::Float(-lit_float.base10_parse::<f64>()?)),
        _ => Err(syn::Error::new(
            lit.span(),
            "default value must be a numeric literal",
        )),
    }
}

fn parse_schema_type(lit: &LitStr) -> syn::Result<SchemaTypeOverride> {
    match lit.value().as_str() {
        "string" => Ok(SchemaTypeOverride::String),
        "integer" | "int" => Ok(SchemaTypeOverride::Integer),
        "float" => Ok(SchemaTypeOverride::Float),
        "number" => Ok(SchemaTypeOverride::Float),
        "boolean" | "bool" => Ok(SchemaTypeOverride::Boolean),
        "null" => Ok(SchemaTypeOverride::Null),
        other => Err(syn::Error::new(
            lit.span(),
            format!(
                "unknown schema type override: '{other}'. expected string, integer, float, boolean, or null"
            ),
        )),
    }
}

fn path_to_string(path: &Path) -> String {
    path.segments
        .iter()
        .map(|s| s.ident.to_string())
        .collect::<Vec<_>>()
        .join("::")
}

pub fn is_option_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| s.ident == "Option")
            .unwrap_or(false),
        _ => false,
    }
}

pub fn is_vec_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| s.ident == "Vec")
            .unwrap_or(false),
        _ => false,
    }
}

pub fn is_hashmap_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| s.ident == "HashMap")
            .unwrap_or(false),
        _ => false,
    }
}

pub fn extract_inner_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(segment) = path.segments.last() {
            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                    return Some(inner);
                }
            }
        }
    }
    None
}

pub fn extract_hashmap_types(ty: &Type) -> Option<(&Type, &Type)> {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(segment) = path.segments.last() {
            if segment.ident == "HashMap" {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    let mut iter = args.args.iter();
                    if let (
                        Some(syn::GenericArgument::Type(key_ty)),
                        Some(syn::GenericArgument::Type(val_ty)),
                    ) = (iter.next(), iter.next())
                    {
                        return Some((key_ty, val_ty));
                    }
                }
            }
        }
    }
    None
}

pub fn extract_tuple_types(ty: &Type) -> Option<(&Type, &Type)> {
    if let Type::Tuple(tuple) = ty {
        if tuple.elems.len() == 2 {
            let mut iter = tuple.elems.iter();
            if let (Some(first), Some(second)) = (iter.next(), iter.next()) {
                return Some((first, second));
            }
        }
    }
    None
}

pub fn extract_vec_tuple_types(ty: &Type) -> Option<(&Type, &Type)> {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(segment) = path.segments.last() {
            if segment.ident == "Vec" {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                        return extract_tuple_types(inner);
                    }
                }
            }
        }
    }
    None
}

pub fn extract_children_map_types(ty: &Type) -> Option<(ChildrenMapKind, &Type, &Type)> {
    if let Some((key_ty, val_ty)) = extract_hashmap_types(ty) {
        return Some((ChildrenMapKind::HashMap, key_ty, val_ty));
    }

    if let Some((key_ty, val_ty)) = extract_vec_tuple_types(ty) {
        return Some((ChildrenMapKind::Vec, key_ty, val_ty));
    }

    if is_option_type(ty) {
        if let Some(inner) = extract_inner_type(ty) {
            if let Some((key_ty, val_ty)) = extract_vec_tuple_types(inner) {
                return Some((ChildrenMapKind::OptionVec, key_ty, val_ty));
            }
        }
    }

    None
}

pub fn extract_registry_vec_value(ty: &Type) -> Option<(&Type, bool)> {
    if let Some((key_ty, val_ty)) = extract_vec_tuple_types(ty) {
        if is_string_type(key_ty) {
            return Some((val_ty, false));
        }
    }

    if is_option_type(ty) {
        if let Some(inner) = extract_inner_type(ty) {
            if let Some((key_ty, val_ty)) = extract_vec_tuple_types(inner) {
                if is_string_type(key_ty) {
                    return Some((val_ty, true));
                }
            }
        }
    }

    None
}

pub fn is_bool_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| s.ident == "bool")
            .unwrap_or(false),
        _ => false,
    }
}

pub fn is_string_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| s.ident == "String" || s.ident == "PathBuf")
            .unwrap_or(false),
        _ => false,
    }
}

pub fn is_numeric_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| {
                matches!(
                    s.ident.to_string().as_str(),
                    "i128" | "i64" | "i32" | "u64" | "u32" | "usize" | "f64" | "f32"
                )
            })
            .unwrap_or(false),
        _ => false,
    }
}

pub fn is_modifier_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| s.ident == "Modifier")
            .unwrap_or(false),
        _ => false,
    }
}

fn is_value_type(ty: &Type) -> bool {
    if is_bool_type(ty) || is_string_type(ty) || is_numeric_type(ty) {
        return true;
    }

    if is_option_type(ty) {
        if let Some(inner) = extract_inner_type(ty) {
            return is_value_type(inner);
        }
    }

    false
}

#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub ident: Ident,
    pub ty: Type,
    pub kdl_key: String,
    pub placement: FieldPlacement,
    pub required: bool,
    pub is_optional: bool,
    pub is_vec: bool,
    pub is_option_vec: bool,
    pub is_hashmap: bool,
    pub is_bool: bool,
    pub is_modifier: bool,
    pub is_scalar: bool,
    pub is_skipped: bool,
    pub children_map: bool,
    pub map_node: Option<String>,
    pub children_map_kind: Option<ChildrenMapKind>,
    pub default: Option<DefaultSpec>,
    pub skip_serializing_if: Option<String>,
    pub bool_mode: Option<BoolMode>,
    pub flag_style: Option<FlagStyle>,
    pub conflict: Option<ConflictPolicy>,
    pub render: Option<RenderPlacement>,
    pub container: Option<String>,
    pub registry_key: Option<RegistryKey>,
    pub schema: FieldSchemaOverride,
}

impl FieldInfo {
    pub fn from_field(field: &Field, rename_all: RenameStrategy) -> syn::Result<Option<Self>> {
        let attrs = parse_field_attrs(field)?;
        let attrs = match attrs {
            Some(a) => a,
            None => return Ok(None),
        };

        let ident = field
            .ident
            .clone()
            .ok_or_else(|| syn::Error::new(field.span(), "tuple structs are not supported"))?;

        let field_name = ident.to_string();
        let kdl_key = attrs.name.unwrap_or_else(|| rename_all.apply(&field_name));

        let is_optional = is_option_type(&field.ty);
        let is_vec = is_vec_type(&field.ty);
        let is_option_vec = is_option_type(&field.ty)
            && extract_inner_type(&field.ty)
                .map(is_vec_type)
                .unwrap_or(false);
        let is_hashmap = is_hashmap_type(&field.ty);
        let is_bool = is_bool_type(&field.ty)
            || (is_optional
                && extract_inner_type(&field.ty)
                    .map(is_bool_type)
                    .unwrap_or(false));
        let is_modifier = is_modifier_type(&field.ty)
            || (is_optional
                && extract_inner_type(&field.ty)
                    .map(is_modifier_type)
                    .unwrap_or(false));
        let is_scalar = attrs.scalar;
        let is_value = is_value_type(&field.ty) || (is_scalar && !is_vec && !is_option_vec);
        let vec_inner = if is_option_vec {
            extract_inner_type(&field.ty).and_then(extract_inner_type)
        } else if is_vec {
            extract_inner_type(&field.ty)
        } else {
            None
        };
        let is_vec_value = vec_inner.map(is_value_type).unwrap_or(false)
            || (is_scalar && (is_vec || is_option_vec));
        let is_value_like = is_value || is_vec_value;

        let required = match attrs.required {
            Some(true) => true,
            Some(false) => false,
            None => {
                if is_bool || attrs.placement.registry || attrs.children_map {
                    false
                } else {
                    !is_optional && !is_option_vec
                }
            }
        };

        let err_span = attrs.span;

        let is_registry_vec = extract_registry_vec_value(&field.ty).is_some();

        if attrs.schema.ty.is_some() && !is_value_like {
            return Err(syn::Error::new(
                err_span,
                "schema(type = ...) is only valid for scalar value fields",
            ));
        }

        if attrs.placement.modifier {
            if !is_modifier {
                return Err(syn::Error::new(
                    err_span,
                    "modifier placement requires Modifier or Option<Modifier> field type",
                ));
            }
            if attrs.placement.attr
                || attrs.placement.keyed
                || attrs.placement.positional.is_some()
                || attrs.placement.positional_list
                || attrs.placement.flag.is_some()
                || attrs.placement.value
                || attrs.placement.child
                || attrs.placement.children
                || attrs.placement.registry
            {
                return Err(syn::Error::new(
                    err_span,
                    "modifier placement cannot be combined with other placements",
                ));
            }
        }

        if (attrs.placement.attr
            || attrs.placement.keyed
            || attrs.placement.positional.is_some()
            || attrs.placement.positional_list
            || attrs.placement.flag.is_some())
            && !is_value_like
        {
            return Err(syn::Error::new(
                err_span,
                "attr/keyed/positional/flag placement is incompatible with nested node types",
            ));
        }

        if (attrs.placement.child || attrs.placement.children) && is_value_like {
            return Err(syn::Error::new(
                err_span,
                "child/children placement is incompatible with scalar types",
            ));
        }

        if attrs.placement.value && !is_value_like {
            return Err(syn::Error::new(
                err_span,
                "value placement is only valid for scalar types",
            ));
        }

        if attrs.placement.registry && !is_hashmap && !is_registry_vec {
            return Err(syn::Error::new(
                err_span,
                "registry placement requires HashMap<String, V> or Vec<(String, V)> field type",
            ));
        }

        if attrs.children_map {
            let children_map_kind = extract_children_map_types(&field.ty);
            if children_map_kind.is_none() {
                return Err(syn::Error::new(
                    err_span,
                    "children_map placement requires HashMap<K, V> or Vec<(K, V)> field type",
                ));
            }
            if attrs.placement.attr
                || attrs.placement.keyed
                || attrs.placement.positional.is_some()
                || attrs.placement.positional_list
                || attrs.placement.flag.is_some()
                || attrs.placement.value
                || attrs.placement.child
                || attrs.placement.children
                || attrs.placement.registry
                || attrs.placement.modifier
            {
                return Err(syn::Error::new(
                    err_span,
                    "children_map placement cannot be combined with other placements",
                ));
            }
            if attrs.container.is_some() {
                return Err(syn::Error::new(
                    err_span,
                    "children_map does not support container override",
                ));
            }
            if attrs.registry_key.is_some() && attrs.map_node.is_none() {
                return Err(syn::Error::new(
                    err_span,
                    "children_map key options require map_node",
                ));
            }
        } else if attrs.map_node.is_some() {
            return Err(syn::Error::new(
                err_span,
                "map_node requires #[kdl(children_map)]",
            ));
        }

        if attrs.registry_key.is_some()
            && !attrs.placement.registry
            && !(attrs.children_map && attrs.map_node.is_some())
        {
            return Err(syn::Error::new(
                err_span,
                "registry key options require #[kdl(registry)] or #[kdl(children_map, map_node = ...)]",
            ));
        }

        if attrs.placement.children && !is_vec && !is_option_vec {
            return Err(syn::Error::new(
                err_span,
                "children placement requires Vec<T> field type",
            ));
        }

        if attrs.placement.flag.is_some() && !is_bool {
            return Err(syn::Error::new(
                err_span,
                "flag placement is only valid for bool fields",
            ));
        }

        if attrs.placement.positional_list && !is_vec && !is_option_vec {
            return Err(syn::Error::new(
                err_span,
                "positional list placement requires Vec<T> or Option<Vec<T>> field type",
            ));
        }

        if let Some((pos, neg)) = &attrs.placement.flag {
            if pos.is_some() ^ neg.is_some() {
                return Err(syn::Error::new(
                    err_span,
                    "flag override requires both flag and neg_flag",
                ));
            }
        }

        if matches!(attrs.conflict, Some(ConflictPolicy::Append)) && !is_vec && !is_option_vec {
            return Err(syn::Error::new(
                err_span,
                "append conflict policy is only valid for Vec<T> fields",
            ));
        }

        Ok(Some(FieldInfo {
            ident,
            ty: field.ty.clone(),
            kdl_key,
            placement: attrs.placement,
            required,
            is_optional,
            is_vec,
            is_option_vec,
            is_hashmap,
            is_bool,
            is_modifier,
            is_scalar,
            is_skipped: attrs.skip,
            children_map: attrs.children_map,
            map_node: attrs.map_node,
            children_map_kind: extract_children_map_types(&field.ty).map(|(kind, _, _)| kind),
            default: attrs.default,
            skip_serializing_if: attrs.skip_serializing_if,
            bool_mode: attrs.bool_mode,
            flag_style: attrs.flag_style,
            conflict: attrs.conflict,
            render: attrs.render,
            container: attrs.container,
            registry_key: attrs.registry_key,
            schema: attrs.schema,
        }))
    }

    pub fn inner_type(&self) -> Option<&Type> {
        extract_inner_type(&self.ty)
    }

    pub fn from_tuple_field(field: &Field, index: usize) -> syn::Result<Option<Self>> {
        let attrs = parse_field_attrs(field)?;
        let attrs = match attrs {
            Some(a) => a,
            None => return Ok(None),
        };

        let err_span = attrs.span;

        if attrs.name.is_some() || attrs.container.is_some() || attrs.registry_key.is_some() {
            return Err(syn::Error::new(
                err_span,
                "tuple struct fields only support positional placement without names or registry keys",
            ));
        }
        if attrs.children_map || attrs.map_node.is_some() {
            return Err(syn::Error::new(
                err_span,
                "tuple struct fields do not support children_map",
            ));
        }

        if attrs.placement.attr
            || attrs.placement.keyed
            || attrs.placement.positional_list
            || attrs.placement.flag.is_some()
            || attrs.placement.value
            || attrs.placement.child
            || attrs.placement.children
            || attrs.placement.registry
            || attrs.placement.modifier
        {
            return Err(syn::Error::new(
                err_span,
                "tuple struct fields only support positional placement",
            ));
        }

        let positional = attrs.placement.positional.unwrap_or(index);
        let mut placement = attrs.placement.clone();
        placement.positional = Some(positional);

        let is_optional = is_option_type(&field.ty);
        let is_vec = is_vec_type(&field.ty);
        let is_option_vec = is_option_type(&field.ty)
            && extract_inner_type(&field.ty)
                .map(is_vec_type)
                .unwrap_or(false);
        let is_hashmap = is_hashmap_type(&field.ty);
        let is_bool = is_bool_type(&field.ty)
            || (is_optional
                && extract_inner_type(&field.ty)
                    .map(is_bool_type)
                    .unwrap_or(false));
        let is_modifier = is_modifier_type(&field.ty)
            || (is_optional
                && extract_inner_type(&field.ty)
                    .map(is_modifier_type)
                    .unwrap_or(false));
        let is_scalar = attrs.scalar;

        if is_vec || is_option_vec {
            return Err(syn::Error::new(
                err_span,
                "tuple struct positional fields do not support Vec types",
            ));
        }

        if is_hashmap || is_modifier {
            return Err(syn::Error::new(
                err_span,
                "tuple struct positional fields must be scalar value types",
            ));
        }

        if !is_value_type(&field.ty) && !is_scalar {
            return Err(syn::Error::new(
                err_span,
                "tuple struct positional fields must be scalar value types",
            ));
        }

        if let Some((pos, neg)) = &attrs.placement.flag {
            if pos.is_some() ^ neg.is_some() {
                return Err(syn::Error::new(
                    err_span,
                    "flag override requires both flag and neg_flag",
                ));
            }
        }

        let required = match attrs.required {
            Some(true) => true,
            Some(false) => false,
            None => {
                if is_bool {
                    false
                } else {
                    !is_optional && !is_option_vec
                }
            }
        };

        let ident = Ident::new(
            &format!("__kdl_field_{index}"),
            proc_macro2::Span::call_site(),
        );
        let kdl_key = format!("arg[{positional}]");

        Ok(Some(FieldInfo {
            ident,
            ty: field.ty.clone(),
            kdl_key,
            placement,
            required,
            is_optional,
            is_vec,
            is_option_vec,
            is_hashmap,
            is_bool,
            is_modifier,
            is_scalar,
            is_skipped: attrs.skip,
            children_map: false,
            map_node: None,
            children_map_kind: None,
            default: attrs.default,
            skip_serializing_if: attrs.skip_serializing_if,
            bool_mode: attrs.bool_mode,
            flag_style: attrs.flag_style,
            conflict: attrs.conflict,
            render: attrs.render,
            container: None,
            registry_key: None,
            schema: attrs.schema,
        }))
    }
}
