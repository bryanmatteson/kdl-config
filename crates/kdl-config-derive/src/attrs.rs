use proc_macro2::Span;
use syn::spanned::Spanned;
use syn::{Attribute, Expr, ExprLit, Field, Ident, Lit, Path, Type, TypePath};

#[derive(Debug, Default)]
pub struct StructAttrs {
    pub node_name: Option<String>,
    pub rename_all: RenameStrategy,
    pub default_placement: Option<DefaultPlacement>,
    pub default_bool: Option<BoolMode>,
    pub default_flag_style: Option<FlagStyle>,
    pub default_conflict: Option<ConflictPolicy>,
    pub deny_unknown: Option<bool>,
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

#[derive(Debug, Clone)]
pub enum FlagStyle {
    Both,
    ValueNo,
    WithWithout,
}

#[derive(Debug, Clone)]
pub enum ConflictPolicy {
    Error,
    First,
    Last,
    Append,
}

#[derive(Debug)]
pub struct FieldAttrs {
    pub span: Span,
    pub placement: FieldPlacement,
    pub required: Option<bool>,
    pub name: Option<String>,
    pub container: Option<String>,
    pub default: Option<DefaultSpec>,
    pub skip_serializing_if: Option<String>,
    pub bool_mode: Option<BoolMode>,
    pub flag_style: Option<FlagStyle>,
    pub conflict: Option<ConflictPolicy>,
    pub render: Option<RenderPlacement>,
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
    pub flag: Option<(Option<String>, Option<String>)>,
    pub value: bool,
    pub child: bool,
    pub children: bool,
    pub registry: bool,
}

impl Default for FieldPlacement {
    fn default() -> Self {
        Self {
            attr: false,
            keyed: false,
            positional: None,
            flag: None,
            value: false,
            child: false,
            children: false,
            registry: false,
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
    Int(i64),
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
        assert_eq!(RenameStrategy::KebabCase.apply("myFieldName"), "my-field-name");
        assert_eq!(RenameStrategy::KebabCase.apply("my_field_name"), "my-field-name");
    }

    #[test]
    fn rename_strategy_snake() {
        assert_eq!(RenameStrategy::SnakeCase.apply("myFieldName"), "my_field_name");
        assert_eq!(RenameStrategy::SnakeCase.apply("my-field-name"), "my_field_name");
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
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    result.node_name = Some(lit.value());
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `node`"));
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
    let mut default_spec: Option<DefaultSpec> = None;
    let mut default_count: u8 = 0;
    let mut skip_serializing_if: Option<String> = None;
    let mut skip = false;
    let mut bool_mode: Option<BoolMode> = None;
    let mut flag_style: Option<FlagStyle> = None;
    let mut conflict: Option<ConflictPolicy> = None;
    let mut render: Option<RenderPlacement> = None;
    let mut primary_span = field.span();

    let mut has_kdl_attr = false;

    for attr in &field.attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }
        has_kdl_attr = true;
        primary_span = attr.span();

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("attr") {
                placement.attr = true;
            } else if meta.path.is_ident("keyed") {
                placement.keyed = true;
            } else if meta.path.is_ident("positional") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Int(lit), .. }) = value {
                    placement.positional = Some(lit.base10_parse()?);
                } else {
                    return Err(syn::Error::new(value.span(), "expected integer literal for `positional`"));
                }
            } else if meta.path.is_ident("flag") {
                if meta.input.peek(syn::Token![=]) {
                    let value: Expr = meta.value()?.parse()?;
                    if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                        placement.flag = Some((Some(lit.value()), None));
                    } else {
                        return Err(syn::Error::new(value.span(), "expected string literal for `flag`"));
                    }
                } else {
                    placement.flag = Some((None, None));
                }
            } else if meta.path.is_ident("neg_flag") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    let (pos, _) = placement.flag.clone().unwrap_or((None, None));
                    placement.flag = Some((pos, Some(lit.value())));
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `neg_flag`"));
                }
            } else if meta.path.is_ident("value") {
                placement.value = true;
            } else if meta.path.is_ident("child") {
                placement.child = true;
            } else if meta.path.is_ident("children") {
                placement.children = true;
            } else if meta.path.is_ident("registry") {
                placement.registry = true;
            } else if meta.path.is_ident("container") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    container = Some(lit.value());
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `container`"));
                }
            } else if meta.path.is_ident("required") {
                required = Some(true);
            } else if meta.path.is_ident("optional") {
                required = Some(false);
            } else if meta.path.is_ident("name") || meta.path.is_ident("rename") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    name = Some(lit.value());
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `name`"));
                }
            } else if meta.path.is_ident("default") {
                default_count += 1;
                if meta.input.peek(syn::Token![=]) {
                    let _ = meta.input.parse::<syn::Token![=]>()?;
                    let lit: Lit = meta.input.parse()?;
                    default_spec = Some(DefaultSpec::Literal(parse_default_literal(&lit)?));
                } else {
                    default_spec = Some(DefaultSpec::Derive);
                }
            } else if meta.path.is_ident("default_fn") {
                default_count += 1;
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    default_spec = Some(DefaultSpec::Function(lit.value()));
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `default_fn`"));
                }
            } else if meta.path.is_ident("skip") {
                skip = true;
            } else if meta.path.is_ident("skip_serializing_if") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    skip_serializing_if = Some(lit.value());
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `skip_serializing_if`"));
                }
            } else if meta.path.is_ident("bool") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    bool_mode = Some(match lit.value().as_str() {
                        "presence+value" => BoolMode::PresenceAndValue,
                        "value-only" => BoolMode::ValueOnly,
                        "presence-only" => BoolMode::PresenceOnly,
                        other => {
                            return Err(syn::Error::new(lit.span(), format!("unknown bool mode: '{other}'")));
                        }
                    });
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `bool`"));
                }
            } else if meta.path.is_ident("flag_style") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    flag_style = Some(match lit.value().as_str() {
                        "both" => FlagStyle::Both,
                        "value|no" => FlagStyle::ValueNo,
                        "with|without" => FlagStyle::WithWithout,
                        other => {
                            return Err(syn::Error::new(lit.span(), format!("unknown flag_style: '{other}'")));
                        }
                    });
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `flag_style`"));
                }
            } else if meta.path.is_ident("conflict") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    conflict = Some(match lit.value().as_str() {
                        "error" => ConflictPolicy::Error,
                        "first" => ConflictPolicy::First,
                        "last" => ConflictPolicy::Last,
                        "append" => ConflictPolicy::Append,
                        other => {
                            return Err(syn::Error::new(lit.span(), format!("unknown conflict policy: '{other}'")));
                        }
                    });
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `conflict`"));
                }
            } else if meta.path.is_ident("render") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }) = value {
                    render = Some(match lit.value().as_str() {
                        "attr" => RenderPlacement::Attr,
                        "value" => RenderPlacement::Value,
                        "child" => RenderPlacement::Child,
                        "children" => RenderPlacement::Children,
                        "registry" => RenderPlacement::Registry,
                        other => {
                            return Err(syn::Error::new(lit.span(), format!("unknown render placement: '{other}'")));
                        }
                    });
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `render`"));
                }
            } else {
                return Err(syn::Error::new(meta.path.span(), format!("unknown field attribute: {}", path_to_string(&meta.path))));
            }
            Ok(())
        })?;
    }

    if default_count > 1 {
        return Err(syn::Error::new(primary_span, "field cannot have multiple default specifications (`default`, `default = ...`, `default_fn`)"));
    }

    if !has_kdl_attr {
        return Ok(Some(FieldAttrs {
            span: primary_span,
            placement,
            required,
            name,
            container,
            default: default_spec,
            skip_serializing_if,
            bool_mode,
            flag_style,
            conflict,
            render,
        }));
    }

    if skip {
        return Ok(None);
    }

    Ok(Some(FieldAttrs {
        span: primary_span,
        placement,
        required,
        name,
        container,
        default: default_spec,
        skip_serializing_if,
        bool_mode,
        flag_style,
        conflict,
        render,
    }))
}

fn parse_default_literal(lit: &Lit) -> syn::Result<DefaultLiteral> {
    match lit {
        Lit::Int(lit_int) => Ok(DefaultLiteral::Int(lit_int.base10_parse()?)),
        Lit::Float(lit_float) => Ok(DefaultLiteral::Float(lit_float.base10_parse()?)),
        Lit::Bool(lit_bool) => Ok(DefaultLiteral::Bool(lit_bool.value())),
        Lit::Str(lit_str) => Ok(DefaultLiteral::String(lit_str.value())),
        _ => Err(syn::Error::new(lit.span(), "default value must be an integer, float, boolean, or string literal")),
    }
}

fn path_to_string(path: &Path) -> String {
    path.segments.iter().map(|s| s.ident.to_string()).collect::<Vec<_>>().join("::")
}

pub fn is_option_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path.segments.last().map(|s| s.ident == "Option").unwrap_or(false),
        _ => false,
    }
}

pub fn is_vec_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path.segments.last().map(|s| s.ident == "Vec").unwrap_or(false),
        _ => false,
    }
}

pub fn is_hashmap_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path.segments.last().map(|s| s.ident == "HashMap").unwrap_or(false),
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
                    if let (Some(syn::GenericArgument::Type(key_ty)), Some(syn::GenericArgument::Type(val_ty))) =
                        (iter.next(), iter.next())
                    {
                        return Some((key_ty, val_ty));
                    }
                }
            }
        }
    }
    None
}

pub fn is_bool_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path.segments.last().map(|s| s.ident == "bool").unwrap_or(false),
        _ => false,
    }
}

pub fn is_string_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path.segments.last().map(|s| s.ident == "String").unwrap_or(false),
        _ => false,
    }
}

pub fn is_numeric_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| matches!(s.ident.to_string().as_str(), "i64" | "i32" | "u64" | "u32" | "usize" | "f64" | "f32"))
            .unwrap_or(false),
        _ => false,
    }
}

#[derive(Debug)]
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
    pub default: Option<DefaultSpec>,
    pub skip_serializing_if: Option<String>,
    pub bool_mode: Option<BoolMode>,
    pub flag_style: Option<FlagStyle>,
    pub conflict: Option<ConflictPolicy>,
    pub render: Option<RenderPlacement>,
    pub container: Option<String>,
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
        let is_option_vec = is_option_type(&field.ty) && extract_inner_type(&field.ty).map(is_vec_type).unwrap_or(false);
        let is_hashmap = is_hashmap_type(&field.ty);
        let is_bool = is_bool_type(&field.ty) || (is_optional && extract_inner_type(&field.ty).map(is_bool_type).unwrap_or(false));

        let required = match attrs.required {
            Some(true) => true,
            Some(false) => false,
            None => !is_optional && !is_option_vec,
        };

        let err_span = attrs.span;

        if attrs.placement.registry && !is_hashmap {
            return Err(syn::Error::new(err_span, "registry placement requires HashMap<K, V> field type"));
        }

        if attrs.placement.children && !is_vec && !is_option_vec {
            return Err(syn::Error::new(err_span, "children placement requires Vec<T> field type"));
        }

        if attrs.placement.flag.is_some() && !is_bool {
            return Err(syn::Error::new(err_span, "flag placement is only valid for bool fields"));
        }

        if let Some((pos, neg)) = &attrs.placement.flag {
            if pos.is_some() ^ neg.is_some() {
                return Err(syn::Error::new(err_span, "flag override requires both flag and neg_flag"));
            }
        }

        if matches!(attrs.conflict, Some(ConflictPolicy::Append)) && !is_vec && !is_option_vec {
            return Err(syn::Error::new(err_span, "append conflict policy is only valid for Vec<T> fields"));
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
            default: attrs.default,
            skip_serializing_if: attrs.skip_serializing_if,
            bool_mode: attrs.bool_mode,
            flag_style: attrs.flag_style,
            conflict: attrs.conflict,
            render: attrs.render,
            container: attrs.container,
        }))
    }

    pub fn inner_type(&self) -> Option<&Type> {
        extract_inner_type(&self.ty)
    }
}
