use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{Attribute, DataEnum, DeriveInput, Expr, ExprLit, ExprUnary, Fields, Lit, Type, UnOp};

use crate::attrs::{
    BoolMode, ConflictPolicy, DefaultPlacement, FieldInfo, FlagStyle, StructAttrs,
    extract_hashmap_types, extract_inner_type, extract_registry_vec_value, is_bool_type,
    is_numeric_type, is_option_type, is_string_type, parse_struct_attrs,
};

pub fn generate_schema_impl(input: &DeriveInput) -> syn::Result<TokenStream> {
    let struct_name = &input.ident;
    let struct_attrs = parse_struct_attrs(&input.attrs)?;

    match &input.data {
        syn::Data::Struct(data) => {
            let mut field_infos = Vec::new();
            match &data.fields {
                syn::Fields::Named(named) => {
                    for field in &named.named {
                        if let Some(info) = FieldInfo::from_field(field, struct_attrs.rename_all)? {
                            if !info.is_skipped {
                                field_infos.push(info);
                            }
                        }
                    }
                }
                syn::Fields::Unnamed(unnamed) => {
                    for (index, field) in unnamed.unnamed.iter().enumerate() {
                        if let Some(info) = FieldInfo::from_tuple_field(field, index)? {
                            if !info.is_skipped {
                                field_infos.push(info);
                            }
                        }
                    }
                }
                syn::Fields::Unit => {}
            }

            let schema_builder = generate_schema_builder(struct_name, &field_infos, &struct_attrs);

            Ok(quote! {
                impl ::kdl_config_runtime::schema::KdlSchema for #struct_name {
                    fn schema_ref() -> ::kdl_config_runtime::schema::SchemaRef {
                        ::kdl_config_runtime::schema::SchemaRef::Ref(stringify!(#struct_name).to_string())
                    }

                    fn register_definitions(registry: &mut ::kdl_config_runtime::schema::SchemaRegistry) {
                        let name = stringify!(#struct_name).to_string();
                        if registry.definitions.contains_key(&name) {
                            return;
                        }

                        // Insert placeholder to break cycles?
                        // Actually, we can just recurse first or insert after.
                        // Let's recurse first for dependencies, BUT we need to handle cycles.
                        // For now, we assume users handle cycles via Refs or we just insert self first.
                        // Let's insert a partial schema first?
                        // No, KDL schema registry is simple. We can just insert at end.
                        // But to prevent infinite recursion we need to check if we are already visiting.
                        // Since this is static code gen, we can't easily have a "visiting" set passed down.
                        // However, the registry HAS the definitions. So if we check existence first (which we did), we are good IF we insert BEFORE recursing?
                        // But we need the fields to build our schema.
                        // Hmmm.
                        // If we construct schema first, we need field schemas.
                        // Field schema_ref() is cheap (returns Ref or inline).
                        // Field register_definitions() recurses.

                        // So algorithm:
                        // 1. Insert placeholder/incomplete schema for Self to stop recursion.
                        // 2. Call register_definitions for all fields.
                        // 3. Build full schema for Self.
                        // 4. Update registry with full schema.

                        registry.definitions.insert(name.clone(), ::kdl_config_runtime::schema::KdlNodeSchema::default());

                        #schema_builder
                    }
                }
            })
        }
        syn::Data::Enum(data) => generate_schema_enum_impl(input, data, &struct_attrs),
        _ => Err(syn::Error::new_spanned(
            struct_name,
            "KdlSchema only supports named structs or enums for now",
        )),
    }
}

#[derive(Debug, Clone, Copy)]
enum SchemaFieldKind {
    Value,
    Node,
    Registry,
    Modifier,
}

fn is_value_type(ty: &syn::Type) -> bool {
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

fn field_kind(field: &FieldInfo) -> SchemaFieldKind {
    if field.is_modifier {
        return SchemaFieldKind::Modifier;
    }
    if field.placement.registry {
        return SchemaFieldKind::Registry;
    }

    if field.is_vec || field.is_option_vec {
        let inner = if field.is_option_vec {
            field.inner_type().and_then(extract_inner_type)
        } else {
            field.inner_type()
        };
        let is_value = inner.map(is_value_type).unwrap_or(false);
        if is_value {
            SchemaFieldKind::Value
        } else {
            SchemaFieldKind::Node
        }
    } else if is_value_type(&field.ty) {
        SchemaFieldKind::Value
    } else {
        SchemaFieldKind::Node
    }
}

fn registry_value_type(field: &FieldInfo) -> Option<&syn::Type> {
    if field.is_hashmap {
        extract_hashmap_types(&field.ty).map(|(_, val)| val)
    } else {
        extract_registry_vec_value(&field.ty).map(|(val, _)| val)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
enum LiteralTag {
    Int(i128),
    Float(f64),
    Bool(bool),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
enum VariantTag {
    String(String),
    Literal(LiteralTag),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TagSchema {
    String,
    Integer,
    Float,
    Boolean,
}

struct SchemaParts {
    register_calls: Vec<TokenStream>,
    prop_inserts: Vec<TokenStream>,
    value_entries: Vec<(usize, TokenStream)>,
    child_inserts: Vec<TokenStream>,
}

fn schema_type_expr(ty: &Type) -> TokenStream {
    quote! {{
        let schema_ref = <#ty as ::kdl_config_runtime::schema::KdlSchema>::schema_ref();
        match schema_ref {
            ::kdl_config_runtime::schema::SchemaRef::Inline(s) => {
                s.values
                    .first()
                    .map(|value| value.ty.clone())
                    .unwrap_or(::kdl_config_runtime::schema::SchemaType::String)
            }
            _ => ::kdl_config_runtime::schema::SchemaType::String,
        }
    }}
}

fn collect_schema_parts(fields: &[FieldInfo], struct_attrs: &StructAttrs) -> SchemaParts {
    let mut prop_inserts = Vec::new();
    let mut value_entries: Vec<(usize, TokenStream)> = Vec::new();
    let mut child_inserts = Vec::new();
    let mut register_calls = Vec::new();

    let default_placement = struct_attrs
        .default_placement
        .clone()
        .unwrap_or(DefaultPlacement::Exhaustive);

    let default_bool = struct_attrs
        .default_bool
        .clone()
        .unwrap_or(BoolMode::PresenceAndValue);
    let default_flag_style = struct_attrs
        .default_flag_style
        .clone()
        .unwrap_or(FlagStyle::Both);
    let default_conflict = struct_attrs
        .default_conflict
        .clone()
        .unwrap_or(ConflictPolicy::Error);

    let prop_insert = |ty: &Type,
                       kdl_key: &str,
                       variadic: bool,
                       required: bool,
                       bool_mode: TokenStream,
                       flag_style: TokenStream,
                       conflict: TokenStream|
     -> TokenStream {
        let type_expr = schema_type_expr(ty);
        quote! {
            let type_spec = #type_expr;
            let type_spec = if #variadic {
                ::kdl_config_runtime::schema::SchemaType::Variadic(::std::boxed::Box::new(type_spec))
            } else {
                type_spec
            };
            schema.props.insert(#kdl_key.to_string(), ::kdl_config_runtime::schema::SchemaProp {
                ty: type_spec,
                required: #required,
                bool_mode: #bool_mode,
                flag_style: #flag_style,
                conflict: #conflict,
            });
        }
    };

    let value_insert = |ty: &Type, required: bool| -> TokenStream {
        let type_expr = schema_type_expr(ty);
        quote! {
            let type_spec = #type_expr;
            schema.values.push(::kdl_config_runtime::schema::SchemaValue {
                ty: type_spec,
                required: #required,
            });
        }
    };

    let value_child_insert = |ty: &Type,
                              kdl_key: &str,
                              variadic: bool,
                              required: bool,
                              bool_mode: TokenStream|
     -> TokenStream {
        let type_expr = schema_type_expr(ty);
        quote! {
            let type_spec = #type_expr;
            let type_spec = if #variadic {
                ::kdl_config_runtime::schema::SchemaType::Variadic(::std::boxed::Box::new(type_spec))
            } else {
                type_spec
            };
            let mut node_schema = ::kdl_config_runtime::schema::KdlNodeSchema::default();
            node_schema.name = Some(#kdl_key.to_string());
            node_schema.values.push(::kdl_config_runtime::schema::SchemaValue { ty: type_spec, required: #required });
            node_schema.required = Some(#required);
            if let Some(bool_mode) = #bool_mode {
                node_schema.defaults.bool_mode = Some(bool_mode);
            }

            if children_nodes.is_none() {
                children_nodes = Some(::kdl_config_runtime::schema::ChildrenSchema { nodes: vec![] });
            }
            if let Some(c) = &mut children_nodes {
                c.nodes.push(::kdl_config_runtime::schema::SchemaRef::Inline(node_schema));
            }
        }
    };

    let node_child_insert = |ty: &Type, kdl_key: &str, required: bool| -> TokenStream {
        quote! {
            let schema_ref = <#ty as ::kdl_config_runtime::schema::KdlSchema>::schema_ref();
            let mut node_schema = ::kdl_config_runtime::schema::KdlNodeSchema::default();
            node_schema.name = Some(#kdl_key.to_string());
            node_schema.required = Some(#required);
            match schema_ref {
                ::kdl_config_runtime::schema::SchemaRef::Ref(r) => {
                    node_schema.ref_type = Some(r);
                }
                ::kdl_config_runtime::schema::SchemaRef::Inline(s) => {
                    node_schema.props = s.props;
                    node_schema.values = s.values;
                    node_schema.children = s.children;
                }
            }

            if children_nodes.is_none() {
                children_nodes = Some(::kdl_config_runtime::schema::ChildrenSchema { nodes: vec![] });
            }
            if let Some(c) = &mut children_nodes {
                c.nodes.push(::kdl_config_runtime::schema::SchemaRef::Inline(node_schema));
            }
        }
    };

    let registry_child_insert = |val_ty: &Type,
                                 container: &str,
                                 key_schema: Option<TokenStream>,
                                 required: bool|
     -> TokenStream {
        quote! {
            let schema_ref = <#val_ty as ::kdl_config_runtime::schema::KdlSchema>::schema_ref();
            let mut node_schema = ::kdl_config_runtime::schema::KdlNodeSchema::default();
            node_schema.name = Some(#container.to_string());
            node_schema.values.push(::kdl_config_runtime::schema::SchemaValue {
                ty: ::kdl_config_runtime::schema::SchemaType::String,
                required: #required,
            });
            node_schema.required = Some(#required);
            if let Some(key_schema) = #key_schema {
                node_schema.registry_key = Some(key_schema);
            }
            match schema_ref {
                ::kdl_config_runtime::schema::SchemaRef::Inline(s) => {
                    node_schema.props = s.props;
                    node_schema.values.extend(s.values);
                    node_schema.children = s.children;
                }
                ::kdl_config_runtime::schema::SchemaRef::Ref(r) => {
                    node_schema.ref_type = Some(r);
                }
            }

            if children_nodes.is_none() {
                children_nodes = Some(::kdl_config_runtime::schema::ChildrenSchema { nodes: vec![] });
            }
            if let Some(c) = &mut children_nodes {
                c.nodes.push(::kdl_config_runtime::schema::SchemaRef::Inline(node_schema));
            }
        }
    };

    for field in fields {
        if field.is_modifier || field.is_skipped {
            continue;
        }
        let required = field.required;
        let bool_mode = field
            .bool_mode
            .clone()
            .unwrap_or_else(|| default_bool.clone());
        let flag_style = field
            .flag_style
            .clone()
            .unwrap_or_else(|| default_flag_style.clone());
        let conflict = field
            .conflict
            .clone()
            .unwrap_or_else(|| default_conflict.clone());

        let bool_mode_expr = if field.is_bool {
            match bool_mode {
                BoolMode::PresenceAndValue => {
                    quote! { Some(::kdl_config_runtime::BoolMode::PresenceAndValue) }
                }
                BoolMode::ValueOnly => quote! { Some(::kdl_config_runtime::BoolMode::ValueOnly) },
                BoolMode::PresenceOnly => {
                    quote! { Some(::kdl_config_runtime::BoolMode::PresenceOnly) }
                }
            }
        } else {
            quote! { None }
        };
        let flag_style_expr = if field.is_bool {
            match flag_style {
                FlagStyle::Both => quote! { Some(::kdl_config_runtime::FlagStyle::Both) },
                FlagStyle::ValueNo => quote! { Some(::kdl_config_runtime::FlagStyle::ValueNo) },
                FlagStyle::WithWithout => {
                    quote! { Some(::kdl_config_runtime::FlagStyle::WithWithout) }
                }
            }
        } else {
            quote! { None }
        };
        let conflict_expr = match conflict {
            ConflictPolicy::Error => quote! { Some(::kdl_config_runtime::ConflictPolicy::Error) },
            ConflictPolicy::First => quote! { Some(::kdl_config_runtime::ConflictPolicy::First) },
            ConflictPolicy::Last => quote! { Some(::kdl_config_runtime::ConflictPolicy::Last) },
            ConflictPolicy::Append => quote! { Some(::kdl_config_runtime::ConflictPolicy::Append) },
        };

        let ty = &field.ty;
        let kdl_key = &field.kdl_key;

        register_calls.push(quote! {
            <#ty as ::kdl_config_runtime::schema::KdlSchema>::register_definitions(registry);
        });

        if field.placement.registry {
            let container = field.container.clone().unwrap_or_else(|| kdl_key.clone());
            let val_ty = registry_value_type(field).expect(
                "registry placement requires HashMap<String, V> or Vec<(String, V)> field type",
            );
            let default_key = crate::attrs::RegistryKey::Arg(0);
            let key_schema = Some(registry_key_schema_expr(
                field.registry_key.as_ref().unwrap_or(&default_key),
            ));
            child_inserts.push(registry_child_insert(
                val_ty, &container, key_schema, required,
            ));
            continue;
        }

        let has_explicit = field.placement.attr
            || field.placement.keyed
            || field.placement.positional.is_some()
            || field.placement.value
            || field.placement.child
            || field.placement.children;

        if has_explicit {
            if field.placement.attr || field.placement.keyed {
                let variadic = field.is_vec || field.is_option_vec;
                prop_inserts.push(prop_insert(
                    ty,
                    kdl_key,
                    variadic,
                    required,
                    bool_mode_expr.clone(),
                    flag_style_expr.clone(),
                    conflict_expr.clone(),
                ));
            }
            if let Some(index) = field.placement.positional {
                value_entries.push((index, value_insert(ty, required)));
            }
            if field.placement.value {
                match field_kind(field) {
                    SchemaFieldKind::Value => {
                        let variadic = field.is_vec || field.is_option_vec;
                        child_inserts.push(value_child_insert(
                            ty,
                            kdl_key,
                            variadic,
                            required,
                            bool_mode_expr.clone(),
                        ));
                    }
                    SchemaFieldKind::Node => {
                        child_inserts.push(node_child_insert(ty, kdl_key, required))
                    }
                    SchemaFieldKind::Registry | SchemaFieldKind::Modifier => {}
                }
            }
            if field.placement.child || field.placement.children {
                child_inserts.push(node_child_insert(ty, kdl_key, required));
            }
        } else {
            match field_kind(field) {
                SchemaFieldKind::Value => match default_placement {
                    DefaultPlacement::Exhaustive => {
                        let variadic = field.is_vec || field.is_option_vec;
                        prop_inserts.push(prop_insert(
                            ty,
                            kdl_key,
                            variadic,
                            required,
                            bool_mode_expr.clone(),
                            flag_style_expr.clone(),
                            conflict_expr.clone(),
                        ));
                        let variadic = field.is_vec || field.is_option_vec;
                        child_inserts.push(value_child_insert(
                            ty,
                            kdl_key,
                            variadic,
                            required,
                            bool_mode_expr.clone(),
                        ));
                    }
                    DefaultPlacement::Attr => {
                        let variadic = field.is_vec || field.is_option_vec;
                        prop_inserts.push(prop_insert(
                            ty,
                            kdl_key,
                            variadic,
                            required,
                            bool_mode_expr.clone(),
                            flag_style_expr.clone(),
                            conflict_expr.clone(),
                        ));
                    }
                    DefaultPlacement::Value | DefaultPlacement::Child => {
                        let variadic = field.is_vec || field.is_option_vec;
                        child_inserts.push(value_child_insert(
                            ty,
                            kdl_key,
                            variadic,
                            required,
                            bool_mode_expr.clone(),
                        ));
                    }
                },
                SchemaFieldKind::Node => match default_placement {
                    DefaultPlacement::Exhaustive | DefaultPlacement::Child => {
                        child_inserts.push(node_child_insert(ty, kdl_key, required));
                    }
                    DefaultPlacement::Attr | DefaultPlacement::Value => {}
                },
                SchemaFieldKind::Registry | SchemaFieldKind::Modifier => {
                    // already handled above
                }
            }
        }
    }

    SchemaParts {
        register_calls,
        prop_inserts,
        value_entries,
        child_inserts,
    }
}

fn render_schema_parts(parts: &SchemaParts) -> TokenStream {
    let prop_inserts = &parts.prop_inserts;
    let child_inserts = &parts.child_inserts;
    let mut value_entries = parts.value_entries.clone();
    value_entries.sort_by_key(|(idx, _)| *idx);
    let value_inserts = value_entries.into_iter().map(|(_, insert)| insert);

    quote! {
        // Props
        #(#prop_inserts)*

        // Values
        #(#value_inserts)*

        // Children
        let mut children_nodes: Option<::kdl_config_runtime::schema::ChildrenSchema> = None;
        #(#child_inserts)*
        schema.children = children_nodes.map(Box::new);
    }
}

fn generate_schema_builder(
    struct_name: &syn::Ident,
    fields: &[FieldInfo],
    struct_attrs: &crate::attrs::StructAttrs,
) -> TokenStream {
    let name_str = struct_name.to_string();
    let parts = collect_schema_parts(fields, struct_attrs);
    let node_name_assign = if let Some(n) = &struct_attrs.node_name {
        quote! { schema.name = Some(#n.to_string()); }
    } else {
        quote! {}
    };

    let schema_body = render_schema_parts(&parts);
    let register_calls = parts.register_calls;

    let deny_unknown = match struct_attrs.deny_unknown {
        Some(val) => quote! { Some(#val) },
        None => quote! { Some(false) },
    };

    let default_placement = match struct_attrs.default_placement {
        Some(DefaultPlacement::Exhaustive) => {
            quote! { Some(::kdl_config_runtime::DefaultPlacement::Exhaustive) }
        }
        Some(DefaultPlacement::Attr) => {
            quote! { Some(::kdl_config_runtime::DefaultPlacement::Attr) }
        }
        Some(DefaultPlacement::Value) => {
            quote! { Some(::kdl_config_runtime::DefaultPlacement::Value) }
        }
        Some(DefaultPlacement::Child) => {
            quote! { Some(::kdl_config_runtime::DefaultPlacement::Child) }
        }
        None => quote! { Some(::kdl_config_runtime::DefaultPlacement::Exhaustive) },
    };

    let default_bool = match struct_attrs.default_bool {
        Some(BoolMode::PresenceAndValue) => {
            quote! { Some(::kdl_config_runtime::BoolMode::PresenceAndValue) }
        }
        Some(BoolMode::ValueOnly) => quote! { Some(::kdl_config_runtime::BoolMode::ValueOnly) },
        Some(BoolMode::PresenceOnly) => {
            quote! { Some(::kdl_config_runtime::BoolMode::PresenceOnly) }
        }
        None => quote! { Some(::kdl_config_runtime::BoolMode::PresenceAndValue) },
    };

    let default_flag_style = match struct_attrs.default_flag_style {
        Some(FlagStyle::Both) => quote! { Some(::kdl_config_runtime::FlagStyle::Both) },
        Some(FlagStyle::ValueNo) => quote! { Some(::kdl_config_runtime::FlagStyle::ValueNo) },
        Some(FlagStyle::WithWithout) => {
            quote! { Some(::kdl_config_runtime::FlagStyle::WithWithout) }
        }
        None => quote! { Some(::kdl_config_runtime::FlagStyle::Both) },
    };

    let default_conflict = match struct_attrs.default_conflict {
        Some(ConflictPolicy::Error) => quote! { Some(::kdl_config_runtime::ConflictPolicy::Error) },
        Some(ConflictPolicy::First) => quote! { Some(::kdl_config_runtime::ConflictPolicy::First) },
        Some(ConflictPolicy::Last) => quote! { Some(::kdl_config_runtime::ConflictPolicy::Last) },
        Some(ConflictPolicy::Append) => {
            quote! { Some(::kdl_config_runtime::ConflictPolicy::Append) }
        }
        None => quote! { Some(::kdl_config_runtime::ConflictPolicy::Error) },
    };

    quote! {
        #(#register_calls)*

        let mut schema = ::kdl_config_runtime::schema::KdlNodeSchema::default();
        #node_name_assign
        schema.deny_unknown = #deny_unknown;
        schema.defaults.placement = #default_placement;
        schema.defaults.bool_mode = #default_bool;
        schema.defaults.flag_style = #default_flag_style;
        schema.defaults.conflict = #default_conflict;

        #schema_body

        registry.definitions.insert(#name_str.to_string(), schema);
    }
}

#[derive(Debug)]
struct VariantAttrs {
    name: Option<String>,
    tag: Option<VariantTag>,
}

impl Default for VariantAttrs {
    fn default() -> Self {
        Self {
            name: None,
            tag: None,
        }
    }
}

fn parse_variant_attrs(attrs: &[Attribute]) -> syn::Result<VariantAttrs> {
    let mut result = VariantAttrs::default();
    for attr in attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("name") || meta.path.is_ident("rename") {
                let value: Expr = meta.value()?.parse()?;
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(lit), ..
                }) = value
                {
                    result.name = Some(lit.value());
                } else {
                    return Err(syn::Error::new(
                        value.span(),
                        "expected string literal for `name`",
                    ));
                }
            } else if meta.path.is_ident("tag") {
                let value: Expr = meta.value()?.parse()?;
                let tag = parse_tag_expr(&value)?;
                result.tag = Some(tag);
            } else {
                return Err(syn::Error::new(
                    meta.path.span(),
                    "unknown enum variant attribute",
                ));
            }
            Ok(())
        })?;
    }
    Ok(result)
}

fn parse_tag_expr(expr: &Expr) -> syn::Result<VariantTag> {
    match expr {
        Expr::Lit(ExprLit { lit, .. }) => parse_tag_lit(lit),
        Expr::Unary(ExprUnary {
            op: UnOp::Neg(_),
            expr,
            ..
        }) => match &**expr {
            Expr::Lit(ExprLit { lit, .. }) => parse_negated_tag_lit(lit),
            other => Err(syn::Error::new(
                other.span(),
                "expected numeric literal for `tag`",
            )),
        },
        _ => Err(syn::Error::new(expr.span(), "expected literal for `tag`")),
    }
}

fn parse_tag_lit(lit: &Lit) -> syn::Result<VariantTag> {
    match lit {
        Lit::Str(lit) => Ok(VariantTag::String(lit.value())),
        Lit::Int(lit) => Ok(VariantTag::Literal(LiteralTag::Int(lit.base10_parse()?))),
        Lit::Float(lit) => Ok(VariantTag::Literal(LiteralTag::Float(lit.base10_parse()?))),
        Lit::Bool(lit) => Ok(VariantTag::Literal(LiteralTag::Bool(lit.value()))),
        _ => Err(syn::Error::new(
            lit.span(),
            "expected string, int, float, or bool literal for `tag`",
        )),
    }
}

fn parse_negated_tag_lit(lit: &Lit) -> syn::Result<VariantTag> {
    match lit {
        Lit::Int(lit) => Ok(VariantTag::Literal(LiteralTag::Int(
            -lit.base10_parse::<i128>()?,
        ))),
        Lit::Float(lit) => Ok(VariantTag::Literal(LiteralTag::Float(
            -lit.base10_parse::<f64>()?,
        ))),
        _ => Err(syn::Error::new(
            lit.span(),
            "expected numeric literal for `tag`",
        )),
    }
}

enum VariantSchemaKind {
    Unit,
    Newtype(Type),
    Tuple(Vec<Type>),
    Struct { fields: Vec<FieldInfo> },
}

struct VariantSchemaInfo {
    kind: VariantSchemaKind,
}

fn tag_schema_type(tags: &[VariantTag]) -> TagSchema {
    let mut has_string = false;
    let mut has_bool = false;
    let mut has_int = false;
    let mut has_float = false;

    for tag in tags {
        match tag {
            VariantTag::String(_) => has_string = true,
            VariantTag::Literal(LiteralTag::Bool(_)) => has_bool = true,
            VariantTag::Literal(LiteralTag::Int(_)) => has_int = true,
            VariantTag::Literal(LiteralTag::Float(_)) => has_float = true,
        }
    }

    if has_string && !(has_bool || has_int || has_float) {
        return TagSchema::String;
    }
    if has_bool && !(has_string || has_int || has_float) {
        return TagSchema::Boolean;
    }
    if (has_int || has_float) && !(has_string || has_bool) {
        return if has_float {
            TagSchema::Float
        } else {
            TagSchema::Integer
        };
    }

    if has_string {
        TagSchema::String
    } else if has_bool {
        TagSchema::Boolean
    } else if has_float {
        TagSchema::Float
    } else {
        TagSchema::Integer
    }
}

fn tag_schema_expr(tags: &[VariantTag]) -> TokenStream {
    match tag_schema_type(tags) {
        TagSchema::String => quote! { ::kdl_config_runtime::schema::SchemaType::String },
        TagSchema::Integer => quote! { ::kdl_config_runtime::schema::SchemaType::Integer },
        TagSchema::Float => quote! { ::kdl_config_runtime::schema::SchemaType::Float },
        TagSchema::Boolean => quote! { ::kdl_config_runtime::schema::SchemaType::Boolean },
    }
}

fn registry_key_schema_expr(key: &crate::attrs::RegistryKey) -> TokenStream {
    match key {
        crate::attrs::RegistryKey::Arg(index) => quote! {
            ::kdl_config_runtime::schema::RegistryKeySchema {
                source: ::kdl_config_runtime::schema::RegistryKeySource::Arg,
                arg_index: Some(#index),
                attr: None,
                func: None,
            }
        },
        crate::attrs::RegistryKey::Attr(name) => quote! {
            ::kdl_config_runtime::schema::RegistryKeySchema {
                source: ::kdl_config_runtime::schema::RegistryKeySource::Attr,
                arg_index: None,
                attr: Some(#name.to_string()),
                func: None,
            }
        },
        crate::attrs::RegistryKey::Function(path) => quote! {
            ::kdl_config_runtime::schema::RegistryKeySchema {
                source: ::kdl_config_runtime::schema::RegistryKeySource::Function,
                arg_index: None,
                attr: None,
                func: Some(#path.to_string()),
            }
        },
    }
}

fn generate_schema_enum_impl(
    input: &DeriveInput,
    data: &DataEnum,
    struct_attrs: &StructAttrs,
) -> syn::Result<TokenStream> {
    let enum_name = &input.ident;

    let mut variants: Vec<VariantSchemaInfo> = Vec::new();
    let mut tag_values: Vec<VariantTag> = Vec::new();

    for variant in &data.variants {
        let attrs = parse_variant_attrs(&variant.attrs)?;
        let kdl_name = attrs
            .name
            .unwrap_or_else(|| struct_attrs.rename_all.apply(&variant.ident.to_string()));
        let tag = attrs
            .tag
            .unwrap_or_else(|| VariantTag::String(kdl_name.clone()));

        let kind = match &variant.fields {
            Fields::Unit => VariantSchemaKind::Unit,
            Fields::Unnamed(fields) => {
                if fields.unnamed.len() == 1 {
                    VariantSchemaKind::Newtype(fields.unnamed.first().unwrap().ty.clone())
                } else {
                    let types = fields.unnamed.iter().map(|f| f.ty.clone()).collect();
                    VariantSchemaKind::Tuple(types)
                }
            }
            Fields::Named(fields) => {
                let mut field_infos = Vec::new();
                for field in &fields.named {
                    if let Some(info) = FieldInfo::from_field(field, struct_attrs.rename_all)? {
                        if !info.is_skipped {
                            field_infos.push(info);
                        }
                    }
                }
                if field_infos.iter().filter(|info| info.is_modifier).count() > 1 {
                    return Err(syn::Error::new_spanned(
                        variant,
                        "KdlSchema supports at most one #[kdl(modifier)] field per variant",
                    ));
                }
                VariantSchemaKind::Struct {
                    fields: field_infos,
                }
            }
        };

        tag_values.push(tag.clone());
        variants.push(VariantSchemaInfo { kind });
    }

    let tag_value_expr = tag_schema_expr(&tag_values);

    let node_name_assign = if let Some(n) = &struct_attrs.node_name {
        quote! { enum_schema.name = Some(#n.to_string()); }
    } else {
        quote! {}
    };

    let variant_registers: Vec<TokenStream> = variants
        .iter()
        .map(|variant| match &variant.kind {
            VariantSchemaKind::Unit => quote! {},
            VariantSchemaKind::Newtype(ty) => {
                quote! { <#ty as ::kdl_config_runtime::schema::KdlSchema>::register_definitions(registry); }
            }
            VariantSchemaKind::Tuple(types) => {
                let register_calls: Vec<TokenStream> = types
                    .iter()
                    .map(|ty| {
                        quote! { <#ty as ::kdl_config_runtime::schema::KdlSchema>::register_definitions(registry); }
                    })
                    .collect();
                quote! { #(#register_calls)* }
            }
            VariantSchemaKind::Struct { fields } => {
                let parts = collect_schema_parts(fields, struct_attrs);
                let register_calls = parts.register_calls;
                quote! { #(#register_calls)* }
            }
        })
        .collect();

    let deny_unknown = match struct_attrs.deny_unknown {
        Some(val) => quote! { Some(#val) },
        None => quote! { Some(false) },
    };

    let default_placement = match struct_attrs.default_placement {
        Some(DefaultPlacement::Exhaustive) => {
            quote! { Some(::kdl_config_runtime::DefaultPlacement::Exhaustive) }
        }
        Some(DefaultPlacement::Attr) => {
            quote! { Some(::kdl_config_runtime::DefaultPlacement::Attr) }
        }
        Some(DefaultPlacement::Value) => {
            quote! { Some(::kdl_config_runtime::DefaultPlacement::Value) }
        }
        Some(DefaultPlacement::Child) => {
            quote! { Some(::kdl_config_runtime::DefaultPlacement::Child) }
        }
        None => quote! { Some(::kdl_config_runtime::DefaultPlacement::Exhaustive) },
    };

    let default_bool = match struct_attrs.default_bool {
        Some(BoolMode::PresenceAndValue) => {
            quote! { Some(::kdl_config_runtime::BoolMode::PresenceAndValue) }
        }
        Some(BoolMode::ValueOnly) => quote! { Some(::kdl_config_runtime::BoolMode::ValueOnly) },
        Some(BoolMode::PresenceOnly) => {
            quote! { Some(::kdl_config_runtime::BoolMode::PresenceOnly) }
        }
        None => quote! { Some(::kdl_config_runtime::BoolMode::PresenceAndValue) },
    };

    let default_flag_style = match struct_attrs.default_flag_style {
        Some(FlagStyle::Both) => quote! { Some(::kdl_config_runtime::FlagStyle::Both) },
        Some(FlagStyle::ValueNo) => quote! { Some(::kdl_config_runtime::FlagStyle::ValueNo) },
        Some(FlagStyle::WithWithout) => {
            quote! { Some(::kdl_config_runtime::FlagStyle::WithWithout) }
        }
        None => quote! { Some(::kdl_config_runtime::FlagStyle::Both) },
    };

    let default_conflict = match struct_attrs.default_conflict {
        Some(ConflictPolicy::Error) => quote! { Some(::kdl_config_runtime::ConflictPolicy::Error) },
        Some(ConflictPolicy::First) => quote! { Some(::kdl_config_runtime::ConflictPolicy::First) },
        Some(ConflictPolicy::Last) => quote! { Some(::kdl_config_runtime::ConflictPolicy::Last) },
        Some(ConflictPolicy::Append) => {
            quote! { Some(::kdl_config_runtime::ConflictPolicy::Append) }
        }
        None => quote! { Some(::kdl_config_runtime::ConflictPolicy::Error) },
    };

    Ok(quote! {
        impl ::kdl_config_runtime::schema::KdlSchema for #enum_name {
            fn schema_ref() -> ::kdl_config_runtime::schema::SchemaRef {
                ::kdl_config_runtime::schema::SchemaRef::Ref(stringify!(#enum_name).to_string())
            }

            fn register_definitions(registry: &mut ::kdl_config_runtime::schema::SchemaRegistry) {
                let name = stringify!(#enum_name).to_string();
                if registry.definitions.contains_key(&name) {
                    return;
                }

                registry.definitions.insert(name.clone(), ::kdl_config_runtime::schema::KdlNodeSchema::default());

                let mut enum_schema = ::kdl_config_runtime::schema::KdlNodeSchema::default();
                #node_name_assign
                enum_schema.deny_unknown = #deny_unknown;
                enum_schema.defaults.placement = #default_placement;
                enum_schema.defaults.bool_mode = #default_bool;
                enum_schema.defaults.flag_style = #default_flag_style;
                enum_schema.defaults.conflict = #default_conflict;
                enum_schema.values.push(::kdl_config_runtime::schema::SchemaValue { ty: #tag_value_expr, required: true });

                #(#variant_registers)*

                registry.definitions.insert(name, enum_schema);
            }
        }
    })
}
