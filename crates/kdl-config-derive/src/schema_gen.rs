use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{Attribute, DataEnum, DeriveInput, Expr, ExprLit, ExprUnary, Fields, Lit, Type, UnOp};

use crate::attrs::{
    BoolMode, ConflictPolicy, DefaultPlacement, FieldInfo, FlagStyle, StructAttrs,
    SchemaTypeOverride, extract_children_map_types, extract_hashmap_types, extract_inner_type,
    extract_registry_vec_value, is_bool_type, is_numeric_type, is_option_type, is_string_type,
    parse_field_attrs, parse_struct_attrs,
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
                            if info.is_skipped || info.schema.skip {
                                continue;
                            }
                            field_infos.push(info);
                        }
                    }
                }
                syn::Fields::Unnamed(unnamed) => {
                    for (index, field) in unnamed.unnamed.iter().enumerate() {
                        if let Some(info) = FieldInfo::from_tuple_field(field, index)? {
                            if info.is_skipped || info.schema.skip {
                                continue;
                            }
                            field_infos.push(info);
                        }
                    }
                }
                syn::Fields::Unit => {}
            }

            let schema_builder = generate_schema_builder(struct_name, &field_infos, &struct_attrs);

            Ok(quote! {
                impl ::kdl_config::schema::KdlSchema for #struct_name {
                    fn schema_ref() -> ::kdl_config::schema::SchemaRef {
                        ::kdl_config::schema::SchemaRef::Ref(stringify!(#struct_name).to_string())
                    }

                    fn register_definitions(registry: &mut ::kdl_config::schema::SchemaRegistry) {
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

                        registry.definitions.insert(name.clone(), ::kdl_config::schema::KdlNodeSchema::default());

                        #schema_builder
                    }
                }
            })
        }
        syn::Data::Enum(data) => generate_schema_enum_impl(input, data, &struct_attrs),
        syn::Data::Union(data) => generate_schema_union_impl(input, data, &struct_attrs),
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
        let is_value = inner.map(is_value_type).unwrap_or(false) || field.is_scalar;
        if is_value {
            SchemaFieldKind::Value
        } else {
            SchemaFieldKind::Node
        }
    } else if is_value_type(&field.ty) || field.is_scalar {
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
        let schema_ref = <#ty as ::kdl_config::schema::KdlSchema>::schema_ref();
        match schema_ref {
            ::kdl_config::schema::SchemaRef::Inline(s) => {
                s.values
                    .first()
                    .map(|value| value.ty.clone())
                    .unwrap_or(::kdl_config::schema::SchemaType::String)
            }
            _ => ::kdl_config::schema::SchemaType::String,
        }
    }}
}

fn schema_type_override_expr(override_ty: &SchemaTypeOverride) -> TokenStream {
    match override_ty {
        SchemaTypeOverride::String => quote! { ::kdl_config::schema::SchemaType::String },
        SchemaTypeOverride::Integer => quote! { ::kdl_config::schema::SchemaType::Integer },
        SchemaTypeOverride::Float => quote! { ::kdl_config::schema::SchemaType::Float },
        SchemaTypeOverride::Boolean => quote! { ::kdl_config::schema::SchemaType::Boolean },
        SchemaTypeOverride::Null => quote! { ::kdl_config::schema::SchemaType::Null },
    }
}

fn schema_type_expr_with_override(
    ty: &Type,
    override_ty: Option<&SchemaTypeOverride>,
) -> TokenStream {
    match override_ty {
        Some(override_ty) => schema_type_override_expr(override_ty),
        None => schema_type_expr(ty),
    }
}

fn schema_desc_expr(desc: Option<&String>) -> TokenStream {
    match desc {
        Some(desc) => quote! { Some(#desc.to_string()) },
        None => quote! { None },
    }
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

    let prop_insert = |type_expr: TokenStream,
                       kdl_key: &str,
                       variadic: bool,
                       required: bool,
                       bool_mode: TokenStream,
                       flag_style: TokenStream,
                       conflict: TokenStream,
                       description: TokenStream|
     -> TokenStream {
        quote! {
            let type_spec = #type_expr;
            let type_spec = if #variadic {
                ::kdl_config::schema::SchemaType::Variadic(::std::boxed::Box::new(type_spec))
            } else {
                type_spec
            };
            schema.props.insert(#kdl_key.to_string(), ::kdl_config::schema::SchemaProp {
                ty: type_spec,
                required: #required,
                bool_mode: #bool_mode,
                flag_style: #flag_style,
                conflict: #conflict,
                description: #description,
            });
        }
    };

    let value_insert =
        |type_expr: TokenStream, required: bool, description: TokenStream| -> TokenStream {
        quote! {
            let type_spec = #type_expr;
            schema.values.push(::kdl_config::schema::SchemaValue {
                ty: type_spec,
                required: #required,
                description: #description,
                enum_values: None,
            });
        }
    };

    let value_child_insert = |type_expr: TokenStream,
                              kdl_key: &str,
                              variadic: bool,
                              required: bool,
                              bool_mode: TokenStream,
                              description: TokenStream|
     -> TokenStream {
        quote! {
            let type_spec = #type_expr;
            let type_spec = if #variadic {
                ::kdl_config::schema::SchemaType::Variadic(::std::boxed::Box::new(type_spec))
            } else {
                type_spec
            };
            let mut node_schema = ::kdl_config::schema::KdlNodeSchema::default();
            node_schema.name = Some(#kdl_key.to_string());
            node_schema.description = #description;
            node_schema.values.push(::kdl_config::schema::SchemaValue {
                ty: type_spec,
                required: #required,
                description: None,
                enum_values: None,
            });
            node_schema.required = Some(#required);
            if let Some(bool_mode) = #bool_mode {
                node_schema.defaults.bool_mode = Some(bool_mode);
            }

            if children_nodes.is_none() {
                children_nodes = Some(::kdl_config::schema::ChildrenSchema { nodes: vec![] });
            }
            if let Some(c) = &mut children_nodes {
                c.nodes.push(::kdl_config::schema::SchemaRef::Inline(node_schema));
            }
        }
    };

    let node_child_insert = |ty: &Type,
                             kdl_key: &str,
                             required: bool,
                             description: TokenStream|
     -> TokenStream {
        quote! {
            let schema_ref = <#ty as ::kdl_config::schema::KdlSchema>::schema_ref();
            let mut node_schema = ::kdl_config::schema::KdlNodeSchema::default();
            node_schema.name = Some(#kdl_key.to_string());
            node_schema.description = #description;
            node_schema.required = Some(#required);
            match schema_ref {
                ::kdl_config::schema::SchemaRef::Ref(r) => {
                    node_schema.ref_type = Some(r);
                }
                ::kdl_config::schema::SchemaRef::Inline(s) => {
                    node_schema.props = s.props;
                    node_schema.values = s.values;
                    node_schema.children = s.children;
                }
                ::kdl_config::schema::SchemaRef::Choice(choices) => {
                    node_schema.children = Some(::std::boxed::Box::new(
                        ::kdl_config::schema::ChildrenSchema {
                            nodes: vec![::kdl_config::schema::SchemaRef::Choice(choices)],
                        },
                    ));
                }
            }

            if children_nodes.is_none() {
                children_nodes = Some(::kdl_config::schema::ChildrenSchema { nodes: vec![] });
            }
            if let Some(c) = &mut children_nodes {
                c.nodes.push(::kdl_config::schema::SchemaRef::Inline(node_schema));
            }
        }
    };

    let registry_child_insert = |val_ty: &Type,
                                 container: &str,
                                 key_schema: Option<TokenStream>,
                                 required: bool,
                                 description: TokenStream|
     -> TokenStream {
        quote! {
            let schema_ref = <#val_ty as ::kdl_config::schema::KdlSchema>::schema_ref();
            let mut node_schema = ::kdl_config::schema::KdlNodeSchema::default();
            node_schema.name = Some(#container.to_string());
            node_schema.description = #description;
            node_schema.values.push(::kdl_config::schema::SchemaValue {
                ty: ::kdl_config::schema::SchemaType::String,
                required: #required,
                description: None,
                enum_values: None,
            });
            node_schema.required = Some(#required);
            if let Some(key_schema) = #key_schema {
                node_schema.registry_key = Some(key_schema);
            }
            match schema_ref {
                ::kdl_config::schema::SchemaRef::Inline(s) => {
                    node_schema.props = s.props;
                    node_schema.values.extend(s.values);
                    node_schema.children = s.children;
                }
                ::kdl_config::schema::SchemaRef::Ref(r) => {
                    node_schema.ref_type = Some(r);
                }
                ::kdl_config::schema::SchemaRef::Choice(choices) => {
                    node_schema.children = Some(::std::boxed::Box::new(
                        ::kdl_config::schema::ChildrenSchema {
                            nodes: vec![::kdl_config::schema::SchemaRef::Choice(choices)],
                        },
                    ));
                }
            }

            if children_nodes.is_none() {
                children_nodes = Some(::kdl_config::schema::ChildrenSchema { nodes: vec![] });
            }
            if let Some(c) = &mut children_nodes {
                c.nodes.push(::kdl_config::schema::SchemaRef::Inline(node_schema));
            }
        }
    };

    let children_map_child_insert = |val_ty: &Type,
                                     key_ty: Option<&Type>,
                                     node_name: &str,
                                     registry_key: Option<TokenStream>,
                                     required: bool,
                                     description: TokenStream|
     -> TokenStream {
        let key_schema = key_ty.map(schema_type_expr);
        quote! {
            let schema_ref = <#val_ty as ::kdl_config::schema::KdlSchema>::schema_ref();
            let mut node_schema = ::kdl_config::schema::KdlNodeSchema::default();
            node_schema.name = Some(#node_name.to_string());
            node_schema.description = #description;
            node_schema.required = Some(#required);
            if let Some(key_schema) = #key_schema {
                node_schema.values.push(::kdl_config::schema::SchemaValue {
                    ty: key_schema,
                    required: #required,
                    description: None,
                    enum_values: None,
                });
            }
            if let Some(registry_key) = #registry_key {
                node_schema.registry_key = Some(registry_key);
            }
            match schema_ref {
                ::kdl_config::schema::SchemaRef::Inline(s) => {
                    node_schema.props = s.props;
                    node_schema.values.extend(s.values);
                    node_schema.children = s.children;
                }
                ::kdl_config::schema::SchemaRef::Ref(r) => {
                    node_schema.ref_type = Some(r);
                }
                ::kdl_config::schema::SchemaRef::Choice(choices) => {
                    node_schema.children = Some(::std::boxed::Box::new(
                        ::kdl_config::schema::ChildrenSchema {
                            nodes: vec![::kdl_config::schema::SchemaRef::Choice(choices)],
                        },
                    ));
                }
            }

            if children_nodes.is_none() {
                children_nodes = Some(::kdl_config::schema::ChildrenSchema { nodes: vec![] });
            }
            if let Some(c) = &mut children_nodes {
                c.nodes.push(::kdl_config::schema::SchemaRef::Inline(node_schema));
            }
        }
    };

    for field in fields {
        if field.is_modifier || field.is_skipped || field.schema.skip {
            continue;
        }
        let required = field.schema.required.unwrap_or(field.required);
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
                    quote! { Some(::kdl_config::BoolMode::PresenceAndValue) }
                }
                BoolMode::ValueOnly => quote! { Some(::kdl_config::BoolMode::ValueOnly) },
                BoolMode::PresenceOnly => {
                    quote! { Some(::kdl_config::BoolMode::PresenceOnly) }
                }
            }
        } else {
            quote! { None }
        };
        let flag_style_expr = if field.is_bool {
            match flag_style {
                FlagStyle::Both => quote! { Some(::kdl_config::FlagStyle::Both) },
                FlagStyle::ValueNo => quote! { Some(::kdl_config::FlagStyle::ValueNo) },
                FlagStyle::WithWithout => {
                    quote! { Some(::kdl_config::FlagStyle::WithWithout) }
                }
            }
        } else {
            quote! { None }
        };
        let conflict_expr = match conflict {
            ConflictPolicy::Error => quote! { Some(::kdl_config::ConflictPolicy::Error) },
            ConflictPolicy::First => quote! { Some(::kdl_config::ConflictPolicy::First) },
            ConflictPolicy::Last => quote! { Some(::kdl_config::ConflictPolicy::Last) },
            ConflictPolicy::Append => quote! { Some(::kdl_config::ConflictPolicy::Append) },
        };

        let ty = &field.ty;
        let kdl_key = field
            .schema
            .name
            .clone()
            .unwrap_or_else(|| field.kdl_key.clone());
        let desc_expr = schema_desc_expr(field.schema.description.as_ref());
        let type_expr = if field.is_scalar && field.schema.ty.is_none() {
            quote! { ::kdl_config::schema::SchemaType::String }
        } else {
            schema_type_expr_with_override(ty, field.schema.ty.as_ref())
        };

        if field.schema.ty.is_none() && !field.is_scalar {
            register_calls.push(quote! {
                <#ty as ::kdl_config::schema::KdlSchema>::register_definitions(registry);
            });
        }

        if field.placement.registry {
            let container = if field.schema.name.is_some() {
                kdl_key.clone()
            } else {
                field.container.clone().unwrap_or_else(|| kdl_key.clone())
            };
            let val_ty = registry_value_type(field).expect(
                "registry placement requires HashMap<String, V> or Vec<(String, V)> field type",
            );
            let default_key = crate::attrs::RegistryKey::Arg(0);
            let key_schema = Some(registry_key_schema_expr(
                field.registry_key.as_ref().unwrap_or(&default_key),
            ));
            child_inserts.push(registry_child_insert(
                val_ty, &container, key_schema, required, desc_expr.clone(),
            ));
            continue;
        }

        if field.children_map {
            let (_kind, key_ty, val_ty) = extract_children_map_types(&field.ty)
                .expect("children_map placement requires HashMap<K, V> or Vec<(K, V)>");
            register_calls.push(quote! {
                <#val_ty as ::kdl_config::schema::KdlSchema>::register_definitions(registry);
            });

            let node_name = field
                .map_node
                .clone()
                .unwrap_or_else(|| "*".to_string());
            let key_ty = field.map_node.as_ref().map(|_| key_ty);
            let key_schema = if field.map_node.is_some() {
                let default_key = crate::attrs::RegistryKey::Arg(0);
                Some(registry_key_schema_expr(
                    field.registry_key.as_ref().unwrap_or(&default_key),
                ))
            } else {
                None
            };
            child_inserts.push(children_map_child_insert(
                val_ty,
                key_ty,
                &node_name,
                key_schema,
                required,
                desc_expr.clone(),
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
                    type_expr.clone(),
                    &kdl_key,
                    variadic,
                    required,
                    bool_mode_expr.clone(),
                    flag_style_expr.clone(),
                    conflict_expr.clone(),
                    desc_expr.clone(),
                ));
            }
            if let Some(index) = field.placement.positional {
                value_entries.push((
                    index,
                    value_insert(type_expr.clone(), required, desc_expr.clone()),
                ));
            }
            if field.placement.value {
                match field_kind(field) {
                    SchemaFieldKind::Value => {
                        let variadic = field.is_vec || field.is_option_vec;
                        child_inserts.push(value_child_insert(
                            type_expr.clone(),
                            &kdl_key,
                            variadic,
                            required,
                            bool_mode_expr.clone(),
                            desc_expr.clone(),
                        ));
                    }
                    SchemaFieldKind::Node => {
                        child_inserts.push(node_child_insert(
                            ty,
                            &kdl_key,
                            required,
                            desc_expr.clone(),
                        ))
                    }
                    SchemaFieldKind::Registry | SchemaFieldKind::Modifier => {}
                }
            }
            if field.placement.child || field.placement.children {
                child_inserts.push(node_child_insert(
                    ty,
                    &kdl_key,
                    required,
                    desc_expr.clone(),
                ));
            }
        } else {
            match field_kind(field) {
                SchemaFieldKind::Value => match default_placement {
                    DefaultPlacement::Exhaustive => {
                        let variadic = field.is_vec || field.is_option_vec;
                        prop_inserts.push(prop_insert(
                            type_expr.clone(),
                            &kdl_key,
                            variadic,
                            required,
                            bool_mode_expr.clone(),
                            flag_style_expr.clone(),
                            conflict_expr.clone(),
                            desc_expr.clone(),
                        ));
                        let variadic = field.is_vec || field.is_option_vec;
                        child_inserts.push(value_child_insert(
                            type_expr.clone(),
                            &kdl_key,
                            variadic,
                            required,
                            bool_mode_expr.clone(),
                            desc_expr.clone(),
                        ));
                    }
                    DefaultPlacement::Attr => {
                        let variadic = field.is_vec || field.is_option_vec;
                        prop_inserts.push(prop_insert(
                            type_expr.clone(),
                            &kdl_key,
                            variadic,
                            required,
                            bool_mode_expr.clone(),
                            flag_style_expr.clone(),
                            conflict_expr.clone(),
                            desc_expr.clone(),
                        ));
                    }
                    DefaultPlacement::Value | DefaultPlacement::Child => {
                        let variadic = field.is_vec || field.is_option_vec;
                        child_inserts.push(value_child_insert(
                            type_expr.clone(),
                            &kdl_key,
                            variadic,
                            required,
                            bool_mode_expr.clone(),
                            desc_expr.clone(),
                        ));
                    }
                },
                SchemaFieldKind::Node => match default_placement {
                    DefaultPlacement::Exhaustive | DefaultPlacement::Child => {
                        child_inserts.push(node_child_insert(
                            ty,
                            &kdl_key,
                            required,
                            desc_expr.clone(),
                        ));
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

fn collect_schema_parts_with_offset(
    fields: &[FieldInfo],
    struct_attrs: &StructAttrs,
    value_offset: usize,
) -> SchemaParts {
    let mut parts = collect_schema_parts(fields, struct_attrs);
    if value_offset > 0 {
        for (idx, _) in parts.value_entries.iter_mut() {
            *idx += value_offset;
        }
    }
    parts
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
        let mut children_nodes: Option<::kdl_config::schema::ChildrenSchema> = None;
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
    let node_name_assign = if let Some(n) = struct_attrs
        .schema
        .name
        .as_ref()
        .or(struct_attrs.node_name.as_ref())
    {
        quote! { schema.name = Some(#n.to_string()); }
    } else {
        quote! {}
    };

    let schema_body = render_schema_parts(&parts);
    let register_calls = parts.register_calls;

    let deny_unknown = match struct_attrs.schema.deny_unknown.or(struct_attrs.deny_unknown) {
        Some(val) => quote! { Some(#val) },
        None => quote! { Some(false) },
    };

    let schema_description = schema_desc_expr(struct_attrs.schema.description.as_ref());

    let default_placement = match struct_attrs.default_placement {
        Some(DefaultPlacement::Exhaustive) => {
            quote! { Some(::kdl_config::DefaultPlacement::Exhaustive) }
        }
        Some(DefaultPlacement::Attr) => {
            quote! { Some(::kdl_config::DefaultPlacement::Attr) }
        }
        Some(DefaultPlacement::Value) => {
            quote! { Some(::kdl_config::DefaultPlacement::Value) }
        }
        Some(DefaultPlacement::Child) => {
            quote! { Some(::kdl_config::DefaultPlacement::Child) }
        }
        None => quote! { Some(::kdl_config::DefaultPlacement::Exhaustive) },
    };

    let default_bool = match struct_attrs.default_bool {
        Some(BoolMode::PresenceAndValue) => {
            quote! { Some(::kdl_config::BoolMode::PresenceAndValue) }
        }
        Some(BoolMode::ValueOnly) => quote! { Some(::kdl_config::BoolMode::ValueOnly) },
        Some(BoolMode::PresenceOnly) => {
            quote! { Some(::kdl_config::BoolMode::PresenceOnly) }
        }
        None => quote! { Some(::kdl_config::BoolMode::PresenceAndValue) },
    };

    let default_flag_style = match struct_attrs.default_flag_style {
        Some(FlagStyle::Both) => quote! { Some(::kdl_config::FlagStyle::Both) },
        Some(FlagStyle::ValueNo) => quote! { Some(::kdl_config::FlagStyle::ValueNo) },
        Some(FlagStyle::WithWithout) => {
            quote! { Some(::kdl_config::FlagStyle::WithWithout) }
        }
        None => quote! { Some(::kdl_config::FlagStyle::Both) },
    };

    let default_conflict = match struct_attrs.default_conflict {
        Some(ConflictPolicy::Error) => quote! { Some(::kdl_config::ConflictPolicy::Error) },
        Some(ConflictPolicy::First) => quote! { Some(::kdl_config::ConflictPolicy::First) },
        Some(ConflictPolicy::Last) => quote! { Some(::kdl_config::ConflictPolicy::Last) },
        Some(ConflictPolicy::Append) => {
            quote! { Some(::kdl_config::ConflictPolicy::Append) }
        }
        None => quote! { Some(::kdl_config::ConflictPolicy::Error) },
    };

    quote! {
        #(#register_calls)*

        let mut schema = ::kdl_config::schema::KdlNodeSchema::default();
        #node_name_assign
        schema.description = #schema_description;
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
    tag: VariantTag,
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
        TagSchema::String => quote! { ::kdl_config::schema::SchemaType::String },
        TagSchema::Integer => quote! { ::kdl_config::schema::SchemaType::Integer },
        TagSchema::Float => quote! { ::kdl_config::schema::SchemaType::Float },
        TagSchema::Boolean => quote! { ::kdl_config::schema::SchemaType::Boolean },
    }
}

fn variant_tag_schema_expr(tag: &VariantTag) -> TokenStream {
    match tag {
        VariantTag::String(_) => quote! { ::kdl_config::schema::SchemaType::String },
        VariantTag::Literal(LiteralTag::Int(_)) => {
            quote! { ::kdl_config::schema::SchemaType::Integer }
        }
        VariantTag::Literal(LiteralTag::Float(_)) => {
            quote! { ::kdl_config::schema::SchemaType::Float }
        }
        VariantTag::Literal(LiteralTag::Bool(_)) => {
            quote! { ::kdl_config::schema::SchemaType::Boolean }
        }
    }
}

fn variant_tag_literal_expr(tag: &VariantTag) -> TokenStream {
    match tag {
        VariantTag::String(s) => {
            quote! { ::kdl_config::schema::SchemaLiteral::String(#s.to_string()) }
        }
        VariantTag::Literal(LiteralTag::Int(n)) => {
            quote! { ::kdl_config::schema::SchemaLiteral::Int(#n) }
        }
        VariantTag::Literal(LiteralTag::Float(f)) => {
            quote! { ::kdl_config::schema::SchemaLiteral::Float(#f) }
        }
        VariantTag::Literal(LiteralTag::Bool(b)) => {
            quote! { ::kdl_config::schema::SchemaLiteral::Bool(#b) }
        }
    }
}

fn registry_key_schema_expr(key: &crate::attrs::RegistryKey) -> TokenStream {
    match key {
        crate::attrs::RegistryKey::Arg(index) => quote! {
            ::kdl_config::schema::RegistryKeySchema {
                source: ::kdl_config::schema::RegistryKeySource::Arg,
                arg_index: Some(#index),
                attr: None,
                func: None,
            }
        },
        crate::attrs::RegistryKey::Attr(name) => quote! {
            ::kdl_config::schema::RegistryKeySchema {
                source: ::kdl_config::schema::RegistryKeySource::Attr,
                arg_index: None,
                attr: Some(#name.to_string()),
                func: None,
            }
        },
        crate::attrs::RegistryKey::Function(path) => quote! {
            ::kdl_config::schema::RegistryKeySchema {
                source: ::kdl_config::schema::RegistryKeySource::Function,
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
                        if info.is_skipped || info.schema.skip {
                            continue;
                        }
                        field_infos.push(info);
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
        variants.push(VariantSchemaInfo { kind, tag });
    }

    let tag_value_expr = tag_schema_expr(&tag_values);
    let enum_literals: Vec<TokenStream> = tag_values
        .iter()
        .map(variant_tag_literal_expr)
        .collect();

    let node_name_assign = if let Some(n) = struct_attrs
        .schema
        .name
        .as_ref()
        .or(struct_attrs.node_name.as_ref())
    {
        quote! { enum_schema.name = Some(#n.to_string()); }
    } else {
        quote! {}
    };

    let variant_registers: Vec<TokenStream> = variants
        .iter()
        .map(|variant| match &variant.kind {
            VariantSchemaKind::Unit => quote! {},
            VariantSchemaKind::Newtype(ty) => {
                quote! { <#ty as ::kdl_config::schema::KdlSchema>::register_definitions(registry); }
            }
            VariantSchemaKind::Tuple(types) => {
                let register_calls: Vec<TokenStream> = types
                    .iter()
                    .map(|ty| {
                        quote! { <#ty as ::kdl_config::schema::KdlSchema>::register_definitions(registry); }
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

    let deny_unknown = match struct_attrs.schema.deny_unknown.or(struct_attrs.deny_unknown) {
        Some(val) => quote! { Some(#val) },
        None => quote! { Some(false) },
    };

    let schema_description = schema_desc_expr(struct_attrs.schema.description.as_ref());

    let default_placement = match struct_attrs.default_placement {
        Some(DefaultPlacement::Exhaustive) => {
            quote! { Some(::kdl_config::DefaultPlacement::Exhaustive) }
        }
        Some(DefaultPlacement::Attr) => {
            quote! { Some(::kdl_config::DefaultPlacement::Attr) }
        }
        Some(DefaultPlacement::Value) => {
            quote! { Some(::kdl_config::DefaultPlacement::Value) }
        }
        Some(DefaultPlacement::Child) => {
            quote! { Some(::kdl_config::DefaultPlacement::Child) }
        }
        None => quote! { Some(::kdl_config::DefaultPlacement::Exhaustive) },
    };

    let default_bool = match struct_attrs.default_bool {
        Some(BoolMode::PresenceAndValue) => {
            quote! { Some(::kdl_config::BoolMode::PresenceAndValue) }
        }
        Some(BoolMode::ValueOnly) => quote! { Some(::kdl_config::BoolMode::ValueOnly) },
        Some(BoolMode::PresenceOnly) => {
            quote! { Some(::kdl_config::BoolMode::PresenceOnly) }
        }
        None => quote! { Some(::kdl_config::BoolMode::PresenceAndValue) },
    };

    let default_flag_style = match struct_attrs.default_flag_style {
        Some(FlagStyle::Both) => quote! { Some(::kdl_config::FlagStyle::Both) },
        Some(FlagStyle::ValueNo) => quote! { Some(::kdl_config::FlagStyle::ValueNo) },
        Some(FlagStyle::WithWithout) => {
            quote! { Some(::kdl_config::FlagStyle::WithWithout) }
        }
        None => quote! { Some(::kdl_config::FlagStyle::Both) },
    };

    let default_conflict = match struct_attrs.default_conflict {
        Some(ConflictPolicy::Error) => quote! { Some(::kdl_config::ConflictPolicy::Error) },
        Some(ConflictPolicy::First) => quote! { Some(::kdl_config::ConflictPolicy::First) },
        Some(ConflictPolicy::Last) => quote! { Some(::kdl_config::ConflictPolicy::Last) },
        Some(ConflictPolicy::Append) => {
            quote! { Some(::kdl_config::ConflictPolicy::Append) }
        }
        None => quote! { Some(::kdl_config::ConflictPolicy::Error) },
    };

    let variant_name_assign = if let Some(n) = struct_attrs
        .schema
        .name
        .as_ref()
        .or(struct_attrs.node_name.as_ref())
    {
        quote! { schema.name = Some(#n.to_string()); }
    } else {
        quote! {}
    };

    let variant_nodes: Vec<TokenStream> = variants
        .iter()
        .map(|variant| {
            let tag_expr = variant_tag_schema_expr(&variant.tag);
            let tag_literal = variant_tag_literal_expr(&variant.tag);
            match &variant.kind {
                VariantSchemaKind::Unit => {
                    quote! {
                        {
                            let mut schema = ::kdl_config::schema::KdlNodeSchema::default();
                            #variant_name_assign
                            schema.deny_unknown = #deny_unknown;
                            schema.defaults.placement = #default_placement;
                            schema.defaults.bool_mode = #default_bool;
                            schema.defaults.flag_style = #default_flag_style;
                            schema.defaults.conflict = #default_conflict;
                            schema.values.push(::kdl_config::schema::SchemaValue {
                                ty: #tag_expr,
                                required: true,
                                description: None,
                                enum_values: Some(vec![#tag_literal]),
                            });
                            ::kdl_config::schema::SchemaRef::Inline(schema)
                        }
                    }
                }
                VariantSchemaKind::Newtype(ty) => {
                    quote! {
                        {
                            let schema_ref = <#ty as ::kdl_config::schema::KdlSchema>::schema_ref();
                            let mut schema = ::kdl_config::schema::KdlNodeSchema::default();
                            #variant_name_assign
                            schema.deny_unknown = #deny_unknown;
                            schema.defaults.placement = #default_placement;
                            schema.defaults.bool_mode = #default_bool;
                            schema.defaults.flag_style = #default_flag_style;
                            schema.defaults.conflict = #default_conflict;
                            schema.values.push(::kdl_config::schema::SchemaValue {
                                ty: #tag_expr,
                                required: true,
                                description: None,
                                enum_values: Some(vec![#tag_literal]),
                            });
                            match schema_ref {
                                ::kdl_config::schema::SchemaRef::Ref(r) => {
                                    schema.ref_type = Some(r);
                                }
                                ::kdl_config::schema::SchemaRef::Inline(s) => {
                                    schema.props = s.props;
                                    schema.values.extend(s.values);
                                    schema.children = s.children;
                                    schema.registry_key = s.registry_key;
                                }
                                ::kdl_config::schema::SchemaRef::Choice(choices) => {
                                    schema.children = Some(::std::boxed::Box::new(
                                        ::kdl_config::schema::ChildrenSchema {
                                            nodes: vec![::kdl_config::schema::SchemaRef::Choice(choices)],
                                        },
                                    ));
                                }
                            }
                            ::kdl_config::schema::SchemaRef::Inline(schema)
                        }
                    }
                }
                VariantSchemaKind::Tuple(types) => {
                    let value_inserts: Vec<TokenStream> = types
                        .iter()
                        .map(|ty| {
                            let is_option = is_option_type(ty);
                            let value_ty = if is_option {
                                extract_inner_type(ty).unwrap_or(ty)
                            } else {
                                ty
                            };
                            let required = !is_option;
                            let type_expr = schema_type_expr(value_ty);
                            quote! {
                                schema.values.push(::kdl_config::schema::SchemaValue {
                                    ty: #type_expr,
                                    required: #required,
                                    description: None,
                                    enum_values: None,
                                });
                            }
                        })
                        .collect();
                    quote! {
                        {
                            let mut schema = ::kdl_config::schema::KdlNodeSchema::default();
                            #variant_name_assign
                            schema.deny_unknown = #deny_unknown;
                            schema.defaults.placement = #default_placement;
                            schema.defaults.bool_mode = #default_bool;
                            schema.defaults.flag_style = #default_flag_style;
                            schema.defaults.conflict = #default_conflict;
                            schema.values.push(::kdl_config::schema::SchemaValue {
                                ty: #tag_expr,
                                required: true,
                                description: None,
                                enum_values: Some(vec![#tag_literal]),
                            });
                            #(#value_inserts)*
                            ::kdl_config::schema::SchemaRef::Inline(schema)
                        }
                    }
                }
                VariantSchemaKind::Struct { fields } => {
                    let parts = collect_schema_parts_with_offset(fields, struct_attrs, 1);
                    let schema_body = render_schema_parts(&parts);
                    quote! {
                        {
                            let mut schema = ::kdl_config::schema::KdlNodeSchema::default();
                            #variant_name_assign
                            schema.deny_unknown = #deny_unknown;
                            schema.defaults.placement = #default_placement;
                            schema.defaults.bool_mode = #default_bool;
                            schema.defaults.flag_style = #default_flag_style;
                            schema.defaults.conflict = #default_conflict;
                            schema.values.push(::kdl_config::schema::SchemaValue {
                                ty: #tag_expr,
                                required: true,
                                description: None,
                                enum_values: Some(vec![#tag_literal]),
                            });
                            #schema_body
                            ::kdl_config::schema::SchemaRef::Inline(schema)
                        }
                    }
                }
            }
        })
        .collect();

    Ok(quote! {
        impl ::kdl_config::schema::KdlSchema for #enum_name {
            fn schema_ref() -> ::kdl_config::schema::SchemaRef {
                ::kdl_config::schema::SchemaRef::Ref(stringify!(#enum_name).to_string())
            }

            fn register_definitions(registry: &mut ::kdl_config::schema::SchemaRegistry) {
                let name = stringify!(#enum_name).to_string();
                if registry.definitions.contains_key(&name) {
                    return;
                }

                registry.definitions.insert(name.clone(), ::kdl_config::schema::KdlNodeSchema::default());

                let mut enum_schema = ::kdl_config::schema::KdlNodeSchema::default();
                #node_name_assign
                enum_schema.description = #schema_description;
                enum_schema.deny_unknown = #deny_unknown;
                enum_schema.defaults.placement = #default_placement;
                enum_schema.defaults.bool_mode = #default_bool;
                enum_schema.defaults.flag_style = #default_flag_style;
                enum_schema.defaults.conflict = #default_conflict;
                enum_schema.values.push(::kdl_config::schema::SchemaValue {
                    ty: #tag_value_expr,
                    required: true,
                    description: None,
                    enum_values: Some(vec![#(#enum_literals),*]),
                });
                enum_schema.variants = Some(vec![#(#variant_nodes),*]);

                #(#variant_registers)*

                registry.definitions.insert(name, enum_schema);
            }
        }
    })
}

fn generate_schema_union_impl(
    input: &DeriveInput,
    data: &syn::DataUnion,
    struct_attrs: &StructAttrs,
) -> syn::Result<TokenStream> {
    let union_name = &input.ident;

    let mut choices = Vec::new();
    let mut register_calls = Vec::new();

    for field in &data.fields.named {
        let ident = field.ident.as_ref().ok_or_else(|| {
            syn::Error::new(field.span(), "union fields must be named")
        })?;
        let attrs = parse_field_attrs(field)?;
        let attrs = match attrs {
            Some(attrs) => attrs,
            None => continue,
        };

        if attrs.skip || attrs.schema.skip {
            continue;
        }

        if attrs.schema.ty.is_some() && !is_value_type(&field.ty) && !attrs.scalar {
            return Err(syn::Error::new(
                field.span(),
                "schema(type = ...) is only valid for scalar value fields",
            ));
        }

        let base_name = attrs
            .name
            .unwrap_or_else(|| struct_attrs.rename_all.apply(&ident.to_string()));
        let schema_name = attrs.schema.name.clone().unwrap_or(base_name);
        let desc_expr = schema_desc_expr(attrs.schema.description.as_ref());
        let required_assign = if let Some(required) = attrs.schema.required {
            quote! { node_schema.required = Some(#required); }
        } else {
            quote! {}
        };

        let override_block = if let Some(override_ty) = attrs.schema.ty.as_ref() {
            let override_expr = schema_type_override_expr(override_ty);
            let required_value = attrs.schema.required.unwrap_or(true);
            quote! {
                let override_ty = #override_expr;
                if node_schema.values.is_empty() {
                    node_schema.values.push(::kdl_config::schema::SchemaValue {
                        ty: override_ty,
                        required: #required_value,
                        description: None,
                        enum_values: None,
                    });
                } else {
                    for value in &mut node_schema.values {
                        value.ty = override_ty.clone();
                    }
                }
            }
        } else if attrs.scalar {
            let required_value = attrs.schema.required.unwrap_or(true);
            quote! {
                if node_schema.values.is_empty() {
                    node_schema.values.push(::kdl_config::schema::SchemaValue {
                        ty: ::kdl_config::schema::SchemaType::String,
                        required: #required_value,
                        description: None,
                        enum_values: None,
                    });
                }
            }
        } else {
            quote! {}
        };

        let ty = &field.ty;
        if !attrs.scalar && attrs.schema.ty.is_none() {
            register_calls.push(quote! {
                <#ty as ::kdl_config::schema::KdlSchema>::register_definitions(registry);
            });
        }

        let schema_ref_block = if attrs.scalar {
            quote! {}
        } else {
            quote! {
                let schema_ref = <#ty as ::kdl_config::schema::KdlSchema>::schema_ref();
                match schema_ref {
                    ::kdl_config::schema::SchemaRef::Ref(r) => {
                        node_schema.ref_type = Some(r);
                    }
                    ::kdl_config::schema::SchemaRef::Inline(s) => {
                        node_schema.props = s.props;
                        node_schema.values = s.values;
                        node_schema.children = s.children;
                        node_schema.registry_key = s.registry_key;
                    }
                    ::kdl_config::schema::SchemaRef::Choice(choices) => {
                        node_schema.children = Some(::std::boxed::Box::new(
                            ::kdl_config::schema::ChildrenSchema {
                                nodes: vec![::kdl_config::schema::SchemaRef::Choice(choices)],
                            },
                        ));
                    }
                }
            }
        };

        choices.push(quote! {
            {
                let mut node_schema = ::kdl_config::schema::KdlNodeSchema::default();
                node_schema.name = Some(#schema_name.to_string());
                node_schema.description = #desc_expr;
                #required_assign
                #schema_ref_block
                #override_block
                ::kdl_config::schema::SchemaRef::Inline(node_schema)
            }
        });
    }

    Ok(quote! {
        impl ::kdl_config::schema::KdlSchema for #union_name {
            fn schema_ref() -> ::kdl_config::schema::SchemaRef {
                ::kdl_config::schema::SchemaRef::Choice(vec![
                    #(#choices),*
                ])
            }

            fn register_definitions(registry: &mut ::kdl_config::schema::SchemaRegistry) {
                #(#register_calls)*
            }
        }
    })
}
