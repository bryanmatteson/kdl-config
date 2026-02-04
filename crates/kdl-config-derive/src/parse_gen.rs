use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::attrs::{
    BoolMode, ChildrenMapKind, ConflictPolicy, DefaultLiteral, DefaultPlacement, DefaultSpec,
    FieldInfo, FlagStyle, StructAttrs, extract_children_map_types, extract_hashmap_types,
    extract_inner_type, has_child_placement, has_value_placement, is_bool_type, is_numeric_type,
    is_string_type, is_value_type,
};

#[derive(Debug, Clone, Copy)]
enum FieldKind {
    ValueScalar,
    ValueVec,
    Node,
    NodeVec,
    Flatten,
    Registry,
    ChildrenMap,
    Modifier,
}

pub fn generate_parse_impl(
    struct_name: &Ident,
    struct_attrs: &StructAttrs,
    fields: &[FieldInfo],
    skipped_fields: &[Ident],
    skipped_infos: &[FieldInfo],
) -> TokenStream {
    let struct_name_str = struct_name.to_string();
    let expected_node_name = struct_attrs.resolved_node_name(&struct_name_str);

    let positional_list_fields: Vec<&FieldInfo> = fields
        .iter()
        .filter(|field| field.placement.positional_list && !field.is_skipped)
        .collect();
    let positional_index_fields: Vec<&FieldInfo> = fields
        .iter()
        .filter(|field| field.placement.positional.is_some() && !field.is_skipped)
        .collect();

    if positional_list_fields.len() > 1 {
        return quote! { compile_error!("only one positional list field is allowed per struct"); };
    }
    if !positional_list_fields.is_empty() && !positional_index_fields.is_empty() {
        return quote! { compile_error!("positional list fields cannot be combined with indexed positional fields"); };
    }

    let validate_name = if let Some(ref node_name) = expected_node_name {
        quote! {
            if node.name != #node_name {
                return Err(::kdl_config::KdlConfigError::node_name_mismatch(
                    #struct_name_str,
                    #node_name,
                    &node.name,
                ));
            }
        }
    } else {
        quote! {}
    };

    let struct_overrides = generate_struct_overrides(struct_attrs);

    let field_parsers: Vec<TokenStream> = fields
        .iter()
        .map(|field| generate_field_parser(field, &struct_name_str))
        .collect();

    let skip_marks: Vec<TokenStream> = skipped_infos
        .iter()
        .map(|field| generate_skip_marks(field))
        .collect();

    let field_names: Vec<&Ident> = fields.iter().map(|f| &f.ident).collect();

    quote! {
        impl ::kdl_config::KdlParse for #struct_name {
            fn from_node(node: &::kdl_config::Node, config: &::kdl_config::ParseConfig) -> ::core::result::Result<Self, ::kdl_config::KdlConfigError> {
                #validate_name
                let struct_overrides = #struct_overrides;
                let struct_config = ::kdl_config::resolve_struct(config, struct_overrides);
                let mut used_keys = ::kdl_config::helpers::UsedKeys::new();

                #(#field_parsers)*
                #(#skip_marks)*

                if struct_config.deny_unknown {
                    used_keys.check_unknowns(node, #struct_name_str)?;
                }

                Ok(Self {
                    #(#field_names,)*
                    #(#skipped_fields: ::std::default::Default::default(),)*
                })
            }
        }
    }
}

pub(crate) fn generate_struct_overrides(struct_attrs: &StructAttrs) -> TokenStream {
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
        None => quote! { None },
    };

    let default_bool = match struct_attrs.default_bool {
        Some(BoolMode::PresenceAndValue) => {
            quote! { Some(::kdl_config::BoolMode::PresenceAndValue) }
        }
        Some(BoolMode::ValueOnly) => quote! { Some(::kdl_config::BoolMode::ValueOnly) },
        Some(BoolMode::PresenceOnly) => {
            quote! { Some(::kdl_config::BoolMode::PresenceOnly) }
        }
        None => quote! { None },
    };

    let default_flag_style = match struct_attrs.default_flag_style {
        Some(FlagStyle::Both) => quote! { Some(::kdl_config::FlagStyle::Both) },
        Some(FlagStyle::ValueNo) => quote! { Some(::kdl_config::FlagStyle::ValueNo) },
        Some(FlagStyle::WithWithout) => {
            quote! { Some(::kdl_config::FlagStyle::WithWithout) }
        }
        None => quote! { None },
    };

    let default_conflict = match struct_attrs.default_conflict {
        Some(ConflictPolicy::Error) => quote! { Some(::kdl_config::ConflictPolicy::Error) },
        Some(ConflictPolicy::First) => quote! { Some(::kdl_config::ConflictPolicy::First) },
        Some(ConflictPolicy::Last) => quote! { Some(::kdl_config::ConflictPolicy::Last) },
        Some(ConflictPolicy::Append) => {
            quote! { Some(::kdl_config::ConflictPolicy::Append) }
        }
        None => quote! { None },
    };

    let deny_unknown = match struct_attrs.deny_unknown {
        Some(true) => quote! { Some(true) },
        Some(false) => quote! { Some(false) },
        None => quote! { None },
    };

    quote! {
        ::kdl_config::StructOverrides {
            default_placement: #default_placement,
            default_bool: #default_bool,
            default_flag_style: #default_flag_style,
            default_conflict: #default_conflict,
            deny_unknown: #deny_unknown,
        }
    }
}

pub(crate) fn generate_field_parser(field: &FieldInfo, struct_name: &str) -> TokenStream {
    if field.is_hashmap && !field.placement.registry && !field.children_map {
        return quote! { compile_error!("HashMap fields require #[kdl(registry)] or #[kdl(children_map)]"); };
    }
    if crate::attrs::extract_registry_vec_value(&field.ty).is_some()
        && !field.placement.registry
        && !field.children_map
    {
        return quote! { compile_error!("Vec<(String, T)> registry fields require #[kdl(registry)]"); };
    }

    let field_overrides = generate_field_overrides(field);
    let mark_usage = generate_usage_marks(field);

    match field_kind(field) {
        FieldKind::Registry => {
            generate_registry_parser(field, struct_name, &field_overrides, mark_usage)
        }
        FieldKind::ChildrenMap => {
            generate_children_map_parser(field, struct_name, &field_overrides, mark_usage)
        }
        FieldKind::Flatten => {
            generate_flatten_parser(field, struct_name, &field_overrides, mark_usage)
        }
        FieldKind::NodeVec => {
            generate_node_vec_parser(field, struct_name, &field_overrides, mark_usage)
        }
        FieldKind::Node => generate_node_parser(field, struct_name, &field_overrides, mark_usage),
        FieldKind::ValueVec => {
            generate_value_vec_parser(field, struct_name, &field_overrides, mark_usage)
        }
        FieldKind::ValueScalar => {
            generate_value_scalar_parser(field, struct_name, &field_overrides, mark_usage)
        }
        FieldKind::Modifier => generate_modifier_parser(field),
    }
}

fn generate_field_overrides(field: &FieldInfo) -> TokenStream {
    let bool_mode = match field.bool_mode {
        Some(BoolMode::PresenceAndValue) => {
            quote! { Some(::kdl_config::BoolMode::PresenceAndValue) }
        }
        Some(BoolMode::ValueOnly) => quote! { Some(::kdl_config::BoolMode::ValueOnly) },
        Some(BoolMode::PresenceOnly) => {
            quote! { Some(::kdl_config::BoolMode::PresenceOnly) }
        }
        None => quote! { None },
    };

    let flag_style = match field.flag_style {
        Some(FlagStyle::Both) => quote! { Some(::kdl_config::FlagStyle::Both) },
        Some(FlagStyle::ValueNo) => quote! { Some(::kdl_config::FlagStyle::ValueNo) },
        Some(FlagStyle::WithWithout) => {
            quote! { Some(::kdl_config::FlagStyle::WithWithout) }
        }
        None => quote! { None },
    };

    let conflict = match field.conflict {
        Some(ConflictPolicy::Error) => quote! { Some(::kdl_config::ConflictPolicy::Error) },
        Some(ConflictPolicy::First) => quote! { Some(::kdl_config::ConflictPolicy::First) },
        Some(ConflictPolicy::Last) => quote! { Some(::kdl_config::ConflictPolicy::Last) },
        Some(ConflictPolicy::Append) => {
            quote! { Some(::kdl_config::ConflictPolicy::Append) }
        }
        None => quote! { None },
    };

    quote! {
        ::kdl_config::FieldOverrides {
            bool_mode: #bool_mode,
            flag_style: #flag_style,
            conflict: #conflict,
        }
    }
}

fn field_kind(field: &FieldInfo) -> FieldKind {
    if field.is_modifier {
        return FieldKind::Modifier;
    }
    if field.flatten {
        return FieldKind::Flatten;
    }
    if field.placement.registry {
        return FieldKind::Registry;
    }
    if field.children_map {
        return FieldKind::ChildrenMap;
    }

    let has_value = has_value_placement(&field.placement);
    let has_child = has_child_placement(&field.placement);

    if field.is_vec || field.is_option_vec {
        if has_value {
            return FieldKind::ValueVec;
        }
        if has_child {
            return FieldKind::NodeVec;
        }
        let inner = if field.is_option_vec {
            field.inner_type().and_then(extract_inner_type)
        } else {
            field.inner_type()
        };
        let is_value = inner.map(is_value_type).unwrap_or(false) || field.is_scalar;
        if is_value {
            FieldKind::ValueVec
        } else {
            FieldKind::NodeVec
        }
    } else if has_value {
        FieldKind::ValueScalar
    } else if has_child {
        FieldKind::Node
    } else if is_value_type(&field.ty) || field.is_scalar {
        FieldKind::ValueScalar
    } else {
        FieldKind::Node
    }
}

fn generate_usage_marks(field: &FieldInfo) -> TokenStream {
    if field.is_modifier {
        return quote! {};
    }
    if field.children_map {
        return quote! {};
    }
    let mut marks = Vec::new();
    let key = field.kdl_key.clone();
    let container = field.container.clone().unwrap_or_else(|| key.clone());
    let has_explicit = field.placement.attr
        || field.placement.keyed
        || field.placement.positional.is_some()
        || field.placement.positional_list
        || field.placement.flag.is_some()
        || field.placement.value
        || field.placement.child
        || field.placement.children
        || field.placement.children_any
        || field.placement.registry;

    if field.placement.registry {
        marks.push(quote! { used_keys.mark_child(#container); });
    } else if has_explicit {
        if field.placement.children_any {
            marks.push(quote! {
                for child in node.children() {
                    used_keys.mark_child(&child.name);
                }
            });
        }
        if field.placement.attr || field.placement.keyed {
            marks.push(quote! { used_keys.mark_attr(#key); });
        }
        if field.placement.value || field.placement.child || field.placement.children {
            marks.push(quote! { used_keys.mark_child(#key); });
        }
    } else {
        match field_kind(field) {
            FieldKind::ValueScalar | FieldKind::ValueVec => {
                marks.push(quote! {
                    match struct_config.default_placement {
                        ::kdl_config::DefaultPlacement::Exhaustive => {
                            used_keys.mark_attr(#key);
                            used_keys.mark_child(#key);
                        }
                        ::kdl_config::DefaultPlacement::Attr => {
                            used_keys.mark_attr(#key);
                        }
                        ::kdl_config::DefaultPlacement::Value | ::kdl_config::DefaultPlacement::Child => {
                            used_keys.mark_child(#key);
                        }
                    }
                });
            }
            FieldKind::Node | FieldKind::NodeVec => {
                marks.push(quote! {
                    match struct_config.default_placement {
                        ::kdl_config::DefaultPlacement::Exhaustive | ::kdl_config::DefaultPlacement::Child => {
                            used_keys.mark_child(#key);
                        }
                        ::kdl_config::DefaultPlacement::Attr | ::kdl_config::DefaultPlacement::Value => {}
                    }
                });
            }
            FieldKind::Flatten => {
                marks.push(quote! {
                    for key in node.attrs().keys() {
                        used_keys.mark_attr(key);
                    }
                    for (idx, _) in node.args().iter().enumerate() {
                        used_keys.mark_arg(idx);
                    }
                    for child in node.children() {
                        used_keys.mark_child(&child.name);
                    }
                });
            }
            FieldKind::Registry => {
                marks.push(quote! { used_keys.mark_child(#container); });
            }
            FieldKind::ChildrenMap => {}
            FieldKind::Modifier => {}
        }
    }

    if marks.is_empty() {
        quote! {}
    } else {
        quote! {
            if struct_config.deny_unknown {
                #(#marks)*
            }
        }
    }
}

pub(crate) fn generate_skip_marks(field: &FieldInfo) -> TokenStream {
    if field.is_modifier {
        return quote! {};
    }
    if field.children_map {
        let map_node = field.map_node.clone();
        let marks = if let Some(map_node) = map_node {
            quote! { used_keys.mark_child(#map_node); }
        } else {
            quote! {
                for child in node.children() {
                    used_keys.mark_child(&child.name);
                }
            }
        };
        return quote! {
            if struct_config.deny_unknown {
                #marks
            }
        };
    }

    let mut marks = Vec::new();
    let key = field.kdl_key.clone();
    let container = field.container.clone().unwrap_or_else(|| key.clone());

    let has_explicit = field.placement.attr
        || field.placement.keyed
        || field.placement.positional.is_some()
        || field.placement.positional_list
        || field.placement.flag.is_some()
        || field.placement.value
        || field.placement.child
        || field.placement.children
        || field.placement.children_any
        || field.placement.registry;

    if field.placement.registry {
        marks.push(quote! { used_keys.mark_child(#container); });
    } else if has_explicit {
        if field.placement.children_any {
            marks.push(quote! {
                for child in node.children() {
                    used_keys.mark_child(&child.name);
                }
            });
        }
        if field.placement.attr || field.placement.keyed {
            marks.push(quote! { used_keys.mark_attr(#key); });
        }
        if let Some(idx) = field.placement.positional {
            marks.push(quote! { used_keys.mark_arg(#idx); });
        }
        if field.placement.positional_list {
            marks.push(quote! {
                for idx in 0..node.args().len() {
                    used_keys.mark_arg(idx);
                }
            });
        }
        if field.placement.value || field.placement.child || field.placement.children {
            marks.push(quote! { used_keys.mark_child(#key); });
        }
    } else {
        match field_kind(field) {
            FieldKind::ValueScalar | FieldKind::ValueVec => {
                marks.push(quote! {
                    match struct_config.default_placement {
                        ::kdl_config::DefaultPlacement::Exhaustive => {
                            used_keys.mark_attr(#key);
                            used_keys.mark_child(#key);
                        }
                        ::kdl_config::DefaultPlacement::Attr => {
                            used_keys.mark_attr(#key);
                        }
                        ::kdl_config::DefaultPlacement::Value | ::kdl_config::DefaultPlacement::Child => {
                            used_keys.mark_child(#key);
                        }
                    }
                });
            }
            FieldKind::Node | FieldKind::NodeVec => {
                marks.push(quote! {
                    match struct_config.default_placement {
                        ::kdl_config::DefaultPlacement::Exhaustive | ::kdl_config::DefaultPlacement::Child => {
                            used_keys.mark_child(#key);
                        }
                        ::kdl_config::DefaultPlacement::Attr | ::kdl_config::DefaultPlacement::Value => {}
                    }
                });
            }
            FieldKind::Flatten => {
                marks.push(quote! {
                    for key in node.attrs().keys() {
                        used_keys.mark_attr(key);
                    }
                    for (idx, _) in node.args().iter().enumerate() {
                        used_keys.mark_arg(idx);
                    }
                    for child in node.children() {
                        used_keys.mark_child(&child.name);
                    }
                });
            }
            FieldKind::Registry => {
                marks.push(quote! { used_keys.mark_child(#container); });
            }
            FieldKind::ChildrenMap => {}
            FieldKind::Modifier => {}
        }
    }

    let field_overrides = generate_field_overrides(field);
    let field_config_ident = format_ident!("__kdl_skip_cfg_{}", field.ident);
    let is_value_scalar = matches!(field_kind(field), FieldKind::ValueScalar);
    let mut flag_marks = Vec::new();

    if field.is_bool {
        let allow_flags_explicit =
            field.placement.flag.is_some() || (field.placement.attr && !field.placement.keyed);

        let (custom_pos, custom_neg) = match &field.placement.flag {
            Some((Some(pos), Some(neg))) => (Some(pos.clone()), Some(neg.clone())),
            _ => (None, None),
        };

        let custom_pos_expr = match custom_pos {
            Some(ref s) => quote! { Some(#s.to_string()) },
            None => quote! { None },
        };
        let custom_neg_expr = match custom_neg {
            Some(ref s) => quote! { Some(#s.to_string()) },
            None => quote! { None },
        };

        let allow_flags_default = quote! {
            matches!(struct_config.default_placement, ::kdl_config::DefaultPlacement::Exhaustive | ::kdl_config::DefaultPlacement::Attr)
        };

        flag_marks.push(quote! {
            if #field_config_ident.bool_mode != ::kdl_config::BoolMode::ValueOnly {
                let allow_flags = if #has_explicit {
                    #allow_flags_explicit
                } else {
                    #allow_flags_default && #is_value_scalar
                };

                if allow_flags {
                    let custom_pos = #custom_pos_expr;
                    let custom_neg = #custom_neg_expr;
                    let (pos, neg) = if let (Some(pos), Some(neg)) = (custom_pos, custom_neg) {
                        (pos, neg)
                    } else {
                        ::kdl_config::helpers::flag_names_for_key(#key, #field_config_ident.flag_style)
                    };
                    used_keys.mark_flag(&pos);
                    used_keys.mark_flag(&neg);
                }
            }
        });
    }

    if marks.is_empty() && flag_marks.is_empty() {
        quote! {}
    } else {
        quote! {
            if struct_config.deny_unknown {
                let #field_config_ident = ::kdl_config::resolve_field(&struct_config, #field_overrides);
                #(#marks)*
                #(#flag_marks)*
            }
        }
    }
}

fn generate_modifier_parser(field: &FieldInfo) -> TokenStream {
    let field_ident = &field.ident;
    let ty = &field.ty;

    if field.is_optional {
        quote! {
            let #field_ident: #ty = if node.modifier == ::kdl_config::Modifier::Inherit {
                None
            } else {
                Some(node.modifier)
            };
        }
    } else {
        quote! {
            let #field_ident: #ty = node.modifier;
        }
    }
}

fn generate_value_scalar_parser(
    field: &FieldInfo,
    struct_name: &str,
    field_overrides: &TokenStream,
    mark_usage: TokenStream,
) -> TokenStream {
    let field_ident = &field.ident;
    let field_name = field.ident.to_string();
    let kdl_key = &field.kdl_key;
    let ty = &field.ty;
    let value_ty = if field.is_optional {
        field.inner_type().unwrap()
    } else {
        ty
    };
    let attr_values_ident = format_ident!("__kdl_attr_values_{}", field_ident);
    let attr_value_ident = format_ident!("__kdl_attr_value_{}", field_ident);
    let arg_value_ident = format_ident!("__kdl_arg_value_{}", field_ident);
    let child_ident = format_ident!("__kdl_child_{}", field_ident);

    let child_arg_ident = format_ident!("__kdl_child_arg_{}", field_ident);

    let is_optional = field.is_optional;
    let is_bool = field.is_bool;

    let allow_attr_keyed =
        field.placement.keyed || (field.placement.attr && field.placement.flag.is_none());
    let allow_attr_positional = field.placement.positional;
    let allow_attr_positional_list = field.placement.positional_list;
    let pos_index_expr = match allow_attr_positional {
        Some(idx) => quote! { Some(#idx) },
        None => quote! { None },
    };
    let allow_value = field.placement.value;
    let allow_flags =
        field.placement.flag.is_some() || (field.placement.attr && !field.placement.keyed);

    let (custom_pos, custom_neg) = match &field.placement.flag {
        Some((pos, neg)) => (pos.clone(), neg.clone()),
        None => (None, None),
    };

    let custom_pos_expr = match custom_pos {
        Some(ref s) => quote! { Some(#s.to_string()) },
        None => quote! { None },
    };
    let custom_neg_expr = match custom_neg {
        Some(ref s) => quote! { Some(#s.to_string()) },
        None => quote! { None },
    };

    let default_expr = generate_missing_expr(
        field,
        value_ty,
        struct_name,
        &field_name,
        kdl_key,
        false,
        quote! { ::kdl_config::Placement::Unknown },
    );
    let finalize_value = if is_optional {
        quote! { Some(val) }
    } else {
        quote! { val }
    };

    quote! {
        #mark_usage
        let field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
        let mut #field_ident: ::core::option::Option<#value_ty> = None;

        let custom_pos = #custom_pos_expr;
        let custom_neg = #custom_neg_expr;
        let pos_index = #pos_index_expr;
        let has_placement = #allow_attr_keyed || #allow_value || pos_index.is_some() || #allow_attr_positional_list || #allow_flags;

        if !has_placement {
            if #is_bool && field_config.bool_mode == ::kdl_config::BoolMode::ValueOnly {
                let flag_val = ::kdl_config::helpers::find_flag_with_style(
                    node,
                    #kdl_key,
                    field_config.flag_style,
                    custom_pos.clone(),
                    custom_neg.clone(),
                    #struct_name,
                    #field_name,
                )?;
                if flag_val.is_some() {
                    return Err(::kdl_config::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "flag placement is not allowed for value-only bools",
                    ));
                }
            }
            match field_config.default_placement {
                ::kdl_config::DefaultPlacement::Exhaustive => {
                    if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                        for #attr_value_ident in #attr_values_ident {
                            if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "explicit values are not allowed in presence-only mode",
                                ));
                            }
                            let v = ::kdl_config::convert_value_checked::<#value_ty>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                            ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                        }
                    }

                    if let Some(idx) = pos_index {
                        if let Some(#arg_value_ident) = node.arg(idx) {
                            if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "explicit values are not allowed in presence-only mode",
                                ));
                            }
                            let v = ::kdl_config::convert_value_checked::<#value_ty>(#arg_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                            ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                            if struct_config.deny_unknown {
                                used_keys.mark_arg(idx);
                            }
                        }
                    }

                    if #is_bool && field_config.bool_mode != ::kdl_config::BoolMode::ValueOnly {
                        let flag_val = if struct_config.deny_unknown {
                            ::kdl_config::helpers::find_flag_with_style_marked(
                                node,
                                #kdl_key,
                                field_config.flag_style,
                                custom_pos.clone(),
                                custom_neg.clone(),
                                #struct_name,
                                #field_name,
                                Some(&mut used_keys),
                            )?
                        } else {
                            ::kdl_config::helpers::find_flag_with_style(
                                node,
                                #kdl_key,
                                field_config.flag_style,
                                custom_pos.clone(),
                                custom_neg.clone(),
                                #struct_name,
                                #field_name,
                            )?
                        };
                        if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                            if let Some(false) = flag_val {
                                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "negative flags are not allowed in presence-only mode",
                                ));
                            }
                        }
                        if let Some(flag_val) = flag_val {
                            let v: #value_ty = ::kdl_config::convert_value_checked(&::kdl_config::Value::Bool(flag_val), #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrFlag)?;
                            ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrFlag)?;
                        }
                    }

                    for #child_ident in node.children_named(#kdl_key) {
                        if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                            if #child_ident.args().is_empty() && #child_ident.children().is_empty() && #child_ident.attrs().is_empty() {
                                let v: #value_ty = ::kdl_config::convert_value_checked(&::kdl_config::Value::Bool(true), #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                                ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                            } else {
                                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "explicit values are not allowed in presence-only mode",
                                ));
                            }
                        } else {
                            if #child_ident.args().is_empty() {
                                if #is_bool && field_config.bool_mode != ::kdl_config::BoolMode::ValueOnly && #child_ident.children().is_empty() && #child_ident.attrs().is_empty() {
                                    let v: #value_ty = ::kdl_config::convert_value_checked(&::kdl_config::Value::Bool(true), #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                                    ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                                } else {
                                    return Err(::kdl_config::KdlConfigError::missing_required(
                                        #struct_name,
                                        #field_name,
                                        #kdl_key,
                                        ::kdl_config::Placement::Value,
                                    ));
                                }
                            } else if #child_ident.args().len() == 1 {
                                let #child_arg_ident = &#child_ident.args()[0];
                                let v = ::kdl_config::convert_value_checked::<#value_ty>(#child_arg_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                                ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                            } else {
                                return Err(::kdl_config::KdlConfigError::too_many(
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config::Placement::Value,
                                    #child_ident.args().len(),
                                ));
                            }
                        }
                    }
                }
                ::kdl_config::DefaultPlacement::Attr => {
                    if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                        for #attr_value_ident in #attr_values_ident {
                            if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "explicit values are not allowed in presence-only mode",
                                ));
                            }
                            let v = ::kdl_config::convert_value_checked::<#value_ty>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                            ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                        }
                    }
                    if #is_bool && field_config.bool_mode != ::kdl_config::BoolMode::ValueOnly {
                        let flag_val = if struct_config.deny_unknown {
                            ::kdl_config::helpers::find_flag_with_style_marked(
                                node,
                                #kdl_key,
                                field_config.flag_style,
                                custom_pos.clone(),
                                custom_neg.clone(),
                                #struct_name,
                                #field_name,
                                Some(&mut used_keys),
                            )?
                        } else {
                            ::kdl_config::helpers::find_flag_with_style(
                                node,
                                #kdl_key,
                                field_config.flag_style,
                                custom_pos.clone(),
                                custom_neg.clone(),
                                #struct_name,
                                #field_name,
                            )?
                        };
                        if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                            if let Some(false) = flag_val {
                                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "negative flags are not allowed in presence-only mode",
                                ));
                            }
                        }
                        if let Some(flag_val) = flag_val {
                            let v: #value_ty = ::kdl_config::convert_value_checked(&::kdl_config::Value::Bool(flag_val), #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrFlag)?;
                            ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrFlag)?;
                        }
                    }
                }
                ::kdl_config::DefaultPlacement::Value => {
                    for #child_ident in node.children_named(#kdl_key) {
                        if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                            if #child_ident.args().is_empty() && #child_ident.children().is_empty() && #child_ident.attrs().is_empty() {
                                let v: #value_ty = ::kdl_config::convert_value_checked(
                                    &::kdl_config::Value::Bool(true),
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config::Placement::Value,
                                )?;
                                ::kdl_config::helpers::resolve_scalar(
                                    field_config.conflict,
                                    &mut #field_ident,
                                    v,
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config::Placement::Value,
                                )?;
                            } else {
                                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "explicit values are not allowed in presence-only mode",
                                ));
                            }
                        } else {
                            if #child_ident.args().is_empty() {
                                if #is_bool && field_config.bool_mode != ::kdl_config::BoolMode::ValueOnly && #child_ident.children().is_empty() && #child_ident.attrs().is_empty() {
                                    let v: #value_ty = ::kdl_config::convert_value_checked(
                                        &::kdl_config::Value::Bool(true),
                                        #struct_name,
                                        #field_name,
                                        #kdl_key,
                                        ::kdl_config::Placement::Value,
                                    )?;
                                    ::kdl_config::helpers::resolve_scalar(
                                        field_config.conflict,
                                        &mut #field_ident,
                                        v,
                                        #struct_name,
                                        #field_name,
                                        #kdl_key,
                                        ::kdl_config::Placement::Value,
                                    )?;
                                } else {
                                    return Err(::kdl_config::KdlConfigError::missing_required(
                                        #struct_name,
                                        #field_name,
                                        #kdl_key,
                                        ::kdl_config::Placement::Value,
                                    ));
                                }
                            } else if #child_ident.args().len() == 1 {
                                let v = ::kdl_config::convert_value_checked::<#value_ty>(
                                    &#child_ident.args()[0],
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config::Placement::Value,
                                )?;
                                ::kdl_config::helpers::resolve_scalar(
                                    field_config.conflict,
                                    &mut #field_ident,
                                    v,
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config::Placement::Value,
                                )?;
                            } else {
                                return Err(::kdl_config::KdlConfigError::too_many(
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config::Placement::Value,
                                    #child_ident.args().len(),
                                ));
                            }
                        }
                    }
                }
                ::kdl_config::DefaultPlacement::Child => {
                    return Err(::kdl_config::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "child placement is incompatible with scalar value types",
                    ));
                }
            }
        } else {
            if !#allow_attr_keyed {
                if node.attr_values(#kdl_key).is_some() {
                    return Err(::kdl_config::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "keyed attribute placement is not allowed for this field",
                    ));
                }
            }

            if !#allow_value {
                if node.children_named(#kdl_key).next().is_some() {
                    return Err(::kdl_config::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "value placement is not allowed for this field",
                    ));
                }
            }

            if #is_bool && (!#allow_flags || field_config.bool_mode == ::kdl_config::BoolMode::ValueOnly) {
                let flag_val = ::kdl_config::helpers::find_flag_with_style(
                    node,
                    #kdl_key,
                    field_config.flag_style,
                    custom_pos.clone(),
                    custom_neg.clone(),
                    #struct_name,
                    #field_name,
                )?;
                if flag_val.is_some() {
                    return Err(::kdl_config::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "flag placement is not allowed for this field",
                    ));
                }
            }

            if #allow_attr_keyed {
                if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                    for #attr_value_ident in #attr_values_ident {
                        if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                            return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                #struct_name,
                                #field_name,
                                "explicit values are not allowed in presence-only mode",
                            ));
                        }
                        let v = ::kdl_config::convert_value_checked::<#value_ty>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                        ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                    }
                }
            }

            if let Some(idx) = pos_index {
                if let Some(#arg_value_ident) = node.arg(idx) {
                    if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                        return Err(::kdl_config::KdlConfigError::incompatible_placement(
                            #struct_name,
                            #field_name,
                            "explicit values are not allowed in presence-only mode",
                        ));
                    }
                    let v = ::kdl_config::convert_value_checked::<#value_ty>(#arg_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                    ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                    if struct_config.deny_unknown {
                        used_keys.mark_arg(idx);
                    }
                }
            }

            if #is_bool && #allow_flags && field_config.bool_mode != ::kdl_config::BoolMode::ValueOnly {
                let flag_val = if struct_config.deny_unknown {
                    ::kdl_config::helpers::find_flag_with_style_marked(
                        node,
                        #kdl_key,
                        field_config.flag_style,
                        custom_pos.clone(),
                        custom_neg.clone(),
                        #struct_name,
                        #field_name,
                        Some(&mut used_keys),
                    )?
                } else {
                    ::kdl_config::helpers::find_flag_with_style(
                        node,
                        #kdl_key,
                        field_config.flag_style,
                        custom_pos.clone(),
                        custom_neg.clone(),
                        #struct_name,
                        #field_name,
                    )?
                };
                if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                    if let Some(false) = flag_val {
                        return Err(::kdl_config::KdlConfigError::incompatible_placement(
                            #struct_name,
                            #field_name,
                            "negative flags are not allowed in presence-only mode",
                        ));
                    }
                }
                if let Some(flag_val) = flag_val {
                    let v: #value_ty = ::kdl_config::convert_value_checked(&::kdl_config::Value::Bool(flag_val), #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrFlag)?;
                    ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrFlag)?;
                }
            }

            if #allow_value {
                for #child_ident in node.children_named(#kdl_key) {
                    if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                        if #child_ident.args().is_empty() && #child_ident.children().is_empty() && #child_ident.attrs().is_empty() {
                            let v: #value_ty = ::kdl_config::convert_value_checked(&::kdl_config::Value::Bool(true), #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                            ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                        } else {
                            return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                #struct_name,
                                #field_name,
                                "explicit values are not allowed in presence-only mode",
                            ));
                        }
                    } else {
                        if #child_ident.args().is_empty() {
                            if #is_bool && field_config.bool_mode != ::kdl_config::BoolMode::ValueOnly && #child_ident.children().is_empty() && #child_ident.attrs().is_empty() {
                                let v: #value_ty = ::kdl_config::convert_value_checked(&::kdl_config::Value::Bool(true), #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                                ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                            } else {
                                return Err(::kdl_config::KdlConfigError::missing_required(
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config::Placement::Value,
                                ));
                            }
                        } else if #child_ident.args().len() == 1 {
                            let #child_arg_ident = &#child_ident.args()[0];
                            let v = ::kdl_config::convert_value_checked::<#value_ty>(#child_arg_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                            ::kdl_config::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                        } else {
                            return Err(::kdl_config::KdlConfigError::too_many(
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config::Placement::Value,
                                #child_ident.args().len(),
                            ));
                        }
                    }
                }
            }
        }

        let #field_ident: #ty = if let Some(val) = #field_ident {
            #finalize_value
        } else {
            #default_expr
        };
    }
}

fn generate_value_vec_parser(
    field: &FieldInfo,
    struct_name: &str,
    field_overrides: &TokenStream,
    mark_usage: TokenStream,
) -> TokenStream {
    let field_ident = &field.ident;
    let field_name = field.ident.to_string();
    let kdl_key = &field.kdl_key;
    let ty = &field.ty;
    let is_option_vec = field.is_option_vec;
    let inner_vec_ty = if is_option_vec {
        field.inner_type().and_then(extract_inner_type).unwrap()
    } else {
        field.inner_type().unwrap()
    };
    let elem_ty = extract_inner_type(inner_vec_ty).unwrap_or(inner_vec_ty);
    let attr_values_ident = format_ident!("__kdl_attr_values_{}", field_ident);
    let attr_value_ident = format_ident!("__kdl_attr_value_{}", field_ident);
    let arg_value_ident = format_ident!("__kdl_arg_value_{}", field_ident);
    let child_ident = format_ident!("__kdl_child_{}", field_ident);
    let child_arg_ident = format_ident!("__kdl_child_arg_{}", field_ident);
    let values_ident = format_ident!("__kdl_values_{}", field_ident);

    let allow_attr_keyed =
        field.placement.keyed || (field.placement.attr && field.placement.flag.is_none());
    let allow_attr_positional = field.placement.positional;
    let allow_attr_positional_list = field.placement.positional_list;
    let allow_value = field.placement.value;
    let pos_index_expr = match allow_attr_positional {
        Some(idx) => quote! { Some(#idx) },
        None => quote! { None },
    };

    let default_expr = generate_missing_expr(
        field,
        inner_vec_ty,
        struct_name,
        &field_name,
        kdl_key,
        true,
        quote! { ::kdl_config::Placement::Value },
    );
    let finalize_value = if is_option_vec {
        quote! { Some(val) }
    } else {
        quote! { val }
    };

    quote! {
        #mark_usage
        let field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
        let mut #field_ident: ::core::option::Option<Vec<#elem_ty>> = None;

        let pos_index = #pos_index_expr;
        let has_placement = #allow_attr_keyed || #allow_value || pos_index.is_some() || #allow_attr_positional_list;

        if !has_placement {
            match field_config.default_placement {
                ::kdl_config::DefaultPlacement::Exhaustive => {
                    if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                        for #attr_value_ident in #attr_values_ident {
                            let v = ::kdl_config::convert_value_checked::<Vec<#elem_ty>>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                            ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                        }
                    }
                    if let Some(idx) = pos_index {
                        if let Some(#arg_value_ident) = node.arg(idx) {
                            let v = ::kdl_config::convert_value_checked::<Vec<#elem_ty>>(#arg_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                            ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                            if struct_config.deny_unknown {
                                used_keys.mark_arg(idx);
                            }
                        }
                    }
                    for #child_ident in node.children_named(#kdl_key) {
                        if #child_ident.args().is_empty() {
                            return Err(::kdl_config::KdlConfigError::missing_required(
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config::Placement::Value,
                            ));
                        }
                        let mut #values_ident = Vec::with_capacity(#child_ident.args().len());
                        for #child_arg_ident in #child_ident.args() {
                            #values_ident.push(::kdl_config::convert_value_checked::<#elem_ty>(#child_arg_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?);
                        }
                        ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, #values_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                    }
                }
                ::kdl_config::DefaultPlacement::Attr => {
                    if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                        for #attr_value_ident in #attr_values_ident {
                            let v = ::kdl_config::convert_value_checked::<Vec<#elem_ty>>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                            ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                        }
                    }
                }
                ::kdl_config::DefaultPlacement::Value => {
                    for #child_ident in node.children_named(#kdl_key) {
                        if #child_ident.args().is_empty() {
                            return Err(::kdl_config::KdlConfigError::missing_required(
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config::Placement::Value,
                            ));
                        }
                        let mut #values_ident = Vec::with_capacity(#child_ident.args().len());
                        for #child_arg_ident in #child_ident.args() {
                            #values_ident.push(::kdl_config::convert_value_checked::<#elem_ty>(#child_arg_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?);
                        }
                        ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, #values_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                    }
                }
                ::kdl_config::DefaultPlacement::Child => {
                    return Err(::kdl_config::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "child placement is incompatible with scalar Vec values",
                    ));
                }
            }
        } else {
            if !#allow_attr_keyed {
                if node.attr_values(#kdl_key).is_some() {
                    return Err(::kdl_config::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "keyed attribute placement is not allowed for this field",
                    ));
                }
            }

            if !#allow_value {
                if node.children_named(#kdl_key).next().is_some() {
                    return Err(::kdl_config::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "value placement is not allowed for this field",
                    ));
                }
            }

            if #allow_attr_keyed {
                if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                    for #attr_value_ident in #attr_values_ident {
                        let v = ::kdl_config::convert_value_checked::<Vec<#elem_ty>>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                        ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                    }
                }
            }
            if let Some(idx) = pos_index {
                if let Some(#arg_value_ident) = node.arg(idx) {
                    let v = ::kdl_config::convert_value_checked::<Vec<#elem_ty>>(#arg_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                    ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                    if struct_config.deny_unknown {
                        used_keys.mark_arg(idx);
                    }
                }
            }
            if #allow_attr_positional_list {
                if !node.args().is_empty() {
                    let mut #values_ident = Vec::with_capacity(node.args().len());
                    for #arg_value_ident in node.args() {
                        #values_ident.push(::kdl_config::convert_value_checked::<#elem_ty>(#arg_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?);
                    }
                    ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, #values_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                    if struct_config.deny_unknown {
                        for idx in 0..node.args().len() {
                            used_keys.mark_arg(idx);
                        }
                    }
                }
            }
            if #allow_value {
                for #child_ident in node.children_named(#kdl_key) {
                    if #child_ident.args().is_empty() {
                        return Err(::kdl_config::KdlConfigError::missing_required(
                            #struct_name,
                            #field_name,
                            #kdl_key,
                            ::kdl_config::Placement::Value,
                        ));
                    }
                    let mut #values_ident = Vec::with_capacity(#child_ident.args().len());
                    for #child_arg_ident in #child_ident.args() {
                        #values_ident.push(::kdl_config::convert_value_checked::<#elem_ty>(#child_arg_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?);
                    }
                    ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, #values_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::Value)?;
                }
            }
        }

        let #field_ident: #ty = if let Some(val) = #field_ident {
            #finalize_value
        } else {
            #default_expr
        };
    }
}

fn generate_node_parser(
    field: &FieldInfo,
    struct_name: &str,
    field_overrides: &TokenStream,
    mark_usage: TokenStream,
) -> TokenStream {
    let field_ident = &field.ident;
    let field_name = field.ident.to_string();
    let kdl_key = &field.kdl_key;
    let ty = &field.ty;
    let is_optional = field.is_optional;
    let value_ty = if field.is_optional {
        field.inner_type().unwrap()
    } else {
        ty
    };
    let child_ident = format_ident!("__kdl_child_{}", field_ident);
    let child_iter = if field.placement.children_any {
        quote! { node.children() }
    } else {
        quote! { node.children_named(#kdl_key) }
    };

    let default_expr = generate_missing_expr(
        field,
        value_ty,
        struct_name,
        &field_name,
        kdl_key,
        false,
        quote! { ::kdl_config::Placement::Child },
    );
    let finalize_value = if is_optional {
        quote! { Some(val) }
    } else {
        quote! { val }
    };

    let allow_child = field.placement.child
        || field.placement.children
        || field.placement.children_any
        || field.placement.value;

    quote! {
        #mark_usage
        let field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
        let mut #field_ident: ::core::option::Option<#value_ty> = None;

        let has_placement = #allow_child;

        if !has_placement {
            match field_config.default_placement {
                ::kdl_config::DefaultPlacement::Exhaustive | ::kdl_config::DefaultPlacement::Child => {
                    for #child_ident in #child_iter {
                        let v = <#value_ty as ::kdl_config::KdlParse>::from_node(#child_ident, config)?;
                        ::kdl_config::helpers::resolve_scalar(
                            field_config.conflict,
                            &mut #field_ident,
                            v,
                            #struct_name,
                            #field_name,
                            #kdl_key,
                            ::kdl_config::Placement::Child,
                        )?;
                    }
                }
                ::kdl_config::DefaultPlacement::Attr | ::kdl_config::DefaultPlacement::Value => {
                    return Err(::kdl_config::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "attr/value placement is incompatible with nested node types",
                    ));
                }
            }
        } else {
            for #child_ident in #child_iter {
                let v = <#value_ty as ::kdl_config::KdlParse>::from_node(#child_ident, config)?;
                ::kdl_config::helpers::resolve_scalar(
                    field_config.conflict,
                    &mut #field_ident,
                    v,
                    #struct_name,
                    #field_name,
                    #kdl_key,
                    ::kdl_config::Placement::Child,
                )?;
            }
        }

        let #field_ident: #ty = if let Some(val) = #field_ident {
            #finalize_value
        } else {
            #default_expr
        };
    }
}

fn generate_flatten_parser(
    field: &FieldInfo,
    struct_name: &str,
    field_overrides: &TokenStream,
    mark_usage: TokenStream,
) -> TokenStream {
    let field_ident = &field.ident;
    let field_name = field.ident.to_string();
    let kdl_key = &field.kdl_key;
    let ty = &field.ty;
    let is_optional = field.is_optional;
    let value_ty = if field.is_optional {
        field.inner_type().unwrap()
    } else {
        ty
    };

    let default_expr = generate_missing_expr(
        field,
        value_ty,
        struct_name,
        &field_name,
        kdl_key,
        false,
        quote! { ::kdl_config::Placement::Unknown },
    );
    let finalize_value = if is_optional {
        quote! { Some(val) }
    } else {
        quote! { val }
    };

    quote! {
        #mark_usage
        let _field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
        let parsed = ::kdl_config::helpers::parse_flatten::<#value_ty>(node, config);
        let #field_ident: #ty = match parsed {
            Ok(val) => {
                #finalize_value
            }
            Err(err) => {
                if matches!(err.kind, ::kdl_config::ErrorKind::MissingRequired) {
                    #default_expr
                } else {
                    return Err(err);
                }
            }
        };
    }
}

fn generate_node_vec_parser(
    field: &FieldInfo,
    struct_name: &str,
    field_overrides: &TokenStream,
    mark_usage: TokenStream,
) -> TokenStream {
    let field_ident = &field.ident;
    let field_name = field.ident.to_string();
    let kdl_key = &field.kdl_key;
    let ty = &field.ty;
    let is_option_vec = field.is_option_vec;
    let inner_vec_ty = if is_option_vec {
        field.inner_type().and_then(extract_inner_type).unwrap()
    } else {
        field.inner_type().unwrap()
    };
    let elem_ty = extract_inner_type(inner_vec_ty).unwrap_or(inner_vec_ty);
    let children_ident = format_ident!("__kdl_children_{}", field_ident);
    let child_ident = format_ident!("__kdl_child_{}", field_ident);
    let values_ident = format_ident!("__kdl_values_{}", field_ident);
    let allow_child =
        field.placement.child || field.placement.children || field.placement.children_any;
    let children_iter = if field.placement.children_any {
        quote! { node.children() }
    } else {
        quote! { node.children_named(#kdl_key) }
    };

    let default_expr = generate_missing_expr(
        field,
        inner_vec_ty,
        struct_name,
        &field_name,
        kdl_key,
        true,
        quote! { ::kdl_config::Placement::Children },
    );
    let finalize_value = if is_option_vec {
        quote! { Some(val) }
    } else {
        quote! { val }
    };

    quote! {
        #mark_usage
        let field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
        let mut #field_ident: ::core::option::Option<Vec<#elem_ty>> = None;

        let has_placement = #allow_child;

        if !has_placement {
            match field_config.default_placement {
                ::kdl_config::DefaultPlacement::Exhaustive | ::kdl_config::DefaultPlacement::Child => {
                    let #children_ident = #children_iter.collect::<Vec<_>>();
                    if !#children_ident.is_empty() {
                        let mut #values_ident = Vec::with_capacity(#children_ident.len());
                        for #child_ident in #children_ident {
                            #values_ident.push(<#elem_ty as ::kdl_config::KdlParse>::from_node(#child_ident, config)?);
                        }
                        #field_ident = Some(#values_ident);
                    }
                }
                ::kdl_config::DefaultPlacement::Attr | ::kdl_config::DefaultPlacement::Value => {
                    return Err(::kdl_config::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "attr/value placement is incompatible with nested node Vec",
                    ));
                }
            }
        } else {
            let #children_ident = #children_iter.collect::<Vec<_>>();
            if !#children_ident.is_empty() {
                let mut #values_ident = Vec::with_capacity(#children_ident.len());
                for #child_ident in #children_ident {
                    #values_ident.push(<#elem_ty as ::kdl_config::KdlParse>::from_node(#child_ident, config)?);
                }
                #field_ident = Some(#values_ident);
            }
        }

        let #field_ident: #ty = if let Some(val) = #field_ident {
            #finalize_value
        } else {
            #default_expr
        };
    }
}

fn generate_registry_parser(
    field: &FieldInfo,
    struct_name: &str,
    field_overrides: &TokenStream,
    mark_usage: TokenStream,
) -> TokenStream {
    let field_ident = &field.ident;
    let field_name = field.ident.to_string();
    let kdl_key = &field.kdl_key;
    let ty = &field.ty;
    let container = field
        .container
        .as_ref()
        .map(|s| quote! { #s })
        .unwrap_or_else(|| quote! { #kdl_key });
    let child_ident = format_ident!("__kdl_child_{}", field_ident);
    let key_val_ident = format_ident!("__kdl_key_val_{}", field_ident);
    let key_ident = format_ident!("__kdl_key_{}", field_ident);
    let node_copy_ident = format_ident!("__kdl_node_copy_{}", field_ident);
    let value_ident = format_ident!("__kdl_value_{}", field_ident);
    let existing_idx_ident = format_ident!("__kdl_existing_idx_{}", field_ident);
    let required = field.required;

    let registry_key = field
        .registry_key
        .clone()
        .unwrap_or(crate::attrs::RegistryKey::Arg(0));

    let key_extract = match registry_key {
        crate::attrs::RegistryKey::Arg(index) => {
            quote! {
                let #key_val_ident = #child_ident.arg(#index).ok_or_else(|| {
                    ::kdl_config::KdlConfigError::invalid_registry_key(
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        "missing registry key",
                    )
                })?;
                let #key_ident = #key_val_ident.as_str().ok_or_else(|| {
                    ::kdl_config::KdlConfigError::invalid_registry_key(
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        "registry key must be a string",
                    )
                })?.to_string();
                let #node_copy_ident = #child_ident.without_arg(#index);
            }
        }
        crate::attrs::RegistryKey::Attr(name) => {
            quote! {
                let #key_val_ident = #child_ident.attr_values(#name).ok_or_else(|| {
                    ::kdl_config::KdlConfigError::invalid_registry_key(
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        "missing registry key",
                    )
                })?;
                if #key_val_ident.len() != 1 {
                    return Err(::kdl_config::KdlConfigError::invalid_registry_key(
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        "registry key attribute must have a single value",
                    ));
                }
                let #key_ident = #key_val_ident[0].as_str().ok_or_else(|| {
                    ::kdl_config::KdlConfigError::invalid_registry_key(
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        "registry key must be a string",
                    )
                })?.to_string();
                let #node_copy_ident = #child_ident.without_attr(#name);
            }
        }
        crate::attrs::RegistryKey::Function(path) => {
            let key_fn: TokenStream = path.parse().unwrap_or_else(|_| {
                let ident = syn::Ident::new(&path, proc_macro2::Span::call_site());
                quote! { #ident }
            });
            quote! {
                let #key_ident = #key_fn(#child_ident)?;
                let #node_copy_ident = #child_ident.clone();
            }
        }
    };

    if field.is_hashmap {
        let (_key_ty, val_ty) = extract_hashmap_types(ty).expect("registry requires HashMap");
        return quote! {
            #mark_usage
            let field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
            let mut #field_ident: #ty = ::std::collections::HashMap::new();

            for #child_ident in node.children_named(#container) {
                #key_extract
                let #value_ident = <#val_ty as ::kdl_config::KdlParse>::from_node(&#node_copy_ident, config)?;
                match field_config.conflict {
                    ::kdl_config::ConflictPolicy::Error => {
                        if #field_ident.contains_key(&#key_ident) {
                            return Err(::kdl_config::KdlConfigError::invalid_registry_key(
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                "duplicate registry key",
                            ));
                        }
                        #field_ident.insert(#key_ident.clone(), #value_ident);
                    }
                    ::kdl_config::ConflictPolicy::First => {
                        #field_ident.entry(#key_ident.clone()).or_insert(#value_ident);
                    }
                    ::kdl_config::ConflictPolicy::Last => {
                        #field_ident.insert(#key_ident.clone(), #value_ident);
                    }
                    ::kdl_config::ConflictPolicy::Append => {
                        return Err(::kdl_config::KdlConfigError::incompatible_placement(
                            #struct_name,
                            #field_name,
                            "append conflict policy is not supported for HashMap registry fields",
                        ));
                    }
                }
            }

            if #required && #field_ident.is_empty() {
                return Err(::kdl_config::KdlConfigError::missing_required(
                    #struct_name,
                    #field_name,
                    #container,
                    ::kdl_config::Placement::Registry,
                ));
            }
        };
    }

    let (val_ty, is_option_vec) = crate::attrs::extract_registry_vec_value(ty)
        .expect("registry vec requires Vec<(String, T)>");

    let finalize_vec = if is_option_vec {
        quote! {
            let #field_ident: #ty = if #field_ident.is_empty() { None } else { Some(#field_ident) };
        }
    } else {
        quote! {}
    };

    quote! {
        #mark_usage
        let field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
        let mut #field_ident: Vec<(String, #val_ty)> = Vec::new();

        for #child_ident in node.children_named(#container) {
            #key_extract
            let #value_ident = <#val_ty as ::kdl_config::KdlParse>::from_node(&#node_copy_ident, config)?;
            match field_config.conflict {
                ::kdl_config::ConflictPolicy::Error => {
                    if #field_ident.iter().any(|(key, _)| key == &#key_ident) {
                        return Err(::kdl_config::KdlConfigError::invalid_registry_key(
                            #struct_name,
                            #field_name,
                            #kdl_key,
                            "duplicate registry key",
                        ));
                    }
                    #field_ident.push((#key_ident.clone(), #value_ident));
                }
                ::kdl_config::ConflictPolicy::First => {
                    if !#field_ident.iter().any(|(key, _)| key == &#key_ident) {
                        #field_ident.push((#key_ident.clone(), #value_ident));
                    }
                }
                ::kdl_config::ConflictPolicy::Last => {
                    if let Some(#existing_idx_ident) = #field_ident.iter().position(|(key, _)| key == &#key_ident) {
                        #field_ident.remove(#existing_idx_ident);
                    }
                    #field_ident.push((#key_ident.clone(), #value_ident));
                }
                ::kdl_config::ConflictPolicy::Append => {
                    #field_ident.push((#key_ident.clone(), #value_ident));
                }
            }
        }

        if #required && #field_ident.is_empty() {
            return Err(::kdl_config::KdlConfigError::missing_required(
                #struct_name,
                #field_name,
                #container,
                ::kdl_config::Placement::Registry,
            ));
        }

        #finalize_vec
    }
}

fn generate_children_map_parser(
    field: &FieldInfo,
    struct_name: &str,
    field_overrides: &TokenStream,
    mark_usage: TokenStream,
) -> TokenStream {
    let field_ident = &field.ident;
    let field_name = field.ident.to_string();
    let kdl_key = &field.kdl_key;
    let ty = &field.ty;
    let child_ident = format_ident!("__kdl_child_{}", field_ident);
    let key_val_ident = format_ident!("__kdl_key_val_{}", field_ident);
    let key_ident = format_ident!("__kdl_key_{}", field_ident);
    let value_ident = format_ident!("__kdl_value_{}", field_ident);
    let node_copy_ident = format_ident!("__kdl_node_copy_{}", field_ident);
    let existing_idx_ident = format_ident!("__kdl_existing_idx_{}", field_ident);
    let required = field.required;
    let map_node = field.map_node.as_ref().map(|s| quote! { #s });

    let (children_map_kind, key_ty, val_ty) =
        extract_children_map_types(ty).expect("children_map requires HashMap or Vec<(K, V)>");

    let missing_key = if let Some(map_node) = map_node.as_ref() {
        quote! { #map_node }
    } else {
        quote! { #kdl_key }
    };

    let duplicate_error = quote! {
        return Err(::kdl_config::KdlConfigError {
            struct_name: #struct_name.to_string(),
            field_name: Some(#field_name.to_string()),
            kdl_key: Some(#kdl_key.to_string()),
            placement: ::kdl_config::Placement::Children,
            required: false,
            kind: ::kdl_config::ErrorKind::Custom("duplicate map key".to_string()),
        });
    };

    let missing_arg_error = quote! {
        ::kdl_config::KdlConfigError {
            struct_name: #struct_name.to_string(),
            field_name: Some(#field_name.to_string()),
            kdl_key: Some(#kdl_key.to_string()),
            placement: ::kdl_config::Placement::Children,
            required: false,
            kind: ::kdl_config::ErrorKind::Custom("missing map key argument".to_string()),
        }
    };
    let missing_attr_error = quote! {
        ::kdl_config::KdlConfigError {
            struct_name: #struct_name.to_string(),
            field_name: Some(#field_name.to_string()),
            kdl_key: Some(#kdl_key.to_string()),
            placement: ::kdl_config::Placement::Children,
            required: false,
            kind: ::kdl_config::ErrorKind::Custom("missing map key attribute".to_string()),
        }
    };

    let registry_key = field
        .registry_key
        .clone()
        .unwrap_or(crate::attrs::RegistryKey::Arg(0));

    let key_extract = if map_node.is_some() {
        match registry_key {
            crate::attrs::RegistryKey::Arg(index) => {
                quote! {
                    let #key_val_ident = #child_ident.arg(#index).ok_or_else(|| {
                        #missing_arg_error
                    })?;
                    let #key_ident = ::kdl_config::convert_value_checked::<#key_ty>(
                        #key_val_ident,
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        ::kdl_config::Placement::Children,
                    )?;
                    let #node_copy_ident = #child_ident.without_arg(#index);
                }
            }
            crate::attrs::RegistryKey::Attr(name) => {
                quote! {
                    let #key_val_ident = #child_ident.attr_values(#name).ok_or_else(|| {
                        #missing_attr_error
                    })?;
                    if #key_val_ident.len() != 1 {
                        return Err(::kdl_config::KdlConfigError {
                            struct_name: #struct_name.to_string(),
                            field_name: Some(#field_name.to_string()),
                            kdl_key: Some(#kdl_key.to_string()),
                            placement: ::kdl_config::Placement::Children,
                            required: false,
                            kind: ::kdl_config::ErrorKind::Custom("map key attribute must have a single value".to_string()),
                        });
                    }
                    let #key_ident = ::kdl_config::convert_value_checked::<#key_ty>(
                        &#key_val_ident[0],
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        ::kdl_config::Placement::Children,
                    )?;
                    let #node_copy_ident = #child_ident.without_attr(#name);
                }
            }
            crate::attrs::RegistryKey::Function(path) => {
                let key_fn: TokenStream = path.parse().unwrap_or_else(|_| {
                    let ident = syn::Ident::new(&path, proc_macro2::Span::call_site());
                    quote! { #ident }
                });
                quote! {
                    let #key_ident = #key_fn(#child_ident)?;
                    let #node_copy_ident = #child_ident.clone();
                }
            }
        }
    } else {
        quote! {
            let #key_val_ident = ::kdl_config::Value::String(#child_ident.name.clone());
            let #key_ident = ::kdl_config::convert_value_checked::<#key_ty>(
                &#key_val_ident,
                #struct_name,
                #field_name,
                #kdl_key,
                ::kdl_config::Placement::Children,
            )?;
        }
    };

    let (field_init, map_insert, finalize_vec) = match children_map_kind {
        ChildrenMapKind::HashMap => (
            quote! { let mut #field_ident: #ty = ::std::collections::HashMap::new(); },
            quote! {
                match field_config.conflict {
                    ::kdl_config::ConflictPolicy::Error => {
                        if #field_ident.contains_key(&#key_ident) {
                            #duplicate_error
                        }
                        #field_ident.insert(#key_ident, #value_ident);
                    }
                    ::kdl_config::ConflictPolicy::First => {
                        #field_ident.entry(#key_ident).or_insert(#value_ident);
                    }
                    ::kdl_config::ConflictPolicy::Last => {
                        #field_ident.insert(#key_ident, #value_ident);
                    }
                    ::kdl_config::ConflictPolicy::Append => {
                        return Err(::kdl_config::KdlConfigError::incompatible_placement(
                            #struct_name,
                            #field_name,
                            "append conflict policy is not supported for children_map HashMap fields",
                        ));
                    }
                }
            },
            quote! {},
        ),
        ChildrenMapKind::Vec => (
            quote! { let mut #field_ident: Vec<(#key_ty, #val_ty)> = Vec::new(); },
            quote! {
                match field_config.conflict {
                    ::kdl_config::ConflictPolicy::Error => {
                        if #field_ident.iter().any(|(key, _)| key == &#key_ident) {
                            #duplicate_error
                        }
                        #field_ident.push((#key_ident, #value_ident));
                    }
                    ::kdl_config::ConflictPolicy::First => {
                        if !#field_ident.iter().any(|(key, _)| key == &#key_ident) {
                            #field_ident.push((#key_ident, #value_ident));
                        }
                    }
                    ::kdl_config::ConflictPolicy::Last => {
                        if let Some(#existing_idx_ident) = #field_ident.iter().position(|(key, _)| key == &#key_ident) {
                            #field_ident.remove(#existing_idx_ident);
                        }
                        #field_ident.push((#key_ident, #value_ident));
                    }
                    ::kdl_config::ConflictPolicy::Append => {
                        #field_ident.push((#key_ident, #value_ident));
                    }
                }
            },
            quote! {},
        ),
        ChildrenMapKind::OptionVec => (
            quote! { let mut #field_ident: Vec<(#key_ty, #val_ty)> = Vec::new(); },
            quote! {
                match field_config.conflict {
                    ::kdl_config::ConflictPolicy::Error => {
                        if #field_ident.iter().any(|(key, _)| key == &#key_ident) {
                            #duplicate_error
                        }
                        #field_ident.push((#key_ident, #value_ident));
                    }
                    ::kdl_config::ConflictPolicy::First => {
                        if !#field_ident.iter().any(|(key, _)| key == &#key_ident) {
                            #field_ident.push((#key_ident, #value_ident));
                        }
                    }
                    ::kdl_config::ConflictPolicy::Last => {
                        if let Some(#existing_idx_ident) = #field_ident.iter().position(|(key, _)| key == &#key_ident) {
                            #field_ident.remove(#existing_idx_ident);
                        }
                        #field_ident.push((#key_ident, #value_ident));
                    }
                    ::kdl_config::ConflictPolicy::Append => {
                        #field_ident.push((#key_ident, #value_ident));
                    }
                }
            },
            quote! {
                let #field_ident: #ty = if #field_ident.is_empty() { None } else { Some(#field_ident) };
            },
        ),
    };

    let map_loop = if let Some(map_node) = map_node {
        quote! {
            if struct_config.deny_unknown {
                used_keys.mark_child(#map_node);
            }
            for #child_ident in node.children_named(#map_node) {
                #key_extract
                let #value_ident = <#val_ty as ::kdl_config::KdlParse>::from_node(&#node_copy_ident, config)?;
                #map_insert
            }
        }
    } else {
        quote! {
            for #child_ident in node.children() {
                if struct_config.deny_unknown {
                    used_keys.mark_child(&#child_ident.name);
                }
                #key_extract
                let #value_ident = <#val_ty as ::kdl_config::KdlParse>::from_node(#child_ident, config)?;
                #map_insert
            }
        }
    };

    quote! {
        #mark_usage
        let field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
        #field_init

        #map_loop

        if #required && #field_ident.is_empty() {
            return Err(::kdl_config::KdlConfigError::missing_required(
                #struct_name,
                #field_name,
                #missing_key,
                ::kdl_config::Placement::Children,
            ));
        }
        #finalize_vec
    }
}

fn generate_missing_expr(
    field: &FieldInfo,
    ty: &syn::Type,
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
    is_vec: bool,
    missing_placement: TokenStream,
) -> TokenStream {
    if field.is_bool && !field.is_optional && field.default.is_none() && !field.required {
        return quote! { false };
    }

    if field.is_optional {
        if let Some(ref default_spec) = field.default {
            let inner = field.inner_type().unwrap();
            let default_expr = generate_default_expr(default_spec, inner);
            quote! { Some(#default_expr) }
        } else {
            quote! { None }
        }
    } else if let Some(ref default_spec) = field.default {
        generate_default_expr(default_spec, ty)
    } else if is_vec {
        let required = field.required;
        quote! {
            if #required {
                return Err(::kdl_config::KdlConfigError::missing_required(
                    #struct_name,
                    #field_name,
                    #kdl_key,
                    #missing_placement,
                ));
            }
            ::std::vec::Vec::new()
        }
    } else {
        quote! {
            return Err(::kdl_config::KdlConfigError::missing_required(
                #struct_name,
                #field_name,
                #kdl_key,
                ::kdl_config::Placement::Unknown,
            ));
        }
    }
}

fn generate_default_expr(default_spec: &DefaultSpec, ty: &syn::Type) -> TokenStream {
    match default_spec {
        DefaultSpec::Derive => quote! { ::std::default::Default::default() },
        DefaultSpec::Function(path) => {
            let path_tokens: TokenStream = path.parse().unwrap_or_else(|_| {
                let ident = syn::Ident::new(path, proc_macro2::Span::call_site());
                quote! { #ident }
            });
            quote! { #path_tokens() }
        }
        DefaultSpec::Literal(lit) => generate_literal_default_expr(lit, ty),
    }
}

fn generate_literal_default_expr(lit: &DefaultLiteral, ty: &syn::Type) -> TokenStream {
    match lit {
        DefaultLiteral::Int(n) => {
            if is_numeric_type(ty) {
                quote! { #n as #ty }
            } else {
                quote! {
                    ::kdl_config::FromKdlValue::from_value(
                        &::kdl_config::Value::Int(#n)
                    ).expect(concat!("invalid default value for ", stringify!(#ty)))
                }
            }
        }
        DefaultLiteral::Float(f) => {
            if is_numeric_type(ty) {
                quote! { #f as #ty }
            } else {
                quote! {
                    ::kdl_config::FromKdlValue::from_value(
                        &::kdl_config::Value::Float(#f)
                    ).expect(concat!("invalid default value for ", stringify!(#ty)))
                }
            }
        }
        DefaultLiteral::Bool(b) => {
            if is_bool_type(ty) {
                quote! { #b }
            } else {
                quote! {
                    ::kdl_config::FromKdlValue::from_value(
                        &::kdl_config::Value::Bool(#b)
                    ).expect(concat!("invalid default value for ", stringify!(#ty)))
                }
            }
        }
        DefaultLiteral::String(s) => {
            if is_string_type(ty) {
                quote! { ::std::string::String::from(#s) }
            } else {
                quote! {
                    ::kdl_config::FromKdlValue::from_value(
                        &::kdl_config::Value::String(::std::string::String::from(#s))
                    ).expect(concat!("invalid default value for ", stringify!(#ty)))
                }
            }
        }
    }
}
