use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::attrs::{
    BoolMode, ConflictPolicy, DefaultLiteral, DefaultPlacement, DefaultSpec, FieldInfo, FlagStyle,
    StructAttrs, extract_hashmap_types, extract_inner_type, is_bool_type, is_numeric_type,
    is_string_type,
};

#[derive(Debug, Clone, Copy)]
enum FieldKind {
    ValueScalar,
    ValueVec,
    Node,
    NodeVec,
    Registry,
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

    let validate_name = if let Some(ref node_name) = struct_attrs.node_name {
        quote! {
            if node.name != #node_name {
                return Err(::kdl_config_runtime::KdlConfigError::node_name_mismatch(
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
        impl ::kdl_config_runtime::KdlParse for #struct_name {
            fn from_node(node: &::kdl_config_runtime::Node, config: &::kdl_config_runtime::ParseConfig) -> ::core::result::Result<Self, ::kdl_config_runtime::KdlConfigError> {
                #validate_name
                let struct_overrides = #struct_overrides;
                let struct_config = ::kdl_config_runtime::resolve_struct(config, struct_overrides);
                let mut used_keys = ::kdl_config_runtime::helpers::UsedKeys::new();

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
        None => quote! { None },
    };

    let default_bool = match struct_attrs.default_bool {
        Some(BoolMode::PresenceAndValue) => {
            quote! { Some(::kdl_config_runtime::BoolMode::PresenceAndValue) }
        }
        Some(BoolMode::ValueOnly) => quote! { Some(::kdl_config_runtime::BoolMode::ValueOnly) },
        Some(BoolMode::PresenceOnly) => {
            quote! { Some(::kdl_config_runtime::BoolMode::PresenceOnly) }
        }
        None => quote! { None },
    };

    let default_flag_style = match struct_attrs.default_flag_style {
        Some(FlagStyle::Both) => quote! { Some(::kdl_config_runtime::FlagStyle::Both) },
        Some(FlagStyle::ValueNo) => quote! { Some(::kdl_config_runtime::FlagStyle::ValueNo) },
        Some(FlagStyle::WithWithout) => {
            quote! { Some(::kdl_config_runtime::FlagStyle::WithWithout) }
        }
        None => quote! { None },
    };

    let default_conflict = match struct_attrs.default_conflict {
        Some(ConflictPolicy::Error) => quote! { Some(::kdl_config_runtime::ConflictPolicy::Error) },
        Some(ConflictPolicy::First) => quote! { Some(::kdl_config_runtime::ConflictPolicy::First) },
        Some(ConflictPolicy::Last) => quote! { Some(::kdl_config_runtime::ConflictPolicy::Last) },
        Some(ConflictPolicy::Append) => {
            quote! { Some(::kdl_config_runtime::ConflictPolicy::Append) }
        }
        None => quote! { None },
    };

    let deny_unknown = match struct_attrs.deny_unknown {
        Some(true) => quote! { Some(true) },
        Some(false) => quote! { Some(false) },
        None => quote! { None },
    };

    quote! {
        ::kdl_config_runtime::StructOverrides {
            default_placement: #default_placement,
            default_bool: #default_bool,
            default_flag_style: #default_flag_style,
            default_conflict: #default_conflict,
            deny_unknown: #deny_unknown,
        }
    }
}

pub(crate) fn generate_field_parser(field: &FieldInfo, struct_name: &str) -> TokenStream {
    if field.is_hashmap && !field.placement.registry {
        return quote! { compile_error!("HashMap fields require #[kdl(registry)]"); };
    }
    if crate::attrs::extract_registry_vec_value(&field.ty).is_some() && !field.placement.registry {
        return quote! { compile_error!("Vec<(String, T)> registry fields require #[kdl(registry)]"); };
    }

    let field_overrides = generate_field_overrides(field);
    let mark_usage = generate_usage_marks(field);

    match field_kind(field) {
        FieldKind::Registry => {
            generate_registry_parser(field, struct_name, &field_overrides, mark_usage)
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
            quote! { Some(::kdl_config_runtime::BoolMode::PresenceAndValue) }
        }
        Some(BoolMode::ValueOnly) => quote! { Some(::kdl_config_runtime::BoolMode::ValueOnly) },
        Some(BoolMode::PresenceOnly) => {
            quote! { Some(::kdl_config_runtime::BoolMode::PresenceOnly) }
        }
        None => quote! { None },
    };

    let flag_style = match field.flag_style {
        Some(FlagStyle::Both) => quote! { Some(::kdl_config_runtime::FlagStyle::Both) },
        Some(FlagStyle::ValueNo) => quote! { Some(::kdl_config_runtime::FlagStyle::ValueNo) },
        Some(FlagStyle::WithWithout) => {
            quote! { Some(::kdl_config_runtime::FlagStyle::WithWithout) }
        }
        None => quote! { None },
    };

    let conflict = match field.conflict {
        Some(ConflictPolicy::Error) => quote! { Some(::kdl_config_runtime::ConflictPolicy::Error) },
        Some(ConflictPolicy::First) => quote! { Some(::kdl_config_runtime::ConflictPolicy::First) },
        Some(ConflictPolicy::Last) => quote! { Some(::kdl_config_runtime::ConflictPolicy::Last) },
        Some(ConflictPolicy::Append) => {
            quote! { Some(::kdl_config_runtime::ConflictPolicy::Append) }
        }
        None => quote! { None },
    };

    quote! {
        ::kdl_config_runtime::FieldOverrides {
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
    if field.placement.registry {
        return FieldKind::Registry;
    }

    if field.is_vec || field.is_option_vec {
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
    } else if is_value_type(&field.ty) || field.is_scalar {
        FieldKind::ValueScalar
    } else {
        FieldKind::Node
    }
}

fn is_value_type(ty: &syn::Type) -> bool {
    if is_bool_type(ty) || is_string_type(ty) || is_numeric_type(ty) {
        return true;
    }

    if let Some(inner) = extract_inner_type(ty) {
        if crate::attrs::is_option_type(ty) {
            return is_value_type(inner);
        }
    }

    false
}

fn generate_usage_marks(field: &FieldInfo) -> TokenStream {
    if field.is_modifier {
        return quote! {};
    }
    let mut marks = Vec::new();
    let key = field.kdl_key.clone();
    let container = field.container.clone().unwrap_or_else(|| key.clone());
    let has_explicit = field.placement.attr
        || field.placement.keyed
        || field.placement.positional.is_some()
        || field.placement.flag.is_some()
        || field.placement.value
        || field.placement.child
        || field.placement.children
        || field.placement.registry;

    if field.placement.registry {
        marks.push(quote! { used_keys.mark_child(#container); });
    } else if has_explicit {
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
                        ::kdl_config_runtime::DefaultPlacement::Exhaustive => {
                            used_keys.mark_attr(#key);
                            used_keys.mark_child(#key);
                        }
                        ::kdl_config_runtime::DefaultPlacement::Attr => {
                            used_keys.mark_attr(#key);
                        }
                        ::kdl_config_runtime::DefaultPlacement::Value | ::kdl_config_runtime::DefaultPlacement::Child => {
                            used_keys.mark_child(#key);
                        }
                    }
                });
            }
            FieldKind::Node | FieldKind::NodeVec => {
                marks.push(quote! {
                    match struct_config.default_placement {
                        ::kdl_config_runtime::DefaultPlacement::Exhaustive | ::kdl_config_runtime::DefaultPlacement::Child => {
                            used_keys.mark_child(#key);
                        }
                        ::kdl_config_runtime::DefaultPlacement::Attr | ::kdl_config_runtime::DefaultPlacement::Value => {}
                    }
                });
            }
            FieldKind::Registry => {
                marks.push(quote! { used_keys.mark_child(#container); });
            }
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

    let mut marks = Vec::new();
    let key = field.kdl_key.clone();
    let container = field.container.clone().unwrap_or_else(|| key.clone());

    let has_explicit = field.placement.attr
        || field.placement.keyed
        || field.placement.positional.is_some()
        || field.placement.flag.is_some()
        || field.placement.value
        || field.placement.child
        || field.placement.children
        || field.placement.registry;

    if field.placement.registry {
        marks.push(quote! { used_keys.mark_child(#container); });
    } else if has_explicit {
        if field.placement.attr || field.placement.keyed {
            marks.push(quote! { used_keys.mark_attr(#key); });
        }
        if let Some(idx) = field.placement.positional {
            marks.push(quote! { used_keys.mark_arg(#idx); });
        }
        if field.placement.value || field.placement.child || field.placement.children {
            marks.push(quote! { used_keys.mark_child(#key); });
        }
    } else {
        match field_kind(field) {
            FieldKind::ValueScalar | FieldKind::ValueVec => {
                marks.push(quote! {
                    match struct_config.default_placement {
                        ::kdl_config_runtime::DefaultPlacement::Exhaustive => {
                            used_keys.mark_attr(#key);
                            used_keys.mark_child(#key);
                        }
                        ::kdl_config_runtime::DefaultPlacement::Attr => {
                            used_keys.mark_attr(#key);
                        }
                        ::kdl_config_runtime::DefaultPlacement::Value | ::kdl_config_runtime::DefaultPlacement::Child => {
                            used_keys.mark_child(#key);
                        }
                    }
                });
            }
            FieldKind::Node | FieldKind::NodeVec => {
                marks.push(quote! {
                    match struct_config.default_placement {
                        ::kdl_config_runtime::DefaultPlacement::Exhaustive | ::kdl_config_runtime::DefaultPlacement::Child => {
                            used_keys.mark_child(#key);
                        }
                        ::kdl_config_runtime::DefaultPlacement::Attr | ::kdl_config_runtime::DefaultPlacement::Value => {}
                    }
                });
            }
            FieldKind::Registry => {
                marks.push(quote! { used_keys.mark_child(#container); });
            }
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
            matches!(struct_config.default_placement, ::kdl_config_runtime::DefaultPlacement::Exhaustive | ::kdl_config_runtime::DefaultPlacement::Attr)
        };

        flag_marks.push(quote! {
            if #field_config_ident.bool_mode != ::kdl_config_runtime::BoolMode::ValueOnly {
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
                        ::kdl_config_runtime::helpers::flag_names_for_key(#key, #field_config_ident.flag_style)
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
                let #field_config_ident = ::kdl_config_runtime::resolve_field(&struct_config, #field_overrides);
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
            let #field_ident: #ty = if node.modifier == ::kdl_config_runtime::Modifier::Inherit {
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
        quote! { ::kdl_config_runtime::Placement::Unknown },
    );
    let finalize_value = if is_optional {
        quote! { Some(val) }
    } else {
        quote! { val }
    };

    quote! {
        #mark_usage
        let field_config = ::kdl_config_runtime::resolve_field(&struct_config, #field_overrides);
        let mut #field_ident: ::core::option::Option<#value_ty> = None;

        let custom_pos = #custom_pos_expr;
        let custom_neg = #custom_neg_expr;
        let pos_index = #pos_index_expr;
        let has_placement = #allow_attr_keyed || #allow_value || pos_index.is_some() || #allow_flags;

        if !has_placement {
            if #is_bool && field_config.bool_mode == ::kdl_config_runtime::BoolMode::ValueOnly {
                let flag_val = ::kdl_config_runtime::helpers::find_flag_with_style(
                    node,
                    #kdl_key,
                    field_config.flag_style,
                    custom_pos.clone(),
                    custom_neg.clone(),
                    #struct_name,
                    #field_name,
                )?;
                if flag_val.is_some() {
                    return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "flag placement is not allowed for value-only bools",
                    ));
                }
            }
            match field_config.default_placement {
                ::kdl_config_runtime::DefaultPlacement::Exhaustive => {
                    if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                        for #attr_value_ident in #attr_values_ident {
                            if field_config.bool_mode == ::kdl_config_runtime::BoolMode::PresenceOnly {
                                return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "explicit values are not allowed in presence-only mode",
                                ));
                            }
                            let v = ::kdl_config_runtime::convert_value_checked::<#value_ty>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrKeyed)?;
                            ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrKeyed)?;
                        }
                    }

                    if let Some(idx) = pos_index {
                        if let Some(#arg_value_ident) = node.arg(idx) {
                            if field_config.bool_mode == ::kdl_config_runtime::BoolMode::PresenceOnly {
                                return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "explicit values are not allowed in presence-only mode",
                                ));
                            }
                            let v = ::kdl_config_runtime::convert_value_checked::<#value_ty>(#arg_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrPositional)?;
                            ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrPositional)?;
                            if struct_config.deny_unknown {
                                used_keys.mark_arg(idx);
                            }
                        }
                    }

                    if #is_bool && field_config.bool_mode != ::kdl_config_runtime::BoolMode::ValueOnly {
                        let flag_val = if struct_config.deny_unknown {
                            ::kdl_config_runtime::helpers::find_flag_with_style_marked(
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
                            ::kdl_config_runtime::helpers::find_flag_with_style(
                                node,
                                #kdl_key,
                                field_config.flag_style,
                                custom_pos.clone(),
                                custom_neg.clone(),
                                #struct_name,
                                #field_name,
                            )?
                        };
                        if field_config.bool_mode == ::kdl_config_runtime::BoolMode::PresenceOnly {
                            if let Some(false) = flag_val {
                                return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "negative flags are not allowed in presence-only mode",
                                ));
                            }
                        }
                        if let Some(flag_val) = flag_val {
                            let v: #value_ty = ::kdl_config_runtime::convert_value_checked(&::kdl_config_runtime::Value::Bool(flag_val), #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrFlag)?;
                            ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrFlag)?;
                        }
                    }

                    for #child_ident in node.children_named(#kdl_key) {
                        if field_config.bool_mode == ::kdl_config_runtime::BoolMode::PresenceOnly {
                            if #child_ident.args().is_empty() && #child_ident.children().is_empty() && #child_ident.attrs().is_empty() {
                                let v: #value_ty = ::kdl_config_runtime::convert_value_checked(&::kdl_config_runtime::Value::Bool(true), #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                                ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                            } else {
                                return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "explicit values are not allowed in presence-only mode",
                                ));
                            }
                        } else {
                            if #child_ident.args().is_empty() {
                                if #is_bool && field_config.bool_mode != ::kdl_config_runtime::BoolMode::ValueOnly && #child_ident.children().is_empty() && #child_ident.attrs().is_empty() {
                                    let v: #value_ty = ::kdl_config_runtime::convert_value_checked(&::kdl_config_runtime::Value::Bool(true), #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                                    ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                                } else {
                                    return Err(::kdl_config_runtime::KdlConfigError::missing_required(
                                        #struct_name,
                                        #field_name,
                                        #kdl_key,
                                        ::kdl_config_runtime::Placement::Value,
                                    ));
                                }
                            } else if #child_ident.args().len() == 1 {
                                let #child_arg_ident = &#child_ident.args()[0];
                                let v = ::kdl_config_runtime::convert_value_checked::<#value_ty>(#child_arg_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                                ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                            } else {
                                return Err(::kdl_config_runtime::KdlConfigError::too_many(
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config_runtime::Placement::Value,
                                    #child_ident.args().len(),
                                ));
                            }
                        }
                    }
                }
                ::kdl_config_runtime::DefaultPlacement::Attr => {
                    if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                        for #attr_value_ident in #attr_values_ident {
                            if field_config.bool_mode == ::kdl_config_runtime::BoolMode::PresenceOnly {
                                return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "explicit values are not allowed in presence-only mode",
                                ));
                            }
                            let v = ::kdl_config_runtime::convert_value_checked::<#value_ty>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrKeyed)?;
                            ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrKeyed)?;
                        }
                    }
                    if #is_bool && field_config.bool_mode != ::kdl_config_runtime::BoolMode::ValueOnly {
                        let flag_val = if struct_config.deny_unknown {
                            ::kdl_config_runtime::helpers::find_flag_with_style_marked(
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
                            ::kdl_config_runtime::helpers::find_flag_with_style(
                                node,
                                #kdl_key,
                                field_config.flag_style,
                                custom_pos.clone(),
                                custom_neg.clone(),
                                #struct_name,
                                #field_name,
                            )?
                        };
                        if field_config.bool_mode == ::kdl_config_runtime::BoolMode::PresenceOnly {
                            if let Some(false) = flag_val {
                                return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "negative flags are not allowed in presence-only mode",
                                ));
                            }
                        }
                        if let Some(flag_val) = flag_val {
                            let v: #value_ty = ::kdl_config_runtime::convert_value_checked(&::kdl_config_runtime::Value::Bool(flag_val), #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrFlag)?;
                            ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrFlag)?;
                        }
                    }
                }
                ::kdl_config_runtime::DefaultPlacement::Value => {
                    for #child_ident in node.children_named(#kdl_key) {
                        if field_config.bool_mode == ::kdl_config_runtime::BoolMode::PresenceOnly {
                            if #child_ident.args().is_empty() && #child_ident.children().is_empty() && #child_ident.attrs().is_empty() {
                                let v: #value_ty = ::kdl_config_runtime::convert_value_checked(
                                    &::kdl_config_runtime::Value::Bool(true),
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config_runtime::Placement::Value,
                                )?;
                                ::kdl_config_runtime::helpers::resolve_scalar(
                                    field_config.conflict,
                                    &mut #field_ident,
                                    v,
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config_runtime::Placement::Value,
                                )?;
                            } else {
                                return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "explicit values are not allowed in presence-only mode",
                                ));
                            }
                        } else {
                            if #child_ident.args().is_empty() {
                                if #is_bool && field_config.bool_mode != ::kdl_config_runtime::BoolMode::ValueOnly && #child_ident.children().is_empty() && #child_ident.attrs().is_empty() {
                                    let v: #value_ty = ::kdl_config_runtime::convert_value_checked(
                                        &::kdl_config_runtime::Value::Bool(true),
                                        #struct_name,
                                        #field_name,
                                        #kdl_key,
                                        ::kdl_config_runtime::Placement::Value,
                                    )?;
                                    ::kdl_config_runtime::helpers::resolve_scalar(
                                        field_config.conflict,
                                        &mut #field_ident,
                                        v,
                                        #struct_name,
                                        #field_name,
                                        #kdl_key,
                                        ::kdl_config_runtime::Placement::Value,
                                    )?;
                                } else {
                                    return Err(::kdl_config_runtime::KdlConfigError::missing_required(
                                        #struct_name,
                                        #field_name,
                                        #kdl_key,
                                        ::kdl_config_runtime::Placement::Value,
                                    ));
                                }
                            } else if #child_ident.args().len() == 1 {
                                let v = ::kdl_config_runtime::convert_value_checked::<#value_ty>(
                                    &#child_ident.args()[0],
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config_runtime::Placement::Value,
                                )?;
                                ::kdl_config_runtime::helpers::resolve_scalar(
                                    field_config.conflict,
                                    &mut #field_ident,
                                    v,
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config_runtime::Placement::Value,
                                )?;
                            } else {
                                return Err(::kdl_config_runtime::KdlConfigError::too_many(
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config_runtime::Placement::Value,
                                    #child_ident.args().len(),
                                ));
                            }
                        }
                    }
                }
                ::kdl_config_runtime::DefaultPlacement::Child => {
                    return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "child placement is incompatible with scalar value types",
                    ));
                }
            }
        } else {
            if !#allow_attr_keyed {
                if node.attr_values(#kdl_key).is_some() {
                    return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "keyed attribute placement is not allowed for this field",
                    ));
                }
            }

            if !#allow_value {
                if node.children_named(#kdl_key).next().is_some() {
                    return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "value placement is not allowed for this field",
                    ));
                }
            }

            if #is_bool && (!#allow_flags || field_config.bool_mode == ::kdl_config_runtime::BoolMode::ValueOnly) {
                let flag_val = ::kdl_config_runtime::helpers::find_flag_with_style(
                    node,
                    #kdl_key,
                    field_config.flag_style,
                    custom_pos.clone(),
                    custom_neg.clone(),
                    #struct_name,
                    #field_name,
                )?;
                if flag_val.is_some() {
                    return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "flag placement is not allowed for this field",
                    ));
                }
            }

            if #allow_attr_keyed {
                if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                    for #attr_value_ident in #attr_values_ident {
                        if field_config.bool_mode == ::kdl_config_runtime::BoolMode::PresenceOnly {
                            return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                                #struct_name,
                                #field_name,
                                "explicit values are not allowed in presence-only mode",
                            ));
                        }
                        let v = ::kdl_config_runtime::convert_value_checked::<#value_ty>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrKeyed)?;
                        ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrKeyed)?;
                    }
                }
            }

            if let Some(idx) = pos_index {
                if let Some(#arg_value_ident) = node.arg(idx) {
                    if field_config.bool_mode == ::kdl_config_runtime::BoolMode::PresenceOnly {
                        return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                            #struct_name,
                            #field_name,
                            "explicit values are not allowed in presence-only mode",
                        ));
                    }
                    let v = ::kdl_config_runtime::convert_value_checked::<#value_ty>(#arg_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrPositional)?;
                    ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrPositional)?;
                    if struct_config.deny_unknown {
                        used_keys.mark_arg(idx);
                    }
                }
            }

            if #is_bool && #allow_flags && field_config.bool_mode != ::kdl_config_runtime::BoolMode::ValueOnly {
                let flag_val = if struct_config.deny_unknown {
                    ::kdl_config_runtime::helpers::find_flag_with_style_marked(
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
                    ::kdl_config_runtime::helpers::find_flag_with_style(
                        node,
                        #kdl_key,
                        field_config.flag_style,
                        custom_pos.clone(),
                        custom_neg.clone(),
                        #struct_name,
                        #field_name,
                    )?
                };
                if field_config.bool_mode == ::kdl_config_runtime::BoolMode::PresenceOnly {
                    if let Some(false) = flag_val {
                        return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                            #struct_name,
                            #field_name,
                            "negative flags are not allowed in presence-only mode",
                        ));
                    }
                }
                if let Some(flag_val) = flag_val {
                    let v: #value_ty = ::kdl_config_runtime::convert_value_checked(&::kdl_config_runtime::Value::Bool(flag_val), #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrFlag)?;
                    ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrFlag)?;
                }
            }

            if #allow_value {
                for #child_ident in node.children_named(#kdl_key) {
                    if field_config.bool_mode == ::kdl_config_runtime::BoolMode::PresenceOnly {
                        if #child_ident.args().is_empty() && #child_ident.children().is_empty() && #child_ident.attrs().is_empty() {
                            let v: #value_ty = ::kdl_config_runtime::convert_value_checked(&::kdl_config_runtime::Value::Bool(true), #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                            ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                        } else {
                            return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                                #struct_name,
                                #field_name,
                                "explicit values are not allowed in presence-only mode",
                            ));
                        }
                    } else {
                        if #child_ident.args().is_empty() {
                            if #is_bool && field_config.bool_mode != ::kdl_config_runtime::BoolMode::ValueOnly && #child_ident.children().is_empty() && #child_ident.attrs().is_empty() {
                                let v: #value_ty = ::kdl_config_runtime::convert_value_checked(&::kdl_config_runtime::Value::Bool(true), #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                                ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                            } else {
                                return Err(::kdl_config_runtime::KdlConfigError::missing_required(
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config_runtime::Placement::Value,
                                ));
                            }
                        } else if #child_ident.args().len() == 1 {
                            let #child_arg_ident = &#child_ident.args()[0];
                            let v = ::kdl_config_runtime::convert_value_checked::<#value_ty>(#child_arg_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                            ::kdl_config_runtime::helpers::resolve_scalar(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                        } else {
                            return Err(::kdl_config_runtime::KdlConfigError::too_many(
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config_runtime::Placement::Value,
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
        quote! { ::kdl_config_runtime::Placement::Value },
    );
    let finalize_value = if is_option_vec {
        quote! { Some(val) }
    } else {
        quote! { val }
    };

    quote! {
        #mark_usage
        let field_config = ::kdl_config_runtime::resolve_field(&struct_config, #field_overrides);
        let mut #field_ident: ::core::option::Option<Vec<#elem_ty>> = None;

        let pos_index = #pos_index_expr;
        let has_placement = #allow_attr_keyed || #allow_value || pos_index.is_some();

        if !has_placement {
            match field_config.default_placement {
                ::kdl_config_runtime::DefaultPlacement::Exhaustive => {
                    if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                        for #attr_value_ident in #attr_values_ident {
                            let v = ::kdl_config_runtime::convert_value_checked::<Vec<#elem_ty>>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrKeyed)?;
                            ::kdl_config_runtime::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrKeyed)?;
                        }
                    }
                    if let Some(idx) = pos_index {
                        if let Some(#arg_value_ident) = node.arg(idx) {
                            let v = ::kdl_config_runtime::convert_value_checked::<Vec<#elem_ty>>(#arg_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrPositional)?;
                            ::kdl_config_runtime::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrPositional)?;
                            if struct_config.deny_unknown {
                                used_keys.mark_arg(idx);
                            }
                        }
                    }
                    for #child_ident in node.children_named(#kdl_key) {
                        if #child_ident.args().is_empty() {
                            return Err(::kdl_config_runtime::KdlConfigError::missing_required(
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config_runtime::Placement::Value,
                            ));
                        }
                        let mut #values_ident = Vec::with_capacity(#child_ident.args().len());
                        for #child_arg_ident in #child_ident.args() {
                            #values_ident.push(::kdl_config_runtime::convert_value_checked::<#elem_ty>(#child_arg_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?);
                        }
                        ::kdl_config_runtime::helpers::resolve_vec(field_config.conflict, &mut #field_ident, #values_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                    }
                }
                ::kdl_config_runtime::DefaultPlacement::Attr => {
                    if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                        for #attr_value_ident in #attr_values_ident {
                            let v = ::kdl_config_runtime::convert_value_checked::<Vec<#elem_ty>>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrKeyed)?;
                            ::kdl_config_runtime::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrKeyed)?;
                        }
                    }
                }
                ::kdl_config_runtime::DefaultPlacement::Value => {
                    for #child_ident in node.children_named(#kdl_key) {
                        if #child_ident.args().is_empty() {
                            return Err(::kdl_config_runtime::KdlConfigError::missing_required(
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config_runtime::Placement::Value,
                            ));
                        }
                        let mut #values_ident = Vec::with_capacity(#child_ident.args().len());
                        for #child_arg_ident in #child_ident.args() {
                            #values_ident.push(::kdl_config_runtime::convert_value_checked::<#elem_ty>(#child_arg_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?);
                        }
                        ::kdl_config_runtime::helpers::resolve_vec(field_config.conflict, &mut #field_ident, #values_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
                    }
                }
                ::kdl_config_runtime::DefaultPlacement::Child => {
                    return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "child placement is incompatible with scalar Vec values",
                    ));
                }
            }
        } else {
            if !#allow_attr_keyed {
                if node.attr_values(#kdl_key).is_some() {
                    return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "keyed attribute placement is not allowed for this field",
                    ));
                }
            }

            if !#allow_value {
                if node.children_named(#kdl_key).next().is_some() {
                    return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "value placement is not allowed for this field",
                    ));
                }
            }

            if #allow_attr_keyed {
                if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                    for #attr_value_ident in #attr_values_ident {
                        let v = ::kdl_config_runtime::convert_value_checked::<Vec<#elem_ty>>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrKeyed)?;
                        ::kdl_config_runtime::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrKeyed)?;
                    }
                }
            }
            if let Some(idx) = pos_index {
                if let Some(#arg_value_ident) = node.arg(idx) {
                    let v = ::kdl_config_runtime::convert_value_checked::<Vec<#elem_ty>>(#arg_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrPositional)?;
                    ::kdl_config_runtime::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::AttrPositional)?;
                    if struct_config.deny_unknown {
                        used_keys.mark_arg(idx);
                    }
                }
            }
            if #allow_value {
                for #child_ident in node.children_named(#kdl_key) {
                    if #child_ident.args().is_empty() {
                        return Err(::kdl_config_runtime::KdlConfigError::missing_required(
                            #struct_name,
                            #field_name,
                            #kdl_key,
                            ::kdl_config_runtime::Placement::Value,
                        ));
                    }
                    let mut #values_ident = Vec::with_capacity(#child_ident.args().len());
                    for #child_arg_ident in #child_ident.args() {
                        #values_ident.push(::kdl_config_runtime::convert_value_checked::<#elem_ty>(#child_arg_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?);
                    }
                    ::kdl_config_runtime::helpers::resolve_vec(field_config.conflict, &mut #field_ident, #values_ident, #struct_name, #field_name, #kdl_key, ::kdl_config_runtime::Placement::Value)?;
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

    let default_expr = generate_missing_expr(
        field,
        value_ty,
        struct_name,
        &field_name,
        kdl_key,
        false,
        quote! { ::kdl_config_runtime::Placement::Child },
    );
    let finalize_value = if is_optional {
        quote! { Some(val) }
    } else {
        quote! { val }
    };

    let allow_child = field.placement.child || field.placement.children || field.placement.value;

    quote! {
        #mark_usage
        let field_config = ::kdl_config_runtime::resolve_field(&struct_config, #field_overrides);
        let mut #field_ident: ::core::option::Option<#value_ty> = None;

        let has_placement = #allow_child;

        if !has_placement {
            match field_config.default_placement {
                ::kdl_config_runtime::DefaultPlacement::Exhaustive | ::kdl_config_runtime::DefaultPlacement::Child => {
                    for #child_ident in node.children_named(#kdl_key) {
                        let v = <#value_ty as ::kdl_config_runtime::KdlParse>::from_node(#child_ident, config)?;
                        ::kdl_config_runtime::helpers::resolve_scalar(
                            field_config.conflict,
                            &mut #field_ident,
                            v,
                            #struct_name,
                            #field_name,
                            #kdl_key,
                            ::kdl_config_runtime::Placement::Child,
                        )?;
                    }
                }
                ::kdl_config_runtime::DefaultPlacement::Attr | ::kdl_config_runtime::DefaultPlacement::Value => {
                    return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "attr/value placement is incompatible with nested node types",
                    ));
                }
            }
        } else {
            for #child_ident in node.children_named(#kdl_key) {
                let v = <#value_ty as ::kdl_config_runtime::KdlParse>::from_node(#child_ident, config)?;
                ::kdl_config_runtime::helpers::resolve_scalar(
                    field_config.conflict,
                    &mut #field_ident,
                    v,
                    #struct_name,
                    #field_name,
                    #kdl_key,
                    ::kdl_config_runtime::Placement::Child,
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
    let allow_child = field.placement.child || field.placement.children;

    let default_expr = generate_missing_expr(
        field,
        inner_vec_ty,
        struct_name,
        &field_name,
        kdl_key,
        true,
        quote! { ::kdl_config_runtime::Placement::Children },
    );
    let finalize_value = if is_option_vec {
        quote! { Some(val) }
    } else {
        quote! { val }
    };

    quote! {
        #mark_usage
        let field_config = ::kdl_config_runtime::resolve_field(&struct_config, #field_overrides);
        let mut #field_ident: ::core::option::Option<Vec<#elem_ty>> = None;

        let has_placement = #allow_child;

        if !has_placement {
            match field_config.default_placement {
                ::kdl_config_runtime::DefaultPlacement::Exhaustive | ::kdl_config_runtime::DefaultPlacement::Child => {
                    let #children_ident = node.children_named(#kdl_key).collect::<Vec<_>>();
                    if !#children_ident.is_empty() {
                        let mut #values_ident = Vec::with_capacity(#children_ident.len());
                        for #child_ident in #children_ident {
                            #values_ident.push(<#elem_ty as ::kdl_config_runtime::KdlParse>::from_node(#child_ident, config)?);
                        }
                        #field_ident = Some(#values_ident);
                    }
                }
                ::kdl_config_runtime::DefaultPlacement::Attr | ::kdl_config_runtime::DefaultPlacement::Value => {
                    return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "attr/value placement is incompatible with nested node Vec",
                    ));
                }
            }
        } else {
            let #children_ident = node.children_named(#kdl_key).collect::<Vec<_>>();
            if !#children_ident.is_empty() {
                let mut #values_ident = Vec::with_capacity(#children_ident.len());
                for #child_ident in #children_ident {
                    #values_ident.push(<#elem_ty as ::kdl_config_runtime::KdlParse>::from_node(#child_ident, config)?);
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
                    ::kdl_config_runtime::KdlConfigError::invalid_registry_key(
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        "missing registry key",
                    )
                })?;
                let #key_ident = #key_val_ident.as_str().ok_or_else(|| {
                    ::kdl_config_runtime::KdlConfigError::invalid_registry_key(
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
                    ::kdl_config_runtime::KdlConfigError::invalid_registry_key(
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        "missing registry key",
                    )
                })?;
                if #key_val_ident.len() != 1 {
                    return Err(::kdl_config_runtime::KdlConfigError::invalid_registry_key(
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        "registry key attribute must have a single value",
                    ));
                }
                let #key_ident = #key_val_ident[0].as_str().ok_or_else(|| {
                    ::kdl_config_runtime::KdlConfigError::invalid_registry_key(
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
            let field_config = ::kdl_config_runtime::resolve_field(&struct_config, #field_overrides);
            let mut #field_ident: #ty = ::std::collections::HashMap::new();

            for #child_ident in node.children_named(#container) {
                #key_extract
                let #value_ident = <#val_ty as ::kdl_config_runtime::KdlParse>::from_node(&#node_copy_ident, config)?;
                match field_config.conflict {
                    ::kdl_config_runtime::ConflictPolicy::Error => {
                        if #field_ident.contains_key(&#key_ident) {
                            return Err(::kdl_config_runtime::KdlConfigError::invalid_registry_key(
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                "duplicate registry key",
                            ));
                        }
                        #field_ident.insert(#key_ident.clone(), #value_ident);
                    }
                    ::kdl_config_runtime::ConflictPolicy::First => {
                        #field_ident.entry(#key_ident.clone()).or_insert(#value_ident);
                    }
                    ::kdl_config_runtime::ConflictPolicy::Last => {
                        #field_ident.insert(#key_ident.clone(), #value_ident);
                    }
                    ::kdl_config_runtime::ConflictPolicy::Append => {
                        return Err(::kdl_config_runtime::KdlConfigError::incompatible_placement(
                            #struct_name,
                            #field_name,
                            "append conflict policy is not supported for HashMap registry fields",
                        ));
                    }
                }
            }

            if #required && #field_ident.is_empty() {
                return Err(::kdl_config_runtime::KdlConfigError::missing_required(
                    #struct_name,
                    #field_name,
                    #container,
                    ::kdl_config_runtime::Placement::Registry,
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
        let field_config = ::kdl_config_runtime::resolve_field(&struct_config, #field_overrides);
        let mut #field_ident: Vec<(String, #val_ty)> = Vec::new();

        for #child_ident in node.children_named(#container) {
            #key_extract
            let #value_ident = <#val_ty as ::kdl_config_runtime::KdlParse>::from_node(&#node_copy_ident, config)?;
            match field_config.conflict {
                ::kdl_config_runtime::ConflictPolicy::Error => {
                    if #field_ident.iter().any(|(key, _)| key == &#key_ident) {
                        return Err(::kdl_config_runtime::KdlConfigError::invalid_registry_key(
                            #struct_name,
                            #field_name,
                            #kdl_key,
                            "duplicate registry key",
                        ));
                    }
                    #field_ident.push((#key_ident.clone(), #value_ident));
                }
                ::kdl_config_runtime::ConflictPolicy::First => {
                    if !#field_ident.iter().any(|(key, _)| key == &#key_ident) {
                        #field_ident.push((#key_ident.clone(), #value_ident));
                    }
                }
                ::kdl_config_runtime::ConflictPolicy::Last => {
                    if let Some(#existing_idx_ident) = #field_ident.iter().position(|(key, _)| key == &#key_ident) {
                        #field_ident.remove(#existing_idx_ident);
                    }
                    #field_ident.push((#key_ident.clone(), #value_ident));
                }
                ::kdl_config_runtime::ConflictPolicy::Append => {
                    #field_ident.push((#key_ident.clone(), #value_ident));
                }
            }
        }

        if #required && #field_ident.is_empty() {
            return Err(::kdl_config_runtime::KdlConfigError::missing_required(
                #struct_name,
                #field_name,
                #container,
                ::kdl_config_runtime::Placement::Registry,
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
                return Err(::kdl_config_runtime::KdlConfigError::missing_required(
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
            return Err(::kdl_config_runtime::KdlConfigError::missing_required(
                #struct_name,
                #field_name,
                #kdl_key,
                ::kdl_config_runtime::Placement::Unknown,
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
                    ::kdl_config_runtime::FromKdlValue::from_value(
                        &::kdl_config_runtime::Value::Int(#n)
                    ).expect(concat!("invalid default value for ", stringify!(#ty)))
                }
            }
        }
        DefaultLiteral::Float(f) => {
            if is_numeric_type(ty) {
                quote! { #f as #ty }
            } else {
                quote! {
                    ::kdl_config_runtime::FromKdlValue::from_value(
                        &::kdl_config_runtime::Value::Float(#f)
                    ).expect(concat!("invalid default value for ", stringify!(#ty)))
                }
            }
        }
        DefaultLiteral::Bool(b) => {
            if is_bool_type(ty) {
                quote! { #b }
            } else {
                quote! {
                    ::kdl_config_runtime::FromKdlValue::from_value(
                        &::kdl_config_runtime::Value::Bool(#b)
                    ).expect(concat!("invalid default value for ", stringify!(#ty)))
                }
            }
        }
        DefaultLiteral::String(s) => {
            if is_string_type(ty) {
                quote! { ::std::string::String::from(#s) }
            } else {
                quote! {
                    ::kdl_config_runtime::FromKdlValue::from_value(
                        &::kdl_config_runtime::Value::String(::std::string::String::from(#s))
                    ).expect(concat!("invalid default value for ", stringify!(#ty)))
                }
            }
        }
    }
}
