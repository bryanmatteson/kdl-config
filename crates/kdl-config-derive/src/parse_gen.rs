use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::attrs::{
    BoolMode, ChildrenMapKind, CollectionMode, ConflictPolicy, DefaultLiteral, DefaultPlacement,
    DefaultSpec, FieldInfo, FieldKind, FlagStyle, InjectOpt, SelectorAst, StructAttrs,
    ValidationRule,
    extract_children_map_types, extract_hashmap_types, extract_inner_type, field_kind,
    is_bool_type, is_numeric_type, is_string_type,
};

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
            if !ctx.allow_any_name && node.name_str() != #node_name {
                return Err(::kdl_config::KdlConfigError::node_name_mismatch(
                    #struct_name_str,
                    #node_name,
                    node.name_str(),
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
        .map(|field| generate_skip_marks(field, &struct_name_str))
        .collect();

    let field_names: Vec<&Ident> = fields.iter().map(|f| &f.ident).collect();

    let valid_attr_keys: Vec<&str> = fields
        .iter()
        .filter(|f| !f.is_skipped)
        .filter(|f| matches!(field_kind(f), FieldKind::ValueScalar | FieldKind::ValueVec))
        .map(|f| f.kdl_key.as_str())
        .collect();
    let valid_child_names: Vec<&str> = fields
        .iter()
        .filter(|f| !f.is_skipped)
        .filter(|f| {
            matches!(
                field_kind(f),
                FieldKind::Node | FieldKind::NodeVec | FieldKind::Flatten | FieldKind::Collection
            )
        })
        .map(|f| f.kdl_key.as_str())
        .collect();

    let cross_field_validations = generate_cross_field_validations(fields, &struct_name_str);

    quote! {
        impl ::kdl_config::KdlDecode for #struct_name {
            fn decode(node: &::kdl_config::KdlNode, ctx: &::kdl_config::DecodeContext) -> ::core::result::Result<Self, ::kdl_config::KdlConfigError> {
                let result = (|| {
                    use ::kdl_config::KdlNodeExt as _;
                    #validate_name
                    let struct_overrides = #struct_overrides;
                    let struct_config = ::kdl_config::resolve_struct(ctx.config, struct_overrides);
                    let mut claims = ::kdl_config::helpers::Claims::new();

                    #(#field_parsers)*
                    #(#skip_marks)*

                    if struct_config.deny_unknown {
                        claims.check_unknowns(node, #struct_name_str, &[#(#valid_attr_keys),*], &[#(#valid_child_names),*])?;
                    }

                    #cross_field_validations

                    Ok(Self {
                        #(#field_names,)*
                        #(#skipped_fields: ::std::default::Default::default(),)*
                    })
                })();

                result.map_err(|err| {
                    err.with_context(ctx.source, ctx.path.as_ref(), Some(node.span().offset()))
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
    if field.is_hashmap && field.collection.is_none() {
        return quote! { compile_error!("HashMap fields require #[kdl(registry)] or #[kdl(children_map)]"); };
    }
    if crate::attrs::extract_registry_vec_value(&field.ty).is_some() && field.collection.is_none() {
        return quote! { compile_error!("Vec<(String, T)> fields require #[kdl(registry)] or #[kdl(children_map)]"); };
    }

    let field_overrides = generate_field_overrides(field);
    let mark_usage = generate_usage_marks(field);

    let kind = field_kind(field);

    if field
        .select
        .as_ref()
        .and_then(|spec| spec.opts.inject.as_ref())
        .is_some()
    {
        match kind {
            FieldKind::Node | FieldKind::NodeVec | FieldKind::Collection => {}
            _ => {
                return quote! { compile_error!("inject is only supported for child/children/registry/children_map fields"); };
            }
        }
    }

    let parser = match kind {
        FieldKind::Collection => {
            let collection = field
                .collection
                .as_ref()
                .expect("collection fields must have a spec");
            generate_collection_parser(field, struct_name, &field_overrides, mark_usage, collection)
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
    };

    let validation = generate_field_validation(field, struct_name, &kind);
    quote! {
        #parser
        #validation
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

fn path_claims_shadow(field: &FieldInfo) -> TokenStream {
    if field.path.is_some() {
        quote! { let mut claims = ::kdl_config::helpers::Claims::new(); }
    } else {
        quote! {}
    }
}

fn path_parent_claims(field: &FieldInfo) -> TokenStream {
    let path = match field.path.as_ref() {
        Some(path) => path,
        None => return quote! {},
    };
    let first = path
        .segments
        .first()
        .expect("path requires at least one segment");
    let claim_block = if path.absolute {
        quote! {
            if let Some(root) = ctx.root {
                if ::core::ptr::eq(root, node) {
                    for (idx, child) in node.iter_children().enumerate() {
                        if child.name_str() == #first {
                            claims.claim_child(idx);
                        }
                    }
                }
            }
        }
    } else {
        quote! {
            for (idx, child) in node.iter_children().enumerate() {
                if child.name_str() == #first {
                    claims.claim_child(idx);
                }
            }
        }
    };

    quote! {
        if struct_config.deny_unknown {
            #claim_block
        }
    }
}

fn wrap_path_scan(
    field: &FieldInfo,
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
    scan_block: TokenStream,
) -> TokenStream {
    let path = match field.path.as_ref() {
        Some(path) => path,
        None => return scan_block,
    };

    let path_candidates_ident = format_ident!("__kdl_path_candidates_{}", field.ident);
    let path_candidate_ident = format_ident!("__kdl_path_candidate_{}", field.ident);
    let path_ctx_ident = format_ident!("__kdl_path_ctx_{}", field.ident);
    let path_segments: Vec<TokenStream> = path
        .segments
        .iter()
        .map(|segment| quote! { #segment })
        .collect();
    let path_segments_expr = quote! { &[#(#path_segments),*] };
    let path_raw = &path.raw;

    let resolve_expr = if path.absolute {
        quote! {
            if let (Some(root), Some(root_path)) = (ctx.root, ctx.root_path.as_ref()) {
                ::kdl_config::helpers::resolve_path_candidates(root, Some(root_path), #path_segments_expr)
            } else {
                return Err(::kdl_config::KdlConfigError {
                    struct_name: #struct_name.to_string(),
                    field_name: Some(#field_name.to_string()),
                    kdl_key: Some(#kdl_key.to_string()),
                    placement: ::kdl_config::Placement::Unknown,
                    required: false,
                    kind: ::kdl_config::ErrorKind::Custom(format!(
                        "absolute path '{}' requires root context",
                        #path_raw
                    )),
                    node_path: None,
                    location: None,
                });
            }
        }
    } else {
        quote! {
            ::kdl_config::helpers::resolve_path_candidates(node, ctx.path.as_ref(), #path_segments_expr)
        }
    };

    quote! {
        let #path_candidates_ident = { #resolve_expr };
        for #path_candidate_ident in #path_candidates_ident {
            let node = #path_candidate_ident.node;
            let #path_ctx_ident = if let Some(ref path) = #path_candidate_ident.path {
                ctx.with_path(path.clone())
            } else {
                ctx.clone()
            };
            let ctx = &#path_ctx_ident;
            let __kdl_scan_result: ::core::result::Result<(), ::kdl_config::KdlConfigError> = (|| {
                #scan_block
                Ok(())
            })();
            if let Err(err) = __kdl_scan_result {
                return Err(err.with_context(ctx.source, ctx.path.as_ref(), Some(node.span().offset())));
            }
        }
    }
}

fn generate_usage_marks(field: &FieldInfo) -> TokenStream {
    let _ = field;
    quote! {}
}

pub(crate) fn generate_skip_marks(field: &FieldInfo, struct_name: &str) -> TokenStream {
    if field.is_modifier {
        return quote! {};
    }

    if let Some(path) = field.path.as_ref() {
        let _ = path;
        return path_parent_claims(field);
    }

    let key = field.kdl_key.clone();
    let field_name = field.ident.to_string();
    let field_overrides = generate_field_overrides(field);

    if let Some(collection) = field.collection.as_ref() {
        let claims = match &collection.mode {
            CollectionMode::Registry { container } => {
                quote! {
                    for (idx, child) in node.iter_children().enumerate() {
                        if child.name_str() == #container {
                            claims.claim_child(idx);
                        }
                    }
                }
            }
            CollectionMode::ChildrenMapNode { node } => {
                quote! {
                    for (idx, child) in node.iter_children().enumerate() {
                        if child.name_str() == #node {
                            claims.claim_child(idx);
                        }
                    }
                }
            }
            CollectionMode::ChildrenMapAll => {
                quote! {
                    for (idx, _child) in node.iter_children().enumerate() {
                        claims.claim_child(idx);
                    }
                }
            }
        };
        return quote! {
            if struct_config.deny_unknown {
                #claims
            }
        };
    }

    let mut marks = Vec::new();
    let has_explicit = field.placement.attr
        || field.placement.keyed
        || field.placement.positional.is_some()
        || field.placement.positional_list
        || field.placement.flag.is_some()
        || field.placement.value
        || field.placement.child
        || field.placement.children
        || field.placement.children_any;

    if has_explicit {
        if field.placement.children_any {
            marks.push(quote! {
                for (idx, _child) in node.iter_children().enumerate() {
                    claims.claim_child(idx);
                }
            });
        }
        if field.placement.attr || field.placement.keyed {
            marks.push(quote! { claims.claim_attr(#key); });
        }
        if let Some(idx) = field.placement.positional {
            marks.push(quote! { claims.claim_arg(#idx); });
        }
        if field.placement.positional_list {
            marks.push(quote! {
                for idx in 0..node.args().len() {
                    claims.claim_arg(idx);
                }
            });
        }
        if field.placement.value || field.placement.child || field.placement.children {
            marks.push(quote! {
                for (idx, child) in node.iter_children().enumerate() {
                    if child.name_str() == #key {
                        claims.claim_child(idx);
                    }
                }
            });
        }
    } else {
        match field_kind(field) {
            FieldKind::ValueScalar | FieldKind::ValueVec => {
                marks.push(quote! {
                    match struct_config.default_placement {
                        ::kdl_config::DefaultPlacement::Exhaustive => {
                            claims.claim_attr(#key);
                            for (idx, child) in node.iter_children().enumerate() {
                                if child.name_str() == #key {
                                    claims.claim_child(idx);
                                }
                            }
                        }
                        ::kdl_config::DefaultPlacement::Attr => {
                            claims.claim_attr(#key);
                        }
                        ::kdl_config::DefaultPlacement::Value | ::kdl_config::DefaultPlacement::Child => {
                            for (idx, child) in node.iter_children().enumerate() {
                                if child.name_str() == #key {
                                    claims.claim_child(idx);
                                }
                            }
                        }
                    }
                });
            }
            FieldKind::Node | FieldKind::NodeVec => {
                marks.push(quote! {
                    match struct_config.default_placement {
                        ::kdl_config::DefaultPlacement::Exhaustive | ::kdl_config::DefaultPlacement::Child => {
                            for (idx, child) in node.iter_children().enumerate() {
                                if child.name_str() == #key {
                                    claims.claim_child(idx);
                                }
                            }
                        }
                        ::kdl_config::DefaultPlacement::Attr | ::kdl_config::DefaultPlacement::Value => {}
                    }
                });
            }
            FieldKind::Flatten => {
                marks.push(quote! {
                    for key in node.attrs().keys() {
                        claims.claim_attr(key);
                    }
                    for (idx, _) in node.args().iter().enumerate() {
                        claims.claim_arg(idx);
                    }
                    for (idx, _child) in node.iter_children().enumerate() {
                        claims.claim_child(idx);
                    }
                });
            }
            FieldKind::Collection => {}
            FieldKind::Modifier => {}
        }
    }

    if field.is_bool {
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
        let allow_flags =
            field.placement.flag.is_some() || (field.placement.attr && !field.placement.keyed);
        let has_explicit_expr = has_explicit;
        marks.push(quote! {
            let field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
            let custom_pos = #custom_pos_expr;
            let custom_neg = #custom_neg_expr;
            let flags_allowed = if #has_explicit_expr {
                #allow_flags
            } else {
                matches!(field_config.default_placement, ::kdl_config::DefaultPlacement::Exhaustive | ::kdl_config::DefaultPlacement::Attr)
            };
            if flags_allowed && field_config.bool_mode != ::kdl_config::BoolMode::ValueOnly {
                if let Some((_flag_val, flag_indices)) = ::kdl_config::helpers::scan_flag_with_style(
                    node,
                    #key,
                    field_config.flag_style,
                    custom_pos,
                    custom_neg,
                    #struct_name,
                    #field_name,
                )? {
                    for idx in flag_indices {
                        claims.claim_arg(idx);
                    }
                }
            }
        });
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

fn generate_modifier_parser(field: &FieldInfo) -> TokenStream {
    let field_ident = &field.ident;
    let ty = &field.ty;

    if field.is_optional {
        quote! {
            let #field_ident: #ty = if node.modifier() == ::kdl_config::Modifier::Inherit {
                None
            } else {
                Some(node.modifier())
            };
        }
    } else {
        quote! {
            let #field_ident: #ty = node.modifier();
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
    let source_ident = format_ident!("__KdlScalarSource_{}", snake_to_upper_camel(field_ident));
    let candidates_ident = format_ident!("__kdl_scalar_candidates_{}", field_ident);

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

    let parent_claims = path_parent_claims(field);
    let claims_shadow = path_claims_shadow(field);
    let scan_block = quote! {
        let flags_allowed = if has_placement {
            #allow_flags
        } else {
            matches!(field_config.default_placement, ::kdl_config::DefaultPlacement::Exhaustive | ::kdl_config::DefaultPlacement::Attr)
        };

        if has_placement && !#allow_attr_keyed {
            if node.attr_values(#kdl_key).is_some() {
                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                    #struct_name,
                    #field_name,
                    "keyed attributes are not allowed for this field",
                ));
            }
        }

        if #is_bool && has_placement && !#allow_flags {
            if let Some((_flag_val, _flag_indices)) = ::kdl_config::helpers::scan_flag_with_style(
                node,
                #kdl_key,
                field_config.flag_style,
                custom_pos.clone(),
                custom_neg.clone(),
                #struct_name,
                #field_name,
            )? {
                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                    #struct_name,
                    #field_name,
                    "flag placement is not allowed for this field",
                ));
            }
        }

        if #is_bool && field_config.bool_mode == ::kdl_config::BoolMode::ValueOnly && flags_allowed {
            if let Some((_flag_val, _flag_indices)) = ::kdl_config::helpers::scan_flag_with_style(
                node,
                #kdl_key,
                field_config.flag_style,
                custom_pos.clone(),
                custom_neg.clone(),
                #struct_name,
                #field_name,
            )? {
                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                    #struct_name,
                    #field_name,
                    "flag placement is not allowed for value-only bools",
                ));
            }
        }

        if !has_placement {
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
                            let v = ::kdl_config::convert_value_checked::<#value_ty>(
                                #attr_value_ident,
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config::Placement::AttrKeyed,
                            )?;
                            #candidates_ident.push((v, #source_ident::AttrKeyed));
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
                            let v = ::kdl_config::convert_value_checked::<#value_ty>(
                                #arg_value_ident,
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config::Placement::AttrPositional,
                            )?;
                            #candidates_ident.push((v, #source_ident::Arg(idx)));
                        }
                    }

                    if #is_bool && field_config.bool_mode != ::kdl_config::BoolMode::ValueOnly {
                        if let Some((flag_val, flag_indices)) = ::kdl_config::helpers::scan_flag_with_style(
                            node,
                            #kdl_key,
                            field_config.flag_style,
                            custom_pos.clone(),
                            custom_neg.clone(),
                            #struct_name,
                            #field_name,
                        )? {
                            if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly && flag_val == false {
                                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "negative flags are not allowed in presence-only mode",
                                ));
                            }
                            let v: #value_ty = ::kdl_config::convert_value_checked(
                                &::kdl_config::KdlValue::Bool(flag_val),
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config::Placement::AttrFlag,
                            )?;
                            #candidates_ident.push((v, #source_ident::Flag(flag_indices)));
                        }
                    }

                    for (child_index, #child_ident) in node.iter_children().enumerate() {
                        if #child_ident.name_str() != #kdl_key {
                            continue;
                        }
                        if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                            if #child_ident.args().is_empty()
                                && #child_ident.iter_children().next().is_none()
                                && #child_ident.attrs().is_empty()
                            {
                                let v: #value_ty = ::kdl_config::convert_value_checked(
                                    &::kdl_config::KdlValue::Bool(true),
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config::Placement::Value,
                                )?;
                                #candidates_ident.push((v, #source_ident::Child(child_index)));
                            } else {
                                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "explicit values are not allowed in presence-only mode",
                                ));
                            }
                        } else if #child_ident.args().is_empty() {
                            if #is_bool
                                && field_config.bool_mode != ::kdl_config::BoolMode::ValueOnly
                                && #child_ident.iter_children().next().is_none()
                                && #child_ident.attrs().is_empty()
                            {
                                let v: #value_ty = ::kdl_config::convert_value_checked(
                                    &::kdl_config::KdlValue::Bool(true),
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config::Placement::Value,
                                )?;
                                #candidates_ident.push((v, #source_ident::Child(child_index)));
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
                            let v = ::kdl_config::convert_value_checked::<#value_ty>(
                                #child_arg_ident,
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config::Placement::Value,
                            )?;
                            #candidates_ident.push((v, #source_ident::Child(child_index)));
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
                            let v = ::kdl_config::convert_value_checked::<#value_ty>(
                                #attr_value_ident,
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config::Placement::AttrKeyed,
                            )?;
                            #candidates_ident.push((v, #source_ident::AttrKeyed));
                        }
                    }

                    if #is_bool && field_config.bool_mode != ::kdl_config::BoolMode::ValueOnly {
                        if let Some((flag_val, flag_indices)) = ::kdl_config::helpers::scan_flag_with_style(
                            node,
                            #kdl_key,
                            field_config.flag_style,
                            custom_pos.clone(),
                            custom_neg.clone(),
                            #struct_name,
                            #field_name,
                        )? {
                            if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly && flag_val == false {
                                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "negative flags are not allowed in presence-only mode",
                                ));
                            }
                            let v: #value_ty = ::kdl_config::convert_value_checked(
                                &::kdl_config::KdlValue::Bool(flag_val),
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config::Placement::AttrFlag,
                            )?;
                            #candidates_ident.push((v, #source_ident::Flag(flag_indices)));
                        }
                    }
                }
                ::kdl_config::DefaultPlacement::Value => {
                    for (child_index, #child_ident) in node.iter_children().enumerate() {
                        if #child_ident.name_str() != #kdl_key {
                            continue;
                        }
                        if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                            if #child_ident.args().is_empty()
                                && #child_ident.iter_children().next().is_none()
                                && #child_ident.attrs().is_empty()
                            {
                                let v: #value_ty = ::kdl_config::convert_value_checked(
                                    &::kdl_config::KdlValue::Bool(true),
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config::Placement::Value,
                                )?;
                                #candidates_ident.push((v, #source_ident::Child(child_index)));
                            } else {
                                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                    #struct_name,
                                    #field_name,
                                    "explicit values are not allowed in presence-only mode",
                                ));
                            }
                        } else if #child_ident.args().is_empty() {
                            if #is_bool
                                && field_config.bool_mode != ::kdl_config::BoolMode::ValueOnly
                                && #child_ident.iter_children().next().is_none()
                                && #child_ident.attrs().is_empty()
                            {
                                let v: #value_ty = ::kdl_config::convert_value_checked(
                                    &::kdl_config::KdlValue::Bool(true),
                                    #struct_name,
                                    #field_name,
                                    #kdl_key,
                                    ::kdl_config::Placement::Value,
                                )?;
                                #candidates_ident.push((v, #source_ident::Child(child_index)));
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
                            #candidates_ident.push((v, #source_ident::Child(child_index)));
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
                ::kdl_config::DefaultPlacement::Child => {}
            }
        } else {
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
                        let v = ::kdl_config::convert_value_checked::<#value_ty>(
                            #attr_value_ident,
                            #struct_name,
                            #field_name,
                            #kdl_key,
                            ::kdl_config::Placement::AttrKeyed,
                        )?;
                        #candidates_ident.push((v, #source_ident::AttrKeyed));
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
                    let v = ::kdl_config::convert_value_checked::<#value_ty>(
                        #arg_value_ident,
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        ::kdl_config::Placement::AttrPositional,
                    )?;
                    #candidates_ident.push((v, #source_ident::Arg(idx)));
                }
            }
            if #is_bool && #allow_flags && field_config.bool_mode != ::kdl_config::BoolMode::ValueOnly {
                if let Some((flag_val, flag_indices)) = ::kdl_config::helpers::scan_flag_with_style(
                    node,
                    #kdl_key,
                    field_config.flag_style,
                    custom_pos.clone(),
                    custom_neg.clone(),
                    #struct_name,
                    #field_name,
                )? {
                    if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly && flag_val == false {
                        return Err(::kdl_config::KdlConfigError::incompatible_placement(
                            #struct_name,
                            #field_name,
                            "negative flags are not allowed in presence-only mode",
                        ));
                    }
                    let v: #value_ty = ::kdl_config::convert_value_checked(
                        &::kdl_config::KdlValue::Bool(flag_val),
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        ::kdl_config::Placement::AttrFlag,
                    )?;
                    #candidates_ident.push((v, #source_ident::Flag(flag_indices)));
                }
            }

            if #allow_value {
                for (child_index, #child_ident) in node.iter_children().enumerate() {
                    if #child_ident.name_str() != #kdl_key {
                        continue;
                    }
                    if field_config.bool_mode == ::kdl_config::BoolMode::PresenceOnly {
                        if #child_ident.args().is_empty()
                            && #child_ident.iter_children().next().is_none()
                            && #child_ident.attrs().is_empty()
                        {
                            let v: #value_ty = ::kdl_config::convert_value_checked(
                                &::kdl_config::KdlValue::Bool(true),
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config::Placement::Value,
                            )?;
                            #candidates_ident.push((v, #source_ident::Child(child_index)));
                        } else {
                            return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                #struct_name,
                                #field_name,
                                "explicit values are not allowed in presence-only mode",
                            ));
                        }
                    } else if #child_ident.args().is_empty() {
                        if #is_bool
                            && field_config.bool_mode != ::kdl_config::BoolMode::ValueOnly
                            && #child_ident.iter_children().next().is_none()
                            && #child_ident.attrs().is_empty()
                        {
                            let v: #value_ty = ::kdl_config::convert_value_checked(
                                &::kdl_config::KdlValue::Bool(true),
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                ::kdl_config::Placement::Value,
                            )?;
                            #candidates_ident.push((v, #source_ident::Child(child_index)));
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
                        let v = ::kdl_config::convert_value_checked::<#value_ty>(
                            #child_arg_ident,
                            #struct_name,
                            #field_name,
                            #kdl_key,
                            ::kdl_config::Placement::Value,
                        )?;
                        #candidates_ident.push((v, #source_ident::Child(child_index)));
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
    };

    let scan_block = wrap_path_scan(field, struct_name, &field_name, kdl_key, scan_block);

    quote! {
        #mark_usage
        let #field_ident: #ty = {
            #parent_claims
            #claims_shadow
            let field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
            let custom_pos = #custom_pos_expr;
            let custom_neg = #custom_neg_expr;
            let pos_index = #pos_index_expr;
            let has_placement = #allow_attr_keyed || #allow_value || pos_index.is_some() || #allow_attr_positional_list || #allow_flags;

            enum #source_ident {
                AttrKeyed,
                Arg(usize),
                Flag(Vec<usize>),
                Child(usize),
            }

            let mut #candidates_ident: ::std::vec::Vec<(#value_ty, #source_ident)> = ::std::vec::Vec::new();

            #scan_block

            let selected = match field_config.conflict {
                ::kdl_config::ConflictPolicy::Error => {
                    if #candidates_ident.len() > 1 {
                        return Err(::kdl_config::KdlConfigError::ambiguous(
                            #struct_name,
                            #field_name,
                            #kdl_key,
                            ::kdl_config::Placement::Unknown,
                            "multiple candidates for scalar field",
                        ));
                    }
                    #candidates_ident.pop()
                }
                ::kdl_config::ConflictPolicy::First => {
                    if #candidates_ident.is_empty() {
                        None
                    } else {
                        Some(#candidates_ident.remove(0))
                    }
                }
                ::kdl_config::ConflictPolicy::Last => #candidates_ident.pop(),
                ::kdl_config::ConflictPolicy::Append => {
                    return Err(::kdl_config::KdlConfigError::incompatible_placement(
                        #struct_name,
                        #field_name,
                        "append conflict policy is not supported for scalar fields",
                    ));
                }
            };

            if let Some((val, source)) = selected {
                match source {
                    #source_ident::AttrKeyed => {
                        claims.claim_attr(#kdl_key);
                    }
                    #source_ident::Arg(idx) => {
                        claims.claim_arg(idx);
                    }
                    #source_ident::Flag(indices) => {
                        for idx in indices {
                            claims.claim_arg(idx);
                        }
                    }
                    #source_ident::Child(idx) => {
                        claims.claim_child(idx);
                    }
                }
                let val = val;
                #finalize_value
            } else {
                #default_expr
            }
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

    let parent_claims = path_parent_claims(field);
    let claims_shadow = path_claims_shadow(field);
    let scan_block = quote! {
        if !has_placement {
            match field_config.default_placement {
                ::kdl_config::DefaultPlacement::Exhaustive => {
                    if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                        for #attr_value_ident in #attr_values_ident {
                            let v = ::kdl_config::convert_value_checked::<Vec<#elem_ty>>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                            ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                        }
                        claims.claim_attr(#kdl_key);
                    }
                    if let Some(idx) = pos_index {
                        if let Some(#arg_value_ident) = node.arg(idx) {
                            let v = ::kdl_config::convert_value_checked::<Vec<#elem_ty>>(#arg_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                            ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                            claims.claim_arg(idx);
                        }
                    }
                    for (child_index, #child_ident) in node.iter_children().enumerate() {
                        if #child_ident.name_str() != #kdl_key {
                            continue;
                        }
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
                        claims.claim_child(child_index);
                    }
                }
                ::kdl_config::DefaultPlacement::Attr => {
                    if let Some(#attr_values_ident) = node.attr_values(#kdl_key) {
                        for #attr_value_ident in #attr_values_ident {
                            let v = ::kdl_config::convert_value_checked::<Vec<#elem_ty>>(#attr_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                            ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrKeyed)?;
                        }
                        claims.claim_attr(#kdl_key);
                    }
                }
                ::kdl_config::DefaultPlacement::Value => {
                    for (child_index, #child_ident) in node.iter_children().enumerate() {
                        if #child_ident.name_str() != #kdl_key {
                            continue;
                        }
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
                        claims.claim_child(child_index);
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
                    claims.claim_attr(#kdl_key);
                }
            }
            if let Some(idx) = pos_index {
                if let Some(#arg_value_ident) = node.arg(idx) {
                    let v = ::kdl_config::convert_value_checked::<Vec<#elem_ty>>(#arg_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                    ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, v, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                    claims.claim_arg(idx);
                }
            }
            if #allow_attr_positional_list {
                if !node.args().is_empty() {
                    let mut #values_ident = Vec::with_capacity(node.args().len());
                    for #arg_value_ident in node.args() {
                        #values_ident.push(::kdl_config::convert_value_checked::<#elem_ty>(#arg_value_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?);
                    }
                    ::kdl_config::helpers::resolve_vec(field_config.conflict, &mut #field_ident, #values_ident, #struct_name, #field_name, #kdl_key, ::kdl_config::Placement::AttrPositional)?;
                    for idx in 0..node.args().len() {
                        claims.claim_arg(idx);
                    }
                }
            }
            if #allow_value {
                for (child_index, #child_ident) in node.iter_children().enumerate() {
                    if #child_ident.name_str() != #kdl_key {
                        continue;
                    }
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
                    claims.claim_child(child_index);
                }
            }
        }
    };

    let scan_block = wrap_path_scan(field, struct_name, &field_name, kdl_key, scan_block);

    quote! {
        #mark_usage
        let #field_ident: #ty = {
            #parent_claims
            #claims_shadow
            let field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
            let mut #field_ident: ::core::option::Option<Vec<#elem_ty>> = None;

            let pos_index = #pos_index_expr;
            let has_placement = #allow_attr_keyed || #allow_value || pos_index.is_some() || #allow_attr_positional_list;

            #scan_block

            let #field_ident: #ty = if let Some(val) = #field_ident {
                #finalize_value
            } else {
                #default_expr
            };
            #field_ident
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
    let inject_node_ident = format_ident!("__kdl_inject_node_{}", field_ident);
    let inject_value_ident = format_ident!("__kdl_inject_value_{}", field_ident);
    let (inject_prep, decode_target) = if let Some(spec) = field.select.as_ref() {
        if let Some(inject) = spec.opts.inject.as_ref() {
            let inject_name = match inject {
                InjectOpt::Implicit => "name".to_string(),
                InjectOpt::Field(name) => name.clone(),
            };
            let consume = spec.opts.consume.unwrap_or(false);
            let inject_extract = selector_inject_extract(
                &spec.selector,
                struct_name,
                &field_name,
                &child_ident,
                &inject_value_ident,
                &inject_node_ident,
                consume,
            );
            let inject_attr = quote! {
                let mut entry = ::kdl_config::KdlEntry::new(#inject_value_ident);
                entry.set_name(Some(#inject_name));
                #inject_node_ident.entries_mut().push(entry);
            };
            (
                quote! { #inject_extract #inject_attr },
                quote! { &#inject_node_ident },
            )
        } else {
            (quote! {}, quote! { #child_ident })
        }
    } else {
        (quote! {}, quote! { #child_ident })
    };
    let explicit_child_loop = if field.placement.children_any {
        quote! {
            for (child_index, #child_ident) in node.iter_children().enumerate() {
                let child_ctx = ctx.with_child(#child_ident.name_str(), child_index);
                #inject_prep
                let v = <#value_ty as ::kdl_config::KdlDecode>::decode(#decode_target, &child_ctx)?;
                claims.claim_child(child_index);
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
    } else {
        quote! {
            for (child_index, #child_ident) in node.iter_children().enumerate() {
                if #child_ident.name_str() != #kdl_key {
                    continue;
                }
                let child_ctx = ctx.with_child(#child_ident.name_str(), child_index);
                #inject_prep
                let v = <#value_ty as ::kdl_config::KdlDecode>::decode(#decode_target, &child_ctx)?;
                claims.claim_child(child_index);
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

    let parent_claims = path_parent_claims(field);
    let claims_shadow = path_claims_shadow(field);
    let scan_block = quote! {
        if !has_placement {
            match field_config.default_placement {
                ::kdl_config::DefaultPlacement::Exhaustive | ::kdl_config::DefaultPlacement::Child => {
                    for (child_index, #child_ident) in node.iter_children().enumerate() {
                        let child_ctx = ctx.with_child(#child_ident.name_str(), child_index);
                        let parsed = <#value_ty as ::kdl_config::KdlDecode>::decode(#child_ident, &child_ctx)
                            .map_err(|err| err.with_context(child_ctx.source, child_ctx.path.as_ref(), Some(#child_ident.span().offset())));
                        let v = match parsed {
                            Ok(v) => v,
                            Err(err) => match err.kind {
                                ::kdl_config::ErrorKind::NodeNameMismatch { .. }
                                | ::kdl_config::ErrorKind::NoMatchingChoice { .. } => {
                                    continue;
                                }
                                _ => return Err(err),
                            },
                        };
                        claims.claim_child(child_index);
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
            #explicit_child_loop
        }
    };

    let scan_block = wrap_path_scan(field, struct_name, &field_name, kdl_key, scan_block);

    quote! {
        #mark_usage
        let #field_ident: #ty = {
            #parent_claims
            #claims_shadow
            let field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
            let mut #field_ident: ::core::option::Option<#value_ty> = None;

            let has_placement = #allow_child;

            #scan_block

            let #field_ident: #ty = if let Some(val) = #field_ident {
                #finalize_value
            } else {
                #default_expr
            };
            #field_ident
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
        let parsed = ::kdl_config::helpers::parse_flatten::<#value_ty>(node, ctx);
        if struct_config.deny_unknown {
            for key in node.attrs().keys() {
                claims.claim_attr(key);
            }
            for (idx, _) in node.args().iter().enumerate() {
                claims.claim_arg(idx);
            }
            for (idx, _child) in node.iter_children().enumerate() {
                claims.claim_child(idx);
            }
        }
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
    let child_ident = format_ident!("__kdl_child_{}", field_ident);
    let values_ident = format_ident!("__kdl_values_{}", field_ident);
    let inject_node_ident = format_ident!("__kdl_inject_node_{}", field_ident);
    let inject_value_ident = format_ident!("__kdl_inject_value_{}", field_ident);
    let (inject_prep, decode_target) = if let Some(spec) = field.select.as_ref() {
        if let Some(inject) = spec.opts.inject.as_ref() {
            let inject_name = match inject {
                InjectOpt::Implicit => "name".to_string(),
                InjectOpt::Field(name) => name.clone(),
            };
            let consume = spec.opts.consume.unwrap_or(false);
            let inject_extract = selector_inject_extract(
                &spec.selector,
                struct_name,
                &field_name,
                &child_ident,
                &inject_value_ident,
                &inject_node_ident,
                consume,
            );
            let inject_attr = quote! {
                let mut entry = ::kdl_config::KdlEntry::new(#inject_value_ident);
                entry.set_name(Some(#inject_name));
                #inject_node_ident.entries_mut().push(entry);
            };
            (
                quote! { #inject_extract #inject_attr },
                quote! { &#inject_node_ident },
            )
        } else {
            (quote! {}, quote! { #child_ident })
        }
    } else {
        (quote! {}, quote! { #child_ident })
    };
    let allow_child =
        field.placement.child || field.placement.children || field.placement.children_any;
    let children_any = field.placement.children_any;

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

    let parent_claims = path_parent_claims(field);
    let claims_shadow = path_claims_shadow(field);
    let scan_block = quote! {
        if !has_placement {
            match field_config.default_placement {
                ::kdl_config::DefaultPlacement::Exhaustive | ::kdl_config::DefaultPlacement::Child => {
                    let mut #values_ident: Vec<#elem_ty> = Vec::new();
                    for (child_index, #child_ident) in node.iter_children().enumerate() {
                        let child_ctx = ctx.with_child(#child_ident.name_str(), child_index);
                        #inject_prep
                        let parsed = <#elem_ty as ::kdl_config::KdlDecode>::decode(#decode_target, &child_ctx)
                            .map_err(|err| err.with_context(child_ctx.source, child_ctx.path.as_ref(), Some(#child_ident.span().offset())));
                        let v = match parsed {
                            Ok(v) => v,
                            Err(err) => match err.kind {
                                ::kdl_config::ErrorKind::NodeNameMismatch { .. }
                                | ::kdl_config::ErrorKind::NoMatchingChoice { .. } => {
                                    continue;
                                }
                                _ => return Err(err),
                            },
                        };
                        claims.claim_child(child_index);
                        #values_ident.push(v);
                    }
                    if !#values_ident.is_empty() {
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
            let mut #values_ident = Vec::new();
            for (child_index, #child_ident) in node.iter_children().enumerate() {
                if !#children_any && #child_ident.name_str() != #kdl_key {
                    continue;
                }
                let child_ctx = ctx.with_child(#child_ident.name_str(), child_index);
                #inject_prep
                let v = <#elem_ty as ::kdl_config::KdlDecode>::decode(#decode_target, &child_ctx)
                    .map_err(|err| err.with_context(child_ctx.source, child_ctx.path.as_ref(), Some(#child_ident.span().offset())))?;
                claims.claim_child(child_index);
                #values_ident.push(v);
            }
            if !#values_ident.is_empty() {
                #field_ident = Some(#values_ident);
            }
        }
    };

    let scan_block = wrap_path_scan(field, struct_name, &field_name, kdl_key, scan_block);

    quote! {
        #mark_usage
        let #field_ident: #ty = {
            #parent_claims
            #claims_shadow
            let field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
            let mut #field_ident: ::core::option::Option<Vec<#elem_ty>> = None;

            let has_placement = #allow_child;

            #scan_block

            let #field_ident: #ty = if let Some(val) = #field_ident {
                #finalize_value
            } else {
                #default_expr
            };
            #field_ident
        };
    }
}

fn selector_func_path(path: &str) -> TokenStream {
    path.parse().unwrap_or_else(|_| {
        let ident = syn::Ident::new(path, proc_macro2::Span::call_site());
        quote! { #ident }
    })
}

fn registry_key_extract(
    selector: &SelectorAst,
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
    child_ident: &Ident,
    key_val_ident: &Ident,
    key_ident: &Ident,
    node_copy_ident: &Ident,
    consume: bool,
) -> TokenStream {
    match selector {
        SelectorAst::Arg(index) => {
            let idx = *index as usize;
            let consume_expr = if consume {
                quote! { #child_ident.without_arg(#idx) }
            } else {
                quote! { #child_ident.clone() }
            };
            quote! {
                let #key_val_ident = #child_ident.arg(#idx).ok_or_else(|| {
                    ::kdl_config::KdlConfigError::invalid_registry_key(
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        "missing registry key",
                    )
                })?;
                let #key_ident = #key_val_ident.as_string().ok_or_else(|| {
                    ::kdl_config::KdlConfigError::invalid_registry_key(
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        "registry key must be a string",
                    )
                })?.to_string();
                let mut #node_copy_ident = #consume_expr;
            }
        }
        SelectorAst::Attr(name) => {
            let consume_expr = if consume {
                quote! { #child_ident.without_attr(#name) }
            } else {
                quote! { #child_ident.clone() }
            };
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
                let #key_ident = #key_val_ident[0].as_string().ok_or_else(|| {
                    ::kdl_config::KdlConfigError::invalid_registry_key(
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        "registry key must be a string",
                    )
                })?.to_string();
                let mut #node_copy_ident = #consume_expr;
            }
        }
        SelectorAst::Func(path) => {
            let key_fn = selector_func_path(path);
            quote! {
                let #key_ident = #key_fn(#child_ident)?;
                let mut #node_copy_ident = #child_ident.clone();
            }
        }
        SelectorAst::Any(selectors) => {
            let mut attempts = Vec::new();
            for selector in selectors {
                let attempt = match selector {
                    SelectorAst::Arg(index) => {
                        let idx = *index as usize;
                        let consume_expr = if consume {
                            quote! { #child_ident.without_arg(#idx) }
                        } else {
                            quote! { #child_ident.clone() }
                        };
                        quote! {
                            if #key_ident.is_none() {
                                if let Some(#key_val_ident) = #child_ident.arg(#idx) {
                                    let value = #key_val_ident.as_string().ok_or_else(|| {
                                        ::kdl_config::KdlConfigError::invalid_registry_key(
                                            #struct_name,
                                            #field_name,
                                            #kdl_key,
                                            "registry key must be a string",
                                        )
                                    })?.to_string();
                                    #key_ident = Some(value);
                                    #node_copy_ident = Some(#consume_expr);
                                }
                            }
                        }
                    }
                    SelectorAst::Attr(name) => {
                        let consume_expr = if consume {
                            quote! { #child_ident.without_attr(#name) }
                        } else {
                            quote! { #child_ident.clone() }
                        };
                        quote! {
                            if #key_ident.is_none() {
                                if let Some(#key_val_ident) = #child_ident.attr_values(#name) {
                                    if #key_val_ident.len() != 1 {
                                        return Err(::kdl_config::KdlConfigError::invalid_registry_key(
                                            #struct_name,
                                            #field_name,
                                            #kdl_key,
                                            "registry key attribute must have a single value",
                                        ));
                                    }
                                    let value = #key_val_ident[0].as_string().ok_or_else(|| {
                                        ::kdl_config::KdlConfigError::invalid_registry_key(
                                            #struct_name,
                                            #field_name,
                                            #kdl_key,
                                            "registry key must be a string",
                                        )
                                    })?.to_string();
                                    #key_ident = Some(value);
                                    #node_copy_ident = Some(#consume_expr);
                                }
                            }
                        }
                    }
                    SelectorAst::Func(path) => {
                        let key_fn = selector_func_path(path);
                        quote! {
                            if #key_ident.is_none() {
                                let value = #key_fn(#child_ident)?;
                                #key_ident = Some(value);
                                #node_copy_ident = Some(#child_ident.clone());
                            }
                        }
                    }
                    SelectorAst::Name | SelectorAst::Any(_) => {
                        quote! {
                            return Err(::kdl_config::KdlConfigError::invalid_registry_key(
                                #struct_name,
                                #field_name,
                                #kdl_key,
                                "unsupported selector in registry any(...)",
                            ));
                        }
                    }
                };
                attempts.push(attempt);
            }
            quote! {
                let mut #key_ident: ::core::option::Option<String> = None;
                let mut #node_copy_ident: ::core::option::Option<::kdl_config::KdlNode> = None;
                #(#attempts)*
                let #key_ident = #key_ident.ok_or_else(|| {
                    ::kdl_config::KdlConfigError::invalid_registry_key(
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        "missing registry key",
                    )
                })?;
                let mut #node_copy_ident = #node_copy_ident.unwrap_or_else(|| #child_ident.clone());
            }
        }
        SelectorAst::Name => {
            quote! {
                return Err(::kdl_config::KdlConfigError::invalid_registry_key(
                    #struct_name,
                    #field_name,
                    #kdl_key,
                    "registry key cannot be derived from name()",
                ));
            }
        }
    }
}

fn children_map_key_extract(
    selector: &SelectorAst,
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
    key_ty: &syn::Type,
    child_ident: &Ident,
    key_val_ident: &Ident,
    key_ident: &Ident,
    node_copy_ident: &Ident,
    missing_arg_error: &TokenStream,
    missing_attr_error: &TokenStream,
    consume: bool,
) -> TokenStream {
    match selector {
        SelectorAst::Arg(index) => {
            let idx = *index as usize;
            let consume_expr = if consume {
                quote! { #child_ident.without_arg(#idx) }
            } else {
                quote! { #child_ident.clone() }
            };
            quote! {
                let #key_val_ident = #child_ident.arg(#idx).ok_or_else(|| {
                    #missing_arg_error
                })?;
                let #key_ident = ::kdl_config::convert_value_checked::<#key_ty>(
                    #key_val_ident,
                    #struct_name,
                    #field_name,
                    #kdl_key,
                    ::kdl_config::Placement::Children,
                )?;
                let mut #node_copy_ident = #consume_expr;
            }
        }
        SelectorAst::Attr(name) => {
            let consume_expr = if consume {
                quote! { #child_ident.without_attr(#name) }
            } else {
                quote! { #child_ident.clone() }
            };
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
                        node_path: None,
                        location: None,
                    });
                }
                let #key_ident = ::kdl_config::convert_value_checked::<#key_ty>(
                    &#key_val_ident[0],
                    #struct_name,
                    #field_name,
                    #kdl_key,
                    ::kdl_config::Placement::Children,
                )?;
                let mut #node_copy_ident = #consume_expr;
            }
        }
        SelectorAst::Func(path) => {
            let key_fn = selector_func_path(path);
            quote! {
                let #key_ident = #key_fn(#child_ident)?;
                let mut #node_copy_ident = #child_ident.clone();
            }
        }
        SelectorAst::Any(selectors) => {
            let mut attempts = Vec::new();
            for selector in selectors {
                let attempt = match selector {
                    SelectorAst::Arg(index) => {
                        let idx = *index as usize;
                        let consume_expr = if consume {
                            quote! { #child_ident.without_arg(#idx) }
                        } else {
                            quote! { #child_ident.clone() }
                        };
                        quote! {
                            if #key_ident.is_none() {
                                if let Some(#key_val_ident) = #child_ident.arg(#idx) {
                                    let value = ::kdl_config::convert_value_checked::<#key_ty>(
                                        #key_val_ident,
                                        #struct_name,
                                        #field_name,
                                        #kdl_key,
                                        ::kdl_config::Placement::Children,
                                    )?;
                                    #key_ident = Some(value);
                                    #node_copy_ident = Some(#consume_expr);
                                }
                            }
                        }
                    }
                    SelectorAst::Attr(name) => {
                        let consume_expr = if consume {
                            quote! { #child_ident.without_attr(#name) }
                        } else {
                            quote! { #child_ident.clone() }
                        };
                        quote! {
                            if #key_ident.is_none() {
                                if let Some(#key_val_ident) = #child_ident.attr_values(#name) {
                                    if #key_val_ident.len() != 1 {
                                        return Err(::kdl_config::KdlConfigError {
                                            struct_name: #struct_name.to_string(),
                                            field_name: Some(#field_name.to_string()),
                                            kdl_key: Some(#kdl_key.to_string()),
                                            placement: ::kdl_config::Placement::Children,
                                            required: false,
                                            kind: ::kdl_config::ErrorKind::Custom("map key attribute must have a single value".to_string()),
                                            node_path: None,
                                            location: None,
                                        });
                                    }
                                    let value = ::kdl_config::convert_value_checked::<#key_ty>(
                                        &#key_val_ident[0],
                                        #struct_name,
                                        #field_name,
                                        #kdl_key,
                                        ::kdl_config::Placement::Children,
                                    )?;
                                    #key_ident = Some(value);
                                    #node_copy_ident = Some(#consume_expr);
                                }
                            }
                        }
                    }
                    SelectorAst::Func(path) => {
                        let key_fn = selector_func_path(path);
                        quote! {
                            if #key_ident.is_none() {
                                let value = #key_fn(#child_ident)?;
                                #key_ident = Some(value);
                                #node_copy_ident = Some(#child_ident.clone());
                            }
                        }
                    }
                    SelectorAst::Name | SelectorAst::Any(_) => {
                        quote! {
                            return Err(::kdl_config::KdlConfigError::incompatible_placement(
                                #struct_name,
                                #field_name,
                                "unsupported selector in children_map any(...)",
                            ));
                        }
                    }
                };
                attempts.push(attempt);
            }
            quote! {
                let mut #key_ident: ::core::option::Option<#key_ty> = None;
                let mut #node_copy_ident: ::core::option::Option<::kdl_config::KdlNode> = None;
                #(#attempts)*
                let #key_ident = #key_ident.ok_or_else(|| {
                    #missing_arg_error
                })?;
                let mut #node_copy_ident = #node_copy_ident.unwrap_or_else(|| #child_ident.clone());
            }
        }
        SelectorAst::Name => {
            quote! {
                return Err(::kdl_config::KdlConfigError::incompatible_placement(
                    #struct_name,
                    #field_name,
                    "children_map key cannot be derived from name() in map_node mode",
                ));
            }
        }
    }
}

fn selector_inject_extract(
    selector: &SelectorAst,
    struct_name: &str,
    field_name: &str,
    child_ident: &Ident,
    value_ident: &Ident,
    node_copy_ident: &Ident,
    consume: bool,
) -> TokenStream {
    let missing_error = quote! {
        ::kdl_config::KdlConfigError::custom(
            #struct_name,
            format!("missing selector value for injected field '{}'", #field_name),
        )
    };

    let attr_value_error = quote! {
        ::kdl_config::KdlConfigError::custom(
            #struct_name,
            "selector attribute must have a single value",
        )
    };

    match selector {
        SelectorAst::Arg(index) => {
            let idx = *index as usize;
            let consume_expr = if consume {
                quote! { #node_copy_ident = #node_copy_ident.without_arg(#idx); }
            } else {
                quote! {}
            };
            quote! {
                let mut #node_copy_ident = #child_ident.clone();
                let #value_ident = #child_ident.arg(#idx).ok_or_else(|| {
                    #missing_error
                })?.clone();
                #consume_expr
            }
        }
        SelectorAst::Attr(name) => {
            let consume_expr = if consume {
                quote! { #node_copy_ident = #node_copy_ident.without_attr(#name); }
            } else {
                quote! {}
            };
            quote! {
                let mut #node_copy_ident = #child_ident.clone();
                let values = #child_ident.attr_values(#name).ok_or_else(|| {
                    #missing_error
                })?;
                if values.len() != 1 {
                    return Err(#attr_value_error);
                }
                let #value_ident = values[0].clone();
                #consume_expr
            }
        }
        SelectorAst::Name => {
            quote! {
                let mut #node_copy_ident = #child_ident.clone();
                let #value_ident = ::kdl_config::KdlValue::String(#child_ident.base_name().to_string());
            }
        }
        SelectorAst::Func(path) => {
            let key_fn = selector_func_path(path);
            quote! {
                let mut #node_copy_ident = #child_ident.clone();
                let value = #key_fn(#child_ident)?;
                let #value_ident: ::kdl_config::KdlValue = value.into();
            }
        }
        SelectorAst::Any(selectors) => {
            let mut attempts = Vec::new();
            for selector in selectors {
                let attempt = match selector {
                    SelectorAst::Arg(index) => {
                        let idx = *index as usize;
                        let consume_expr = if consume {
                            quote! { #child_ident.without_arg(#idx) }
                        } else {
                            quote! { #child_ident.clone() }
                        };
                        quote! {
                            if #value_ident.is_none() {
                                if let Some(val) = #child_ident.arg(#idx) {
                                    #value_ident = Some(val.clone());
                                    #node_copy_ident = Some(#consume_expr);
                                }
                            }
                        }
                    }
                    SelectorAst::Attr(name) => {
                        let consume_expr = if consume {
                            quote! { #child_ident.without_attr(#name) }
                        } else {
                            quote! { #child_ident.clone() }
                        };
                        quote! {
                            if #value_ident.is_none() {
                                if let Some(values) = #child_ident.attr_values(#name) {
                                    if values.len() != 1 {
                                        return Err(#attr_value_error);
                                    }
                                    #value_ident = Some(values[0].clone());
                                    #node_copy_ident = Some(#consume_expr);
                                }
                            }
                        }
                    }
                    SelectorAst::Name => {
                        quote! {
                            if #value_ident.is_none() {
                                #value_ident = Some(::kdl_config::KdlValue::String(#child_ident.base_name().to_string()));
                                #node_copy_ident = Some(#child_ident.clone());
                            }
                        }
                    }
                    SelectorAst::Func(path) => {
                        let key_fn = selector_func_path(path);
                        quote! {
                            if #value_ident.is_none() {
                                let value = #key_fn(#child_ident)?;
                                #value_ident = Some(value.into());
                                #node_copy_ident = Some(#child_ident.clone());
                            }
                        }
                    }
                    SelectorAst::Any(_) => {
                        quote! {
                            return Err(::kdl_config::KdlConfigError::custom(
                                #struct_name,
                                "unsupported selector in any(...) for injection",
                            ));
                        }
                    }
                };
                attempts.push(attempt);
            }
            quote! {
                let mut #value_ident: ::core::option::Option<::kdl_config::KdlValue> = None;
                let mut #node_copy_ident: ::core::option::Option<::kdl_config::KdlNode> = None;
                #(#attempts)*
                let #value_ident = #value_ident.ok_or_else(|| {
                    #missing_error
                })?;
                let #node_copy_ident = #node_copy_ident.unwrap_or_else(|| #child_ident.clone());
            }
        }
    }
}

fn generate_collection_parser(
    field: &FieldInfo,
    struct_name: &str,
    field_overrides: &TokenStream,
    mark_usage: TokenStream,
    collection: &crate::attrs::CollectionSpec,
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
    let is_registry = matches!(collection.mode, CollectionMode::Registry { .. });

    let (collection_kind, key_ty_opt, val_ty) = if is_registry {
        if field.is_hashmap {
            let (_key_ty, val_ty) = extract_hashmap_types(ty).expect("registry requires HashMap");
            (ChildrenMapKind::HashMap, None, val_ty)
        } else {
            let (val_ty, is_option_vec) = crate::attrs::extract_registry_vec_value(ty)
                .expect("registry vec requires Vec<(String, T)>");
            let kind = if is_option_vec {
                ChildrenMapKind::OptionVec
            } else {
                ChildrenMapKind::Vec
            };
            (kind, None, val_ty)
        }
    } else {
        let (kind, key_ty, val_ty) =
            extract_children_map_types(ty).expect("children_map requires HashMap or Vec<(K, V)>");
        (kind, Some(key_ty), val_ty)
    };
    let key_ty = if is_registry {
        None
    } else {
        Some(key_ty_opt.expect("children_map requires key type"))
    };

    let (container_name, map_node_name) = match &collection.mode {
        CollectionMode::Registry { container } => (Some(container.as_str()), None),
        CollectionMode::ChildrenMapNode { node } => (None, Some(node.as_str())),
        CollectionMode::ChildrenMapAll => (None, None),
    };
    let container = container_name
        .map(|s| quote! { #s })
        .unwrap_or_else(|| quote! { #kdl_key });
    let map_node = map_node_name.map(|s| quote! { #s });

    let missing_key = if is_registry {
        quote! { #container }
    } else if let Some(map_node) = map_node.as_ref() {
        quote! { #map_node }
    } else {
        quote! { #kdl_key }
    };
    let missing_placement = if is_registry {
        quote! { ::kdl_config::Placement::Registry }
    } else {
        quote! { ::kdl_config::Placement::Children }
    };

    let duplicate_error = if is_registry {
        quote! {
            return Err(::kdl_config::KdlConfigError::invalid_registry_key(
                #struct_name,
                #field_name,
                #kdl_key,
                "duplicate registry key",
            ));
        }
    } else {
        quote! {
            return Err(::kdl_config::KdlConfigError {
                struct_name: #struct_name.to_string(),
                field_name: Some(#field_name.to_string()),
                kdl_key: Some(#kdl_key.to_string()),
                placement: ::kdl_config::Placement::Children,
                required: false,
                kind: ::kdl_config::ErrorKind::Custom("duplicate map key".to_string()),
                node_path: None,
                location: None,
            });
        }
    };

    let missing_arg_error = quote! {
        ::kdl_config::KdlConfigError {
            struct_name: #struct_name.to_string(),
            field_name: Some(#field_name.to_string()),
            kdl_key: Some(#kdl_key.to_string()),
            placement: ::kdl_config::Placement::Children,
            required: false,
            kind: ::kdl_config::ErrorKind::Custom("missing map key argument".to_string()),
            node_path: None,
            location: None,
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
            node_path: None,
            location: None,
        }
    };

    let inject_name = collection.inject.as_ref().map(|inject| match inject {
        InjectOpt::Implicit => "name".to_string(),
        InjectOpt::Field(name) => name.clone(),
    });

    let consume = collection.consume;
    let selector = collection.selector.clone();
    let inject_selector = selector.clone();

    let inject_value_ident = format_ident!("__kdl_inject_value_{}", field_ident);
    let inject_extract = if inject_name.is_some() {
        selector_inject_extract(
            &inject_selector,
            struct_name,
            &field_name,
            &child_ident,
            &inject_value_ident,
            &node_copy_ident,
            consume,
        )
    } else {
        quote! {}
    };

    let key_from_inject = if inject_name.is_some() {
        if is_registry {
            quote! {
                let #key_ident = #inject_value_ident.as_string().ok_or_else(|| {
                    ::kdl_config::KdlConfigError::invalid_registry_key(
                        #struct_name,
                        #field_name,
                        #kdl_key,
                        "registry key must be a string",
                    )
                })?.to_string();
            }
        } else {
            let key_ty = key_ty.as_ref().expect("children_map requires key type");
            quote! {
                let #key_ident = ::kdl_config::convert_value_checked::<#key_ty>(
                    &#inject_value_ident,
                    #struct_name,
                    #field_name,
                    #kdl_key,
                    ::kdl_config::Placement::Children,
                )?;
            }
        }
    } else {
        quote! {}
    };

    let key_extract = if inject_name.is_some() {
        quote! { #inject_extract #key_from_inject }
    } else if is_registry {
        registry_key_extract(
            &selector,
            struct_name,
            &field_name,
            kdl_key,
            &child_ident,
            &key_val_ident,
            &key_ident,
            &node_copy_ident,
            consume,
        )
    } else if map_node.is_some() {
        let key_ty = key_ty.as_ref().expect("children_map requires key type");
        children_map_key_extract(
            &selector,
            struct_name,
            &field_name,
            kdl_key,
            key_ty,
            &child_ident,
            &key_val_ident,
            &key_ident,
            &node_copy_ident,
            &missing_arg_error,
            &missing_attr_error,
            consume,
        )
    } else {
        let key_ty = key_ty.as_ref().expect("children_map requires key type");
        quote! {
            let #key_val_ident = ::kdl_config::KdlValue::String(#child_ident.name_str().to_string());
            let #key_ident = ::kdl_config::convert_value_checked::<#key_ty>(
                &#key_val_ident,
                #struct_name,
                #field_name,
                #kdl_key,
                ::kdl_config::Placement::Children,
            )?;
            let mut #node_copy_ident = #child_ident.clone();
        }
    };

    let inject_attr = if let Some(name) = inject_name.as_ref() {
        quote! {
            let mut entry = ::kdl_config::KdlEntry::new(#inject_value_ident);
            entry.set_name(Some(#name));
            #node_copy_ident.entries_mut().push(entry);
        }
    } else {
        quote! {}
    };

    let (field_init, map_insert, finalize_vec) = match collection_kind {
        ChildrenMapKind::HashMap => {
            let field_init = if is_registry {
                quote! { let mut #field_ident: #ty = ::std::collections::HashMap::new(); }
            } else {
                quote! { let mut #field_ident: #ty = ::std::collections::HashMap::new(); }
            };
            let map_insert = if is_registry {
                quote! {
                    match field_config.conflict {
                        ::kdl_config::ConflictPolicy::Error => {
                            if #field_ident.contains_key(&#key_ident) {
                                #duplicate_error
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
            } else {
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
                }
            };
            (field_init, map_insert, quote! {})
        }
        ChildrenMapKind::Vec => {
            let (field_init, map_insert) = if is_registry {
                (
                    quote! { let mut #field_ident: Vec<(String, #val_ty)> = Vec::new(); },
                    quote! {
                        match field_config.conflict {
                            ::kdl_config::ConflictPolicy::Error => {
                                if #field_ident.iter().any(|(key, _)| key == &#key_ident) {
                                    #duplicate_error
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
                    },
                )
            } else {
                (
                    {
                        let key_ty = key_ty.expect("children_map requires key type");
                        quote! { let mut #field_ident: Vec<(#key_ty, #val_ty)> = Vec::new(); }
                    },
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
                )
            };
            (field_init, map_insert, quote! {})
        }
        ChildrenMapKind::OptionVec => {
            let (field_init, map_insert) = if is_registry {
                (
                    quote! { let mut #field_ident: Vec<(String, #val_ty)> = Vec::new(); },
                    quote! {
                        match field_config.conflict {
                            ::kdl_config::ConflictPolicy::Error => {
                                if #field_ident.iter().any(|(key, _)| key == &#key_ident) {
                                    #duplicate_error
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
                    },
                )
            } else {
                (
                    {
                        let key_ty = key_ty.expect("children_map requires key type");
                        quote! { let mut #field_ident: Vec<(#key_ty, #val_ty)> = Vec::new(); }
                    },
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
                )
            };
            let finalize_vec = if is_registry {
                quote! {
                    let #field_ident: #ty = if #field_ident.is_empty() { None } else { Some(#field_ident) };
                }
            } else {
                quote! {
                    let #field_ident: #ty = if #field_ident.is_empty() { None } else { Some(#field_ident) };
                }
            };
            (field_init, map_insert, finalize_vec)
        }
    };

    let filter = if is_registry {
        quote! {
            if #child_ident.name_str() != #container {
                continue;
            }
        }
    } else if let Some(map_node) = map_node.as_ref() {
        quote! {
            if #child_ident.name_str() != #map_node {
                continue;
            }
        }
    } else {
        quote! {}
    };

    let parent_claims = path_parent_claims(field);
    let claims_shadow = path_claims_shadow(field);
    let scan_block = quote! {
        for (child_index, #child_ident) in node.iter_children().enumerate() {
            #filter
            let child_ctx = ctx.with_child(#child_ident.name_str(), child_index);
            #key_extract
            #inject_attr
            let #value_ident = <#val_ty as ::kdl_config::KdlDecode>::decode(&#node_copy_ident, &child_ctx)
                .map_err(|err| err.with_context(child_ctx.source, child_ctx.path.as_ref(), Some(#child_ident.span().offset())))?;
            #map_insert
            claims.claim_child(child_index);
        }
    };

    let scan_block = wrap_path_scan(field, struct_name, &field_name, kdl_key, scan_block);

    quote! {
        #mark_usage
        let #field_ident: #ty = {
            #parent_claims
            #claims_shadow
            let field_config = ::kdl_config::resolve_field(&struct_config, #field_overrides);
            #field_init

            #scan_block

            if #required && #field_ident.is_empty() {
                return Err(::kdl_config::KdlConfigError::missing_required(
                    #struct_name,
                    #field_name,
                    #missing_key,
                    #missing_placement,
                ));
            }

            #finalize_vec
            #field_ident
        };
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
                        &::kdl_config::KdlValue::Integer(#n)
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
                        &::kdl_config::KdlValue::Float(#f)
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
                        &::kdl_config::KdlValue::Bool(#b)
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
                        &::kdl_config::KdlValue::String(::std::string::String::from(#s))
                    ).expect(concat!("invalid default value for ", stringify!(#ty)))
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Validation codegen
// ---------------------------------------------------------------------------

/// Generate validation code for a single field (non-cross-field rules).
/// Cross-field validations are handled separately in `generate_cross_field_validations`.
fn generate_field_validation(
    field: &FieldInfo,
    struct_name: &str,
    kind: &FieldKind,
) -> TokenStream {
    let validations = &field.schema.validations;
    if validations.is_empty() {
        return quote! {};
    }

    // Separate validation rules by category
    let mut scalar_rules: Vec<&ValidationRule> = Vec::new();
    let mut count_rules: Vec<&ValidationRule> = Vec::new();
    let mut func_rules: Vec<&ValidationRule> = Vec::new();
    // Cross-field rules are skipped here; they run after all fields are decoded.

    for v in validations {
        match v {
            ValidationRule::Func(_) => func_rules.push(v),
            ValidationRule::MinItems(_) | ValidationRule::MaxItems(_) => count_rules.push(v),
            ValidationRule::LessThan(_)
            | ValidationRule::LessThanOrEqual(_)
            | ValidationRule::GreaterThan(_)
            | ValidationRule::GreaterThanOrEqual(_)
            | ValidationRule::EqualTo(_)
            | ValidationRule::NotEqualTo(_) => {
                // Cross-field — handled in generate_cross_field_validations
            }
            _ => scalar_rules.push(v),
        }
    }

    let field_ident = &field.ident;
    let field_name = field.ident.to_string();
    let kdl_key = &field.kdl_key;

    let mut blocks = Vec::new();

    // Scalar validations (numeric range, string length, etc.)
    if !scalar_rules.is_empty() {
        let validation_exprs: Vec<TokenStream> =
            scalar_rules.iter().map(|v| quote! { #v }).collect();
        blocks.push(quote! {
            ::kdl_config::run_field_validations(
                &#field_ident,
                &[#(#validation_exprs),*],
                #struct_name,
                #field_name,
                #kdl_key,
            )?;
        });
    }

    // Count validations (MinItems/MaxItems for vecs and collections)
    if !count_rules.is_empty() {
        match kind {
            FieldKind::ValueVec | FieldKind::NodeVec | FieldKind::Collection => {
                let validation_exprs: Vec<TokenStream> =
                    count_rules.iter().map(|v| quote! { #v }).collect();
                blocks.push(quote! {
                    ::kdl_config::run_count_validations(
                        &#field_ident,
                        &[#(#validation_exprs),*],
                        #struct_name,
                        #field_name,
                        #kdl_key,
                    )?;
                });
            }
            _ => {
                // MinItems/MaxItems on a scalar field is a compile-time error
                blocks.push(
                    quote! { compile_error!("min_items/max_items validation is only valid for Vec/collection fields"); },
                );
            }
        }
    }

    // Func validations
    for v in &func_rules {
        if let ValidationRule::Func(path_str) = v {
            let func_path: syn::Path = syn::parse_str(path_str)
                .expect("invalid function path in validate(func(...))");
            blocks.push(quote! {
                ::kdl_config::run_func_validation(
                    &#field_ident,
                    #func_path,
                    #struct_name,
                    #field_name,
                    #kdl_key,
                )?;
            });
        }
    }

    if blocks.is_empty() {
        quote! {}
    } else {
        quote! { #(#blocks)* }
    }
}

/// Generate cross-field validation code that runs after all fields are decoded.
fn generate_cross_field_validations(fields: &[FieldInfo], struct_name: &str) -> TokenStream {
    let mut blocks = Vec::new();

    for field in fields {
        let validations = &field.schema.validations;
        let field_ident = &field.ident;
        let field_name = field.ident.to_string();
        let kdl_key = &field.kdl_key;

        for v in validations {
            let other_field_name = match v {
                ValidationRule::LessThan(f)
                | ValidationRule::LessThanOrEqual(f)
                | ValidationRule::GreaterThan(f)
                | ValidationRule::GreaterThanOrEqual(f)
                | ValidationRule::EqualTo(f)
                | ValidationRule::NotEqualTo(f) => f,
                _ => continue,
            };

            // Find the other field's ident by matching kdl_key or field ident name
            let other_ident = fields
                .iter()
                .find(|f| f.kdl_key == *other_field_name || f.ident.to_string() == *other_field_name)
                .map(|f| &f.ident);

            let validation_expr: TokenStream = quote! { #v };

            if let Some(other_ident) = other_ident {
                blocks.push(quote! {
                    ::kdl_config::run_cross_field_validation(
                        ::kdl_config::AsF64::as_f64(&#field_ident),
                        ::kdl_config::AsF64::as_f64(&#other_ident),
                        &#validation_expr,
                        #struct_name,
                        #field_name,
                        #kdl_key,
                    )?;
                });
            } else {
                let msg = format!(
                    "cross-field validation references unknown field '{}'",
                    other_field_name
                );
                blocks.push(quote! { compile_error!(#msg); });
            }
        }
    }

    if blocks.is_empty() {
        quote! {}
    } else {
        quote! { #(#blocks)* }
    }
}

fn snake_to_upper_camel(ident: &Ident) -> String {
    let raw = ident.to_string();
    let name = raw.strip_prefix("r#").unwrap_or(&raw);
    name.split('_')
        .filter(|s| !s.is_empty())
        .map(|s| {
            let mut chars = s.chars();
            match chars.next() {
                Some(c) => {
                    let mut out = c.to_uppercase().to_string();
                    out.push_str(chars.as_str());
                    out
                }
                None => String::new(),
            }
        })
        .collect()
}
