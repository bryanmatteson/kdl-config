use proc_macro2::TokenStream;
use quote::quote;

use crate::attrs::{
    BoolMode, CollectionMode, DefaultPlacement, FieldInfo, FieldKind, FlagStyle, RenderPlacement,
    SelectorAst, StructAttrs, field_kind,
};
use crate::render_gen::FieldAccessor;

pub fn generate_update_impl(
    struct_name: &syn::Ident,
    struct_attrs: &StructAttrs,
    fields: &[FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
    prelude: Option<TokenStream>,
) -> TokenStream {
    let struct_name_str = struct_name.to_string();
    let struct_overrides = crate::parse_gen::generate_struct_overrides(struct_attrs);

    let mut update_fields_non_bool = Vec::new();
    let mut update_fields_bool = Vec::new();
    for field in fields.iter().filter(|field| !field.is_skipped) {
        let tokens = generate_field_update(field, struct_name, struct_attrs, accessor);
        if field.is_bool {
            update_fields_bool.push(tokens);
        } else {
            update_fields_non_bool.push(tokens);
        }
    }

    let prelude = prelude.unwrap_or_else(|| quote! {});

    quote! {
        impl ::kdl_config::KdlUpdate for #struct_name {
            fn update(&self, node: &mut ::kdl_config::KdlNode, ctx: &::kdl_config::UpdateContext) -> ::core::result::Result<(), ::kdl_config::KdlConfigError> {
                use ::kdl_config::KdlNodeExt as _;
                let struct_overrides = #struct_overrides;
                let struct_config = ::kdl_config::resolve_struct(ctx.config, struct_overrides);
                #prelude
                #(#update_fields_non_bool)*
                #(#update_fields_bool)*
                let _ = #struct_name_str;
                Ok(())
            }
        }
    }
}

pub(crate) fn generate_field_update(
    field: &FieldInfo,
    struct_name: &syn::Ident,
    struct_attrs: &StructAttrs,
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let access = accessor(field);
    let field_name = field.ident.to_string();
    let kdl_key = field.kdl_key.clone();
    let struct_name_str = struct_name.to_string();

    if field.is_modifier {
        let update_body = generate_update_modifier(field, &access);
        return quote! {
            #update_body
            let _ = (#field_name, #kdl_key);
        };
    }

    let cond = render_condition(field, &access);
    let kind = field_kind(field);
    let render_placement = render_placement_for(struct_attrs, field, kind);

    let update_body = match render_placement {
        RenderPlacement::Attr => generate_update_attr(field, &access),
        RenderPlacement::Value => generate_update_value(field, &access),
        RenderPlacement::Child => generate_update_child(field, &access, &struct_name_str),
        RenderPlacement::Children => generate_update_children(field, &access, &struct_name_str),
        RenderPlacement::Registry => generate_update_collection(field, &access, &struct_name_str),
    };

    let remove_body = match render_placement {
        RenderPlacement::Attr => {
            quote! {
                ::kdl_config::remove_attr_entries(node, #kdl_key);
            }
        }
        RenderPlacement::Value | RenderPlacement::Child | RenderPlacement::Children => {
            quote! {
                if let Some(children) = node.children_mut() {
                    children.nodes_mut().retain(|child| child.base_name() != #kdl_key);
                }
            }
        }
        RenderPlacement::Registry => {
            let filter = match field.collection.as_ref().map(|spec| &spec.mode) {
                Some(CollectionMode::Registry { container }) => {
                    quote! { child.base_name() != #container }
                }
                Some(CollectionMode::ChildrenMapNode { node }) => {
                    quote! { child.base_name() != #node }
                }
                Some(CollectionMode::ChildrenMapAll) | None => {
                    quote! { false }
                }
            };
            quote! {
                if let Some(children) = node.children_mut() {
                    children.nodes_mut().retain(|child| { #filter });
                }
            }
        }
    };

    quote! {
        if #cond {
            #update_body
        } else {
            #remove_body
        }
        let _ = (#field_name, #kdl_key);
    }
}

fn render_condition(field: &FieldInfo, accessor: &FieldAccessor) -> TokenStream {
    if let Some(ref predicate) = field.skip_serializing_if {
        let path: TokenStream = predicate.parse().unwrap_or_else(|_| {
            let ident = syn::Ident::new(predicate, proc_macro2::Span::call_site());
            quote! { #ident }
        });
        let reference = &accessor.reference;
        quote! { !(#path(#reference)) }
    } else {
        quote! { true }
    }
}

/// Determine the render placement for a field during update codegen.
///
/// NOTE: This intentionally differs from `render_gen::render_placement_for` in
/// collection handling. Update always returns `Registry` for collections because
/// it patches via registry semantics regardless of the underlying mode, while
/// render dispatches on `CollectionMode` to produce the correct output shape.
fn render_placement_for(
    struct_attrs: &StructAttrs,
    field: &FieldInfo,
    kind: FieldKind,
) -> RenderPlacement {
    if let Some(render) = field.render {
        return render;
    }

    if let Some(collection) = field.collection.as_ref() {
        let _ = collection;
        return RenderPlacement::Registry;
    }

    if field.placement.children_any {
        if field.is_vec || field.is_option_vec {
            return RenderPlacement::Children;
        }
        return RenderPlacement::Child;
    }
    if field.placement.child {
        return RenderPlacement::Child;
    }
    if field.placement.children {
        return RenderPlacement::Children;
    }
    let is_vec_field = field.is_vec || field.is_option_vec;
    if field.placement.value && is_vec_field {
        return RenderPlacement::Value;
    }
    if field.placement.attr
        || field.placement.keyed
        || field.placement.positional.is_some()
        || field.placement.positional_list
        || field.placement.flag.is_some()
    {
        return RenderPlacement::Attr;
    }
    if field.placement.value {
        return RenderPlacement::Value;
    }

    let default_placement = struct_attrs
        .default_placement
        .clone()
        .unwrap_or(DefaultPlacement::Exhaustive);
    match kind {
        FieldKind::ValueScalar => RenderPlacement::Attr,
        FieldKind::ValueVec => match default_placement {
            DefaultPlacement::Attr => RenderPlacement::Attr,
            DefaultPlacement::Exhaustive | DefaultPlacement::Value | DefaultPlacement::Child => {
                RenderPlacement::Value
            }
        },
        FieldKind::Node => RenderPlacement::Child,
        FieldKind::NodeVec => RenderPlacement::Children,
        FieldKind::Flatten => RenderPlacement::Child,
        FieldKind::Collection => RenderPlacement::Children,
        FieldKind::Modifier => RenderPlacement::Child,
    }
}

fn generate_update_attr(field: &FieldInfo, access: &FieldAccessor) -> TokenStream {
    let kdl_key = &field.kdl_key;

    let is_bool = field.is_bool;
    let bool_mode = field.bool_mode.clone();
    let flag_style = field.flag_style.clone();

    if !is_bool {
        if field.placement.positional_list {
            let reference = &access.reference;
            if field.is_option_vec {
                return quote! {
                    if let Some(values) = #reference {
                        node.entries_mut().retain(|entry| entry.name().is_some());
                        for (idx, value) in values.iter().enumerate() {
                            let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(value.clone()));
                            ::kdl_config::update_or_insert_positional(node, idx, v);
                        }
                    } else {
                        node.entries_mut().retain(|entry| entry.name().is_some());
                    }
                };
            }
            return quote! {
                node.entries_mut().retain(|entry| entry.name().is_some());
                for (idx, value) in (#reference).iter().enumerate() {
                    let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(value.clone()));
                    ::kdl_config::update_or_insert_positional(node, idx, v);
                }
            };
        }

        if let Some(positional) = field.placement.positional {
            let reference = &access.reference;
            if field.is_optional {
                return quote! {
                    if let Some(value) = #reference {
                        let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(value.clone()));
                        ::kdl_config::update_or_insert_positional(node, #positional, v);
                    } else {
                        ::kdl_config::remove_positional_entry(node, #positional);
                    }
                };
            }
            let value = &access.value;
            return quote! {
                let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(#value.clone()));
                ::kdl_config::update_or_insert_positional(node, #positional, v);
            };
        }
    }

    let update_keyed = if field.is_vec || field.is_option_vec {
        let reference = &access.reference;
        if field.is_option_vec {
            quote! {
                if let Some(values) = #reference {
                    ::kdl_config::remove_attr_entries(node, #kdl_key);
                    for value in values {
                        let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(value.clone()));
                        ::kdl_config::update_or_insert_attr(node, #kdl_key, v);
                    }
                } else {
                    ::kdl_config::remove_attr_entries(node, #kdl_key);
                }
            }
        } else {
            quote! {
                ::kdl_config::remove_attr_entries(node, #kdl_key);
                for value in #reference {
                    let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(value.clone()));
                    ::kdl_config::update_or_insert_attr(node, #kdl_key, v);
                }
            }
        }
    } else if field.is_optional {
        let reference = &access.reference;
        quote! {
            if let Some(value) = #reference {
                let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(value.clone()));
                ::kdl_config::update_or_insert_attr(node, #kdl_key, v);
            } else {
                ::kdl_config::remove_attr_entries(node, #kdl_key);
            }
        }
    } else {
        let value = &access.value;
        quote! {
            let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(#value.clone()));
            ::kdl_config::update_or_insert_attr(node, #kdl_key, v);
        }
    };

    if !is_bool {
        return update_keyed;
    }

    let reference = &access.reference;
    let bool_value = &access.bool_value;

    let bool_mode_tokens = bool_mode_tokens(bool_mode.unwrap_or(BoolMode::PresenceAndValue));
    let flag_style_tokens = flag_style_tokens(flag_style.unwrap_or(FlagStyle::Both));
    let bool_opt_expr = if field.is_optional {
        quote! { *#reference }
    } else {
        quote! { Some(#bool_value) }
    };

    quote! {
        let field_config = ::kdl_config::resolve_field(&struct_config, ::kdl_config::FieldOverrides {
            bool_mode: Some(#bool_mode_tokens),
            flag_style: Some(#flag_style_tokens),
            conflict: None,
        });

        let (pos_flag, neg_flag, alt_pos, alt_neg) = match field_config.flag_style {
            ::kdl_config::FlagStyle::Both => (
                #kdl_key.to_string(),
                format!("no-{}", #kdl_key),
                Some(format!("with-{}", #kdl_key)),
                Some(format!("without-{}", #kdl_key)),
            ),
            ::kdl_config::FlagStyle::ValueNo => (
                #kdl_key.to_string(),
                format!("no-{}", #kdl_key),
                Some(format!("with-{}", #kdl_key)),
                Some(format!("without-{}", #kdl_key)),
            ),
            ::kdl_config::FlagStyle::WithWithout => (
                format!("with-{}", #kdl_key),
                format!("without-{}", #kdl_key),
                Some(#kdl_key.to_string()),
                Some(format!("no-{}", #kdl_key)),
            ),
        };

        let mut remove_indices = ::std::vec::Vec::new();
        for (idx, entry) in node.entries().iter().enumerate() {
            if entry.name().is_some() {
                continue;
            }
            if let ::kdl_config::KdlValue::String(s) = entry.value() {
                if s == &pos_flag || s == &neg_flag {
                    remove_indices.push(idx);
                }
                if let Some(alt) = alt_pos.as_ref() {
                    if s == alt {
                        remove_indices.push(idx);
                    }
                }
                if let Some(alt) = alt_neg.as_ref() {
                    if s == alt {
                        remove_indices.push(idx);
                    }
                }
            }
        }
        ::kdl_config::remove_entry_indices(node, &remove_indices);

        let bool_val_opt: ::core::option::Option<bool> = #bool_opt_expr;
        match field_config.bool_mode {
            ::kdl_config::BoolMode::ValueOnly => {
                ::kdl_config::remove_attr_entries(node, #kdl_key);
                if let Some(value) = bool_val_opt {
                    let v = ::kdl_config::KdlValue::Bool(value);
                    ::kdl_config::update_or_insert_attr(node, #kdl_key, v);
                }
            }
            ::kdl_config::BoolMode::PresenceOnly => {
                ::kdl_config::remove_attr_entries(node, #kdl_key);
                if let Some(true) = bool_val_opt {
                    node.entries_mut().push(::kdl_config::KdlEntry::new(::kdl_config::KdlValue::String(pos_flag)));
                }
            }
            ::kdl_config::BoolMode::PresenceAndValue => {
                ::kdl_config::remove_attr_entries(node, #kdl_key);
                if let Some(value) = bool_val_opt {
                    let token = if value { pos_flag } else { neg_flag };
                    node.entries_mut().push(::kdl_config::KdlEntry::new(::kdl_config::KdlValue::String(token)));
                }
            }
        }
    }
}

fn generate_update_value(field: &FieldInfo, access: &FieldAccessor) -> TokenStream {
    let kdl_key = &field.kdl_key;

    if field.is_vec || field.is_option_vec {
        let reference = &access.reference;
        if field.is_option_vec {
            quote! {
                if let Some(values) = #reference {
                    let mut target_child: ::core::option::Option<&mut ::kdl_config::KdlNode> = None;
                    if let Some(children) = node.children_mut() {
                        for child in children.nodes_mut().iter_mut() {
                            if child.base_name() == #kdl_key {
                                target_child = Some(child);
                                break;
                            }
                        }
                    }
                    if target_child.is_none() {
                        let new_child = ::kdl_config::KdlNode::new(#kdl_key);
                        let children = node.ensure_children();
                        children.nodes_mut().push(new_child);
                        let last = children.nodes().len() - 1;
                        target_child = children.nodes_mut().get_mut(last);
                    }
                    if let Some(child) = target_child {
                        child.entries_mut().retain(|entry| entry.name().is_none());
                        child.entries_mut().clear();
                        for value in values {
                            let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(value.clone()));
                            child.entries_mut().push(::kdl_config::KdlEntry::new(v));
                        }
                    }
                } else {
                    if let Some(children) = node.children_mut() {
                        children.nodes_mut().retain(|child| child.base_name() != #kdl_key);
                    }
                }
            }
        } else {
            quote! {
                let mut target_child: ::core::option::Option<&mut ::kdl_config::KdlNode> = None;
                if let Some(children) = node.children_mut() {
                    for child in children.nodes_mut().iter_mut() {
                        if child.base_name() == #kdl_key {
                            target_child = Some(child);
                            break;
                        }
                    }
                }
                if target_child.is_none() {
                    let new_child = ::kdl_config::KdlNode::new(#kdl_key);
                    let children = node.ensure_children();
                    children.nodes_mut().push(new_child);
                    let last = children.nodes().len() - 1;
                    target_child = children.nodes_mut().get_mut(last);
                }
                if let Some(child) = target_child {
                    child.entries_mut().retain(|entry| entry.name().is_none());
                    child.entries_mut().clear();
                    for value in #reference {
                        let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(value.clone()));
                        child.entries_mut().push(::kdl_config::KdlEntry::new(v));
                    }
                }
            }
        }
    } else if field.is_optional {
        let reference = &access.reference;
        quote! {
            if let Some(value) = #reference {
                let mut target_child: ::core::option::Option<&mut ::kdl_config::KdlNode> = None;
                if let Some(children) = node.children_mut() {
                    for child in children.nodes_mut().iter_mut() {
                        if child.base_name() == #kdl_key {
                            target_child = Some(child);
                            break;
                        }
                    }
                }
                if target_child.is_none() {
                    let new_child = ::kdl_config::KdlNode::new(#kdl_key);
                    let children = node.ensure_children();
                    children.nodes_mut().push(new_child);
                    let last = children.nodes().len() - 1;
                    target_child = children.nodes_mut().get_mut(last);
                }
                if let Some(child) = target_child {
                    child.entries_mut().retain(|entry| entry.name().is_none());
                    child.entries_mut().clear();
                    let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(value.clone()));
                    child.entries_mut().push(::kdl_config::KdlEntry::new(v));
                }
            } else {
                if let Some(children) = node.children_mut() {
                    children.nodes_mut().retain(|child| child.base_name() != #kdl_key);
                }
            }
        }
    } else {
        let value = &access.value;
        quote! {
            let mut target_child: ::core::option::Option<&mut ::kdl_config::KdlNode> = None;
            if let Some(children) = node.children_mut() {
                for child in children.nodes_mut().iter_mut() {
                    if child.base_name() == #kdl_key {
                        target_child = Some(child);
                        break;
                    }
                }
            }
            if target_child.is_none() {
                let new_child = ::kdl_config::KdlNode::new(#kdl_key);
                let children = node.ensure_children();
                children.nodes_mut().push(new_child);
                let last = children.nodes().len() - 1;
                target_child = children.nodes_mut().get_mut(last);
            }
            if let Some(child) = target_child {
                child.entries_mut().retain(|entry| entry.name().is_none());
                child.entries_mut().clear();
                let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(#value.clone()));
                child.entries_mut().push(::kdl_config::KdlEntry::new(v));
            }
        }
    }
}

fn generate_update_modifier(field: &FieldInfo, access: &FieldAccessor) -> TokenStream {
    let cond = render_condition(field, access);
    let modifier_expr = if field.is_optional {
        let reference = &access.reference;
        quote! { (*#reference).unwrap_or(::kdl_config::Modifier::Inherit) }
    } else {
        let value = &access.value;
        quote! { #value }
    };

    quote! {
        if #cond {
            let modifier = #modifier_expr;
            let base = node.base_name().to_string();
            let name = match modifier {
                ::kdl_config::Modifier::Inherit => base,
                ::kdl_config::Modifier::Append => format!("+{}", base),
                ::kdl_config::Modifier::Remove => format!("-{}", base),
                ::kdl_config::Modifier::Replace => format!("!{}", base),
            };
            node.set_name(name);
        } else {
            let base = node.base_name().to_string();
            node.set_name(base);
        }
    }
}

fn generate_update_child(field: &FieldInfo, access: &FieldAccessor, struct_name: &str) -> TokenStream {
    let kdl_key = &field.kdl_key;

    if field.is_optional {
        let reference = &access.reference;
        quote! {
            if let Some(value) = #reference {
                let mut target_child: ::core::option::Option<&mut ::kdl_config::KdlNode> = None;
                if let Some(children) = node.children_mut() {
                    for child in children.nodes_mut().iter_mut() {
                        if child.base_name() == #kdl_key {
                            target_child = Some(child);
                            break;
                        }
                    }
                }
                if let Some(child) = target_child {
                    value.update(child, ctx)?;
                } else {
                    let rendered = ::kdl_config::to_kdl(value, #kdl_key);
                    let doc: ::kdl_config::KdlDocument = rendered.parse().map_err(|e: kdl::KdlError| {
                        ::kdl_config::KdlConfigError::custom(#struct_name, e.to_string())
                    })?;
                    let new_node = doc.nodes()[0].clone();
                    node.ensure_children().nodes_mut().push(new_node);
                }
            } else if let Some(children) = node.children_mut() {
                children.nodes_mut().retain(|child| child.base_name() != #kdl_key);
            }
        }
    } else {
        let value = &access.value;
        let reference = &access.reference;
        quote! {
            let mut target_child: ::core::option::Option<&mut ::kdl_config::KdlNode> = None;
            if let Some(children) = node.children_mut() {
                for child in children.nodes_mut().iter_mut() {
                    if child.base_name() == #kdl_key {
                        target_child = Some(child);
                        break;
                    }
                }
            }
            if let Some(child) = target_child {
                #value.update(child, ctx)?;
            } else {
                let rendered = ::kdl_config::to_kdl(#reference, #kdl_key);
                let doc: ::kdl_config::KdlDocument = rendered.parse().map_err(|e: kdl::KdlError| {
                    ::kdl_config::KdlConfigError::custom(#struct_name, e.to_string())
                })?;
                let new_node = doc.nodes()[0].clone();
                node.ensure_children().nodes_mut().push(new_node);
            }
        }
    }
}

fn generate_update_children(field: &FieldInfo, access: &FieldAccessor, struct_name: &str) -> TokenStream {
    let kdl_key = &field.kdl_key;
    let children_any = field.placement.children_any;
    let filter_expr = if children_any {
        quote! { true }
    } else {
        quote! { child.base_name() == #kdl_key }
    };

    if field.is_option_vec {
        let reference = &access.reference;
        quote! {
            if let Some(values) = #reference {
                let mut indices: ::std::vec::Vec<usize> = Vec::new();
                if let Some(children) = node.children_mut() {
                    for (idx, child) in children.nodes_mut().iter_mut().enumerate() {
                        if #filter_expr {
                            indices.push(idx);
                        }
                    }
                }
                let mut idx_iter = indices.into_iter();
                for value in values {
                    if let Some(child_idx) = idx_iter.next() {
                        if let Some(children) = node.children_mut() {
                            if let Some(child) = children.nodes_mut().get_mut(child_idx) {
                                value.update(child, ctx)?;
                            }
                        }
                    } else {
                        let rendered = ::kdl_config::to_kdl(value, #kdl_key);
                        let doc: ::kdl_config::KdlDocument = rendered.parse().map_err(|e: kdl::KdlError| {
                            ::kdl_config::KdlConfigError::custom(#struct_name, e.to_string())
                        })?;
                        let new_node = doc.nodes()[0].clone();
                        node.ensure_children().nodes_mut().push(new_node);
                    }
                }
                if let Some(children) = node.children_mut() {
                    let mut remaining: ::std::vec::Vec<usize> = idx_iter.collect();
                    remaining.sort_unstable_by(|a, b| b.cmp(a));
                    for idx in remaining {
                        children.nodes_mut().remove(idx);
                    }
                }
            } else if let Some(children) = node.children_mut() {
                children.nodes_mut().retain(|child| !(#filter_expr));
            }
        }
    } else {
        let reference = &access.reference;
        quote! {
            let mut indices: ::std::vec::Vec<usize> = Vec::new();
            if let Some(children) = node.children_mut() {
                for (idx, child) in children.nodes_mut().iter_mut().enumerate() {
                    if #filter_expr {
                        indices.push(idx);
                    }
                }
            }
            let mut idx_iter = indices.into_iter();
            for value in #reference {
                if let Some(child_idx) = idx_iter.next() {
                    if let Some(children) = node.children_mut() {
                        if let Some(child) = children.nodes_mut().get_mut(child_idx) {
                            value.update(child, ctx)?;
                        }
                    }
                } else {
                    let rendered = ::kdl_config::to_kdl(value, #kdl_key);
                    let doc: ::kdl_config::KdlDocument = rendered.parse().map_err(|e: kdl::KdlError| {
                        ::kdl_config::KdlConfigError::custom(#struct_name, e.to_string())
                    })?;
                    let new_node = doc.nodes()[0].clone();
                    node.ensure_children().nodes_mut().push(new_node);
                }
            }
            if let Some(children) = node.children_mut() {
                let mut remaining: ::std::vec::Vec<usize> = idx_iter.collect();
                remaining.sort_unstable_by(|a, b| b.cmp(a));
                for idx in remaining {
                    children.nodes_mut().remove(idx);
                }
            }
        }
    }
}

fn generate_update_collection(field: &FieldInfo, access: &FieldAccessor, struct_name: &str) -> TokenStream {
    let field_name = field.ident.to_string();
    let collection = field.collection.as_ref().expect("collection spec");
    let (container, map_node, all_children) = match &collection.mode {
        CollectionMode::Registry { container } => (Some(container.clone()), None, false),
        CollectionMode::ChildrenMapNode { node } => (None, Some(node.clone()), false),
        CollectionMode::ChildrenMapAll => (None, None, true),
    };
    let container_expr = container.as_ref().map(|s| quote! { #s });
    let map_node_expr = map_node.as_ref().map(|s| quote! { #s });
    let selector = &collection.selector;
    let (key_ty, is_registry) = if matches!(collection.mode, CollectionMode::Registry { .. }) {
        (None, true)
    } else {
        let (_kind, key_ty, _val_ty) = crate::attrs::extract_children_map_types(&field.ty)
            .expect("children_map requires HashMap<K, V> or Vec<(K, V)>");
        (Some(key_ty.clone()), false)
    };
    let is_hashmap = field.is_hashmap;
    let is_option_vec = field.is_option_vec;

    let filter = if all_children {
        quote! { true }
    } else if let Some(container) = container_expr.as_ref() {
        quote! { child.base_name() == #container }
    } else if let Some(node_name) = map_node_expr.as_ref() {
        quote! { child.base_name() == #node_name }
    } else {
        quote! { true }
    };

    let key_extract = generate_collection_key_extract(selector, struct_name, &field_name, key_ty.as_ref(), is_registry);
    let key_set_child = generate_collection_key_set(selector, quote! { child });
    let key_set_new = generate_collection_key_set(selector, quote! { &mut new_node });

    let insert_render = match &collection.mode {
        CollectionMode::Registry { container } => {
            quote! {
                let rendered = ::kdl_config::to_kdl(value, #container);
            }
        }
        CollectionMode::ChildrenMapNode { node } => {
            quote! {
                let rendered = ::kdl_config::to_kdl(value, #node);
            }
        }
        CollectionMode::ChildrenMapAll => {
            quote! {
                let node_name = key.to_string();
                let rendered = ::kdl_config::to_kdl(value, &node_name);
            }
        }
    };

    let update_existing = quote! {
        if let Some(children) = node.children_mut() {
            if let Some(child) = children.nodes_mut().get_mut(idx) {
                used[idx] = true;
                #key_set_child
                value.update(child, ctx)?;
            }
        }
    };

    let insert_new = quote! {
        #insert_render
        let doc: ::kdl_config::KdlDocument = rendered.parse().map_err(|e: kdl::KdlError| {
            ::kdl_config::KdlConfigError::custom(#struct_name, e.to_string())
        })?;
        let mut new_node = doc.nodes()[0].clone();
        #key_set_new
        node.ensure_children().nodes_mut().push(new_node);
        used.push(true);
    };

    let iter_body = quote! {
        let mut found_idx: ::core::option::Option<usize> = None;
        if let Some(children) = node.children_mut() {
            for (idx, child) in children.nodes_mut().iter_mut().enumerate() {
                if used.get(idx).copied().unwrap_or(false) {
                    continue;
                }
                if !(#filter) {
                    continue;
                }
                if let Some(existing_key) = { #key_extract } {
                    if &existing_key == key {
                        found_idx = Some(idx);
                        break;
                    }
                }
            }
        }
        if let Some(idx) = found_idx {
            #update_existing
        } else {
            #insert_new
        }
    };

    let reference = &access.reference;
    let update_entries = if is_hashmap {
        quote! {
            let reference = #reference;
            for (key, value) in reference.iter() {
                #iter_body
            }
        }
    } else if is_option_vec {
        quote! {
            if let Some(entries) = #reference {
                for (key, value) in entries {
                    #iter_body
                }
            }
        }
    } else {
        quote! {
            let reference = #reference;
            for (key, value) in reference.iter() {
                #iter_body
            }
        }
    };

    quote! {
        let mut used: ::std::vec::Vec<bool> = Vec::new();
        if let Some(children) = node.children_mut() {
            used = vec![false; children.nodes().len()];
        }

        #update_entries

        if let Some(children) = node.children_mut() {
            let mut idx = 0usize;
            while idx < children.nodes().len() {
                if used.get(idx).copied().unwrap_or(false) {
                    idx += 1;
                    continue;
                }
                let remove = {
                    let child = &children.nodes()[idx];
                    #filter
                };
                if remove {
                    children.nodes_mut().remove(idx);
                } else {
                    idx += 1;
                }
            }
        }
    }
}

fn generate_collection_key_extract(
    selector: &SelectorAst,
    struct_name: &str,
    field_name: &str,
    key_ty: Option<&syn::Type>,
    is_registry: bool,
) -> TokenStream {
    let key_ty_tokens = key_ty.map(|ty| quote! { #ty });
    match selector {
        SelectorAst::Arg(index) => {
            let idx = *index as usize;
            if is_registry {
                quote! {
                    child.arg(#idx)
                        .and_then(|v| v.as_string().map(|s| s.to_string()))
                }
            } else if let Some(key_ty) = key_ty_tokens {
                quote! {
                    child.arg(#idx)
                        .and_then(|v| ::kdl_config::convert_value_checked::<#key_ty>(
                            v,
                            #struct_name,
                            #field_name,
                            #field_name,
                            ::kdl_config::Placement::Children,
                        ).ok())
                }
            } else {
                quote! { None }
            }
        }
        SelectorAst::Attr(name) => {
            if is_registry {
                quote! {
                    child
                        .attr_values(#name)
                        .and_then(|vals| if vals.len() == 1 { vals[0].as_string().map(|s| s.to_string()) } else { None })
                }
            } else if let Some(key_ty) = key_ty_tokens {
                quote! {
                    child
                        .attr_values(#name)
                        .and_then(|vals| if vals.len() == 1 {
                            ::kdl_config::convert_value_checked::<#key_ty>(
                                &vals[0],
                                #struct_name,
                                #field_name,
                                #field_name,
                                ::kdl_config::Placement::Children,
                            ).ok()
                        } else { None })
                }
            } else {
                quote! { None }
            }
        }
        SelectorAst::Name => {
            if is_registry {
                quote! { None }
            } else if let Some(key_ty) = key_ty_tokens {
                quote! {
                    ::kdl_config::convert_value_checked::<#key_ty>(
                        &::kdl_config::KdlValue::String(child.base_name().to_string()),
                        #struct_name,
                        #field_name,
                        #field_name,
                        ::kdl_config::Placement::Children,
                    ).ok()
                }
            } else {
                quote! { None }
            }
        }
        SelectorAst::Func(path) => {
            let key_fn: TokenStream = path.parse().unwrap_or_else(|_| {
                let ident = syn::Ident::new(path, proc_macro2::Span::call_site());
                quote! { #ident }
            });
            quote! { #key_fn(child).ok() }
        }
        SelectorAst::Any(selectors) => {
            let attempts: Vec<TokenStream> = selectors
                .iter()
                .map(|sel| generate_collection_key_extract(sel, struct_name, field_name, key_ty, is_registry))
                .collect();
            quote! {
                {
                    let mut result = None;
                    #(
                        if result.is_none() {
                            result = { #attempts };
                        }
                    )*
                    result
                }
            }
        }
    }
}

fn generate_collection_key_set(selector: &SelectorAst, target: TokenStream) -> TokenStream {
    match selector {
        SelectorAst::Arg(index) => {
            let idx = *index as usize;
            quote! {
                let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(key.clone()));
                ::kdl_config::update_or_insert_positional(#target, #idx, v);
            }
        }
        SelectorAst::Attr(name) => {
            quote! {
                let v = ::kdl_config::value_to_kdl(&::kdl_config::Value::from(key.clone()));
                ::kdl_config::update_or_insert_attr(#target, #name, v);
            }
        }
        SelectorAst::Name => {
            quote! {
                (#target).set_name(key.to_string());
            }
        }
        SelectorAst::Func(_) => {
            quote! {}
        }
        SelectorAst::Any(_) => {
            quote! {}
        }
    }
}

fn bool_mode_tokens(mode: BoolMode) -> TokenStream {
    match mode {
        BoolMode::PresenceAndValue => quote! { ::kdl_config::BoolMode::PresenceAndValue },
        BoolMode::ValueOnly => quote! { ::kdl_config::BoolMode::ValueOnly },
        BoolMode::PresenceOnly => quote! { ::kdl_config::BoolMode::PresenceOnly },
    }
}

fn flag_style_tokens(style: FlagStyle) -> TokenStream {
    match style {
        FlagStyle::Both => quote! { ::kdl_config::FlagStyle::Both },
        FlagStyle::ValueNo => quote! { ::kdl_config::FlagStyle::ValueNo },
        FlagStyle::WithWithout => quote! { ::kdl_config::FlagStyle::WithWithout },
    }
}
