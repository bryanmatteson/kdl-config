use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

use crate::attrs::{BoolMode, FieldInfo, FlagStyle, RenderPlacement, StructAttrs};

#[derive(Debug, Clone, Copy)]
enum FieldKind {
    ValueScalar,
    ValueVec,
    Node,
    NodeVec,
    Registry,
    ChildrenMap,
    Modifier,
}

pub(crate) struct FieldAccessor {
    value: TokenStream,
    reference: TokenStream,
    bool_value: TokenStream,
}

impl FieldAccessor {
    pub(crate) fn for_self(field: &FieldInfo) -> Self {
        let ident = &field.ident;
        Self {
            value: quote! { self.#ident },
            reference: quote! { &self.#ident },
            bool_value: quote! { self.#ident },
        }
    }

    pub(crate) fn binding(field: &FieldInfo) -> Self {
        let ident = &field.ident;
        Self {
            value: quote! { #ident },
            reference: quote! { #ident },
            bool_value: quote! { *#ident },
        }
    }
}

pub fn generate_render_impl(
    struct_name: &Ident,
    struct_attrs: &StructAttrs,
    fields: &[FieldInfo],
) -> TokenStream {
    let render_body = render_body_with_accessor(
        struct_attrs,
        fields,
        quote! { name.to_string() },
        None,
        FieldAccessor::for_self,
    );

    quote! {
        impl ::kdl_config::KdlRender for #struct_name {
            fn render<W: ::std::fmt::Write>(&self, w: &mut W, name: &str, indent: usize) -> ::std::fmt::Result {
                let rendered = #render_body;
                ::kdl_config::write_indent(w, indent)?;
                w.write_str(&rendered)?;
                Ok(())
            }
        }
    }
}

pub(crate) fn render_body_with_accessor(
    struct_attrs: &StructAttrs,
    fields: &[FieldInfo],
    name_expr: TokenStream,
    modifier_expr: Option<TokenStream>,
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut positional_fields = Vec::new();
    let mut keyed_fields = Vec::new();
    let mut flag_fields = Vec::new();
    let mut value_fields = Vec::new();
    let mut child_fields = Vec::new();
    let mut children_fields = Vec::new();
    let mut registry_fields = Vec::new();
    let mut children_map_fields = Vec::new();

    for field in fields {
        if field.is_modifier || field.is_skipped {
            continue;
        }
        if field.children_map {
            children_map_fields.push(field);
            continue;
        }
        let kind = field_kind(field);
        let render_placement = render_placement_for(field, kind);

        match render_placement {
            RenderPlacement::Attr => {
                if field.placement.positional.is_some() {
                    positional_fields.push(field);
                } else if field.is_bool {
                    flag_fields.push(field);
                } else {
                    keyed_fields.push(field);
                }
            }
            RenderPlacement::Value => value_fields.push(field),
            RenderPlacement::Child => child_fields.push(field),
            RenderPlacement::Children => children_fields.push(field),
            RenderPlacement::Registry => registry_fields.push(field),
        }
    }

    let positional_render = render_positional_fields(&positional_fields, accessor);
    let keyed_render = render_keyed_fields(&keyed_fields, accessor);
    let flag_render = render_flag_fields(&flag_fields, struct_attrs, accessor);
    let value_render = render_value_fields(&value_fields, struct_attrs, accessor);
    let child_render = render_child_fields(&child_fields, accessor);
    let children_render = render_children_fields(&children_fields, accessor);
    let registry_render = render_registry_fields(&registry_fields, accessor);
    let children_map_render = render_children_map_fields(&children_map_fields, accessor);
    let modifier_expr = modifier_expr.unwrap_or_else(|| modifier_expr_for(fields, accessor));

    quote! {
        {
            let mut renderer = ::kdl_config::NodeRenderer::new(#name_expr, #modifier_expr);

            #positional_render
            #flag_render
            #keyed_render

            let mut value_nodes: ::std::vec::Vec<(String, usize, String)> = ::std::vec::Vec::new();
            let mut child_nodes: ::std::vec::Vec<(String, usize, String)> = ::std::vec::Vec::new();
            let mut idx: usize = 0;

            #value_render
            #child_render
            #children_render
            #children_map_render
            #registry_render

            value_nodes.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));
            for (_, _, rendered) in value_nodes {
                renderer.child(rendered);
            }

            child_nodes.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));
            for (_, _, rendered) in child_nodes {
                renderer.child(rendered);
            }

            renderer.render()
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
    if field.children_map {
        return FieldKind::ChildrenMap;
    }

    if field.is_vec || field.is_option_vec {
        let inner = if field.is_option_vec {
            field.inner_type().and_then(crate::attrs::extract_inner_type)
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
    if crate::attrs::is_bool_type(ty) || crate::attrs::is_string_type(ty) || crate::attrs::is_numeric_type(ty) {
        return true;
    }

    if crate::attrs::is_option_type(ty) {
        if let Some(inner) = crate::attrs::extract_inner_type(ty) {
            return is_value_type(inner);
        }
    }

    false
}

fn render_placement_for(field: &FieldInfo, kind: FieldKind) -> RenderPlacement {
    if let Some(render) = field.render {
        return render;
    }

    match kind {
        FieldKind::ValueScalar => RenderPlacement::Attr,
        FieldKind::ValueVec => RenderPlacement::Value,
        FieldKind::Node => RenderPlacement::Child,
        FieldKind::NodeVec => RenderPlacement::Children,
        FieldKind::Registry => RenderPlacement::Registry,
        FieldKind::ChildrenMap => RenderPlacement::Children,
        FieldKind::Modifier => RenderPlacement::Child,
    }
}

fn modifier_expr_for(fields: &[FieldInfo], accessor: fn(&FieldInfo) -> FieldAccessor) -> TokenStream {
    if let Some(field) = fields
        .iter()
        .find(|field| field.is_modifier && !field.is_skipped)
    {
        let access = accessor(field);
        if field.is_optional {
            let reference = &access.reference;
            quote! { (#reference).as_ref().cloned().unwrap_or(::kdl_config::Modifier::Inherit) }
        } else {
            let value = &access.value;
            quote! { #value }
        }
    } else {
        quote! { ::kdl_config::Modifier::Inherit }
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

fn render_positional_fields(fields: &[&FieldInfo], accessor: fn(&FieldInfo) -> FieldAccessor) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let idx = field.placement.positional.expect("positional field");
        let cond = render_condition(field, &access);

        let render_body = if field.is_optional {
            let reference = &access.reference;
            quote! {
                if let Some(value) = #reference {
                    renderer.positional_raw(#idx, ::kdl_config::render_value(&::kdl_config::Value::from(value.clone())));
                }
            }
        } else {
            let value = &access.value;
            quote! {
                renderer.positional_raw(#idx, ::kdl_config::render_value(&::kdl_config::Value::from(#value.clone())));
            }
        };

        items.push(quote! {
            if #cond {
                #render_body
            }
        });
    }
    quote! { #(#items)* }
}

fn render_keyed_fields(fields: &[&FieldInfo], accessor: fn(&FieldInfo) -> FieldAccessor) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);

        let render_body = if field.is_vec || field.is_option_vec {
            if field.is_option_vec {
                let reference = &access.reference;
                quote! {
                    if let Some(values) = #reference {
                        for value in values {
                            renderer.keyed_raw(#key, ::kdl_config::render_value(&::kdl_config::Value::from(value.clone())));
                        }
                    }
                }
            } else {
                let reference = &access.reference;
                quote! {
                    for value in #reference {
                        renderer.keyed_raw(#key, ::kdl_config::render_value(&::kdl_config::Value::from(value.clone())));
                    }
                }
            }
        } else if field.is_optional {
            let reference = &access.reference;
            quote! {
                if let Some(value) = #reference {
                    renderer.keyed_raw(#key, ::kdl_config::render_value(&::kdl_config::Value::from(value.clone())));
                }
            }
        } else {
            let value = &access.value;
            quote! {
                renderer.keyed_raw(#key, ::kdl_config::render_value(&::kdl_config::Value::from(#value.clone())));
            }
        };

        items.push(quote! {
            if #cond {
                #render_body
            }
        });
    }
    quote! { #(#items)* }
}

fn render_flag_fields(fields: &[&FieldInfo], struct_attrs: &StructAttrs, accessor: fn(&FieldInfo) -> FieldAccessor) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);

        let bool_mode = field
            .bool_mode
            .clone()
            .unwrap_or(struct_attrs.default_bool.clone().unwrap_or(BoolMode::PresenceAndValue));
        let bool_mode_tokens = bool_mode_tokens(bool_mode);
        let flag_style = field
            .flag_style
            .clone()
            .unwrap_or(struct_attrs.default_flag_style.clone().unwrap_or(FlagStyle::Both));

        let (pos_flag, neg_flag) = match &field.placement.flag {
            Some((Some(pos), Some(neg))) => (pos.clone(), neg.clone()),
            _ => match flag_style {
                FlagStyle::ValueNo | FlagStyle::Both => (key.to_string(), format!("no-{}", key)),
                FlagStyle::WithWithout => (format!("with-{}", key), format!("without-{}", key)),
            },
        };

        let pos_flag_lit = pos_flag;
        let neg_flag_lit = neg_flag;

        let render_body = if field.is_optional {
            let reference = &access.reference;
            quote! {
                if let Some(value) = #reference {
                    match #bool_mode_tokens {
                        ::kdl_config::BoolMode::ValueOnly => {
                            renderer.keyed_raw(#key, ::kdl_config::render_value(&::kdl_config::Value::Bool(*value)));
                        }
                        ::kdl_config::BoolMode::PresenceOnly => {
                            if *value {
                                renderer.flag(#pos_flag_lit);
                            }
                        }
                        ::kdl_config::BoolMode::PresenceAndValue => {
                            if *value {
                                renderer.flag(#pos_flag_lit);
                            } else {
                                renderer.flag(#neg_flag_lit);
                            }
                        }
                    }
                }
            }
        } else {
            let bool_value = &access.bool_value;
            quote! {
                match #bool_mode_tokens {
                    ::kdl_config::BoolMode::ValueOnly => {
                        renderer.keyed_raw(#key, ::kdl_config::render_value(&::kdl_config::Value::Bool(#bool_value)));
                    }
                    ::kdl_config::BoolMode::PresenceOnly => {
                        if #bool_value {
                            renderer.flag(#pos_flag_lit);
                        }
                    }
                    ::kdl_config::BoolMode::PresenceAndValue => {
                        if #bool_value {
                            renderer.flag(#pos_flag_lit);
                        } else {
                            renderer.flag(#neg_flag_lit);
                        }
                    }
                }
            }
        };

        items.push(quote! {
            if #cond {
                #render_body
            }
        });
    }
    quote! { #(#items)* }
}

fn render_value_fields(fields: &[&FieldInfo], struct_attrs: &StructAttrs, accessor: fn(&FieldInfo) -> FieldAccessor) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);

        let bool_mode = field
            .bool_mode
            .clone()
            .unwrap_or(struct_attrs.default_bool.clone().unwrap_or(BoolMode::PresenceAndValue));
        let bool_mode_tokens = bool_mode_tokens(bool_mode);

        let render_body = if field.is_vec || field.is_option_vec {
            if field.is_option_vec {
                let reference = &access.reference;
                quote! {
                    if let Some(values) = #reference {
                        if !values.is_empty() {
                            let rendered = ::kdl_config::render_value_node(#key, &values.iter().cloned().map(::kdl_config::Value::from).collect::<::std::vec::Vec<_>>());
                            value_nodes.push((#key.to_string(), idx, rendered));
                            idx += 1;
                        }
                    }
                }
            } else {
                let reference = &access.reference;
                quote! {
                    if !(#reference).is_empty() {
                        let rendered = ::kdl_config::render_value_node(#key, &(#reference).iter().cloned().map(::kdl_config::Value::from).collect::<::std::vec::Vec<_>>());
                        value_nodes.push((#key.to_string(), idx, rendered));
                        idx += 1;
                    }
                }
            }
        } else if field.is_bool {
            if field.is_optional {
                let reference = &access.reference;
                quote! {
                    if let Some(value) = #reference {
                        match #bool_mode_tokens {
                            ::kdl_config::BoolMode::PresenceOnly => {
                                if *value {
                                    let rendered = ::kdl_config::render_key(#key);
                                    value_nodes.push((#key.to_string(), idx, rendered));
                                    idx += 1;
                                }
                            }
                            _ => {
                                let rendered = ::kdl_config::render_value_node_scalar(#key, &::kdl_config::Value::Bool(*value));
                                value_nodes.push((#key.to_string(), idx, rendered));
                                idx += 1;
                            }
                        }
                    }
                }
            } else {
                let bool_value = &access.bool_value;
                quote! {
                    match #bool_mode_tokens {
                        ::kdl_config::BoolMode::PresenceOnly => {
                            if #bool_value {
                                let rendered = ::kdl_config::render_key(#key);
                                value_nodes.push((#key.to_string(), idx, rendered));
                                idx += 1;
                            }
                        }
                        _ => {
                            let rendered = ::kdl_config::render_value_node_scalar(#key, &::kdl_config::Value::Bool(#bool_value));
                            value_nodes.push((#key.to_string(), idx, rendered));
                            idx += 1;
                        }
                    }
                }
            }
        } else if field.is_optional {
            let reference = &access.reference;
            quote! {
                if let Some(value) = #reference {
                    let rendered = ::kdl_config::render_value_node_scalar(#key, &::kdl_config::Value::from(value.clone()));
                    value_nodes.push((#key.to_string(), idx, rendered));
                    idx += 1;
                }
            }
        } else {
            let value = &access.value;
            quote! {
                let rendered = ::kdl_config::render_value_node_scalar(#key, &::kdl_config::Value::from(#value.clone()));
                value_nodes.push((#key.to_string(), idx, rendered));
                idx += 1;
            }
        };

        items.push(quote! {
            if #cond {
                #render_body
            }
        });
    }
    quote! { #(#items)* }
}

fn render_child_fields(fields: &[&FieldInfo], accessor: fn(&FieldInfo) -> FieldAccessor) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);

        let render_body = if field.is_optional {
            let reference = &access.reference;
            quote! {
                if let Some(value) = #reference {
                    let rendered = ::kdl_config::to_kdl(value, #key);
                    child_nodes.push((#key.to_string(), idx, rendered));
                    idx += 1;
                }
            }
        } else {
            let reference = &access.reference;
            quote! {
                let rendered = ::kdl_config::to_kdl(#reference, #key);
                child_nodes.push((#key.to_string(), idx, rendered));
                idx += 1;
            }
        };

        items.push(quote! {
            if #cond {
                #render_body
            }
        });
    }
    quote! { #(#items)* }
}

fn render_children_fields(fields: &[&FieldInfo], accessor: fn(&FieldInfo) -> FieldAccessor) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);

        let render_body = if field.is_option_vec {
            let reference = &access.reference;
            quote! {
                if let Some(values) = #reference {
                    for child in values {
                        let rendered = ::kdl_config::to_kdl(child, #key);
                        child_nodes.push((#key.to_string(), idx, rendered));
                        idx += 1;
                    }
                }
            }
        } else {
            let reference = &access.reference;
            quote! {
                for child in #reference {
                    let rendered = ::kdl_config::to_kdl(child, #key);
                    child_nodes.push((#key.to_string(), idx, rendered));
                    idx += 1;
                }
            }
        };

        items.push(quote! {
            if #cond {
                #render_body
            }
        });
    }
    quote! { #(#items)* }
}

fn render_registry_fields(fields: &[&FieldInfo], accessor: fn(&FieldInfo) -> FieldAccessor) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let container = field.container.clone().unwrap_or_else(|| field.kdl_key.clone());
        let cond = render_condition(field, &access);
        let reference = &access.reference;
        let registry_vec = crate::attrs::extract_registry_vec_value(&field.ty);

        if let Some((_val_ty, is_option_vec)) = registry_vec {
            if is_option_vec {
                items.push(quote! {
                    if #cond {
                        if let Some(entries) = #reference {
                            for (name, value) in entries {
                                let mut rendered = ::kdl_config::to_kdl(value, #container);
                                let key_rendered = ::kdl_config::render_value(&::kdl_config::Value::String(name.clone()));
                                rendered = ::kdl_config::insert_arg(&rendered, &key_rendered);
                                child_nodes.push((#container.to_string(), idx, rendered));
                                idx += 1;
                            }
                        }
                    }
                });
            } else {
                items.push(quote! {
                    if #cond {
                        for (name, value) in #reference {
                            let mut rendered = ::kdl_config::to_kdl(value, #container);
                            let key_rendered = ::kdl_config::render_value(&::kdl_config::Value::String(name.clone()));
                            rendered = ::kdl_config::insert_arg(&rendered, &key_rendered);
                            child_nodes.push((#container.to_string(), idx, rendered));
                            idx += 1;
                        }
                    }
                });
            }
        } else {
            items.push(quote! {
                if #cond {
                    let mut entries: ::std::vec::Vec<(&String, &_)> = (#reference).iter().collect();
                    entries.sort_by(|a, b| a.0.cmp(b.0));
                    for (name, value) in entries {
                        let mut rendered = ::kdl_config::to_kdl(value, #container);
                        let key_rendered = ::kdl_config::render_value(&::kdl_config::Value::String(name.clone()));
                        rendered = ::kdl_config::insert_arg(&rendered, &key_rendered);
                        child_nodes.push((#container.to_string(), idx, rendered));
                        idx += 1;
                    }
                }
            });
        }
    }
    quote! { #(#items)* }
}

fn render_children_map_fields(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let cond = render_condition(field, &access);
        let reference = &access.reference;
        let map_node = field.map_node.clone();
        let map_kind = field.children_map_kind.expect("children_map field kind");

        match (map_node, map_kind) {
            (Some(map_node), crate::attrs::ChildrenMapKind::HashMap) => {
                items.push(quote! {
                    if #cond {
                        let mut entries: ::std::vec::Vec<(String, &_, &_)> = (#reference)
                            .iter()
                            .map(|(name, value)| {
                                let rendered = ::kdl_config::render_value(
                                    &::kdl_config::Value::from((*name).clone()),
                                );
                                (rendered, name, value)
                            })
                            .collect();
                        entries.sort_by(|a, b| a.0.cmp(&b.0));
                        for (rendered_key, _name, value) in entries {
                            let mut rendered = ::kdl_config::to_kdl(value, #map_node);
                            rendered = ::kdl_config::insert_arg(&rendered, &rendered_key);
                            child_nodes.push((#map_node.to_string(), idx, rendered));
                            idx += 1;
                        }
                    }
                });
            }
            (Some(map_node), crate::attrs::ChildrenMapKind::Vec) => {
                items.push(quote! {
                    if #cond {
                        for (name, value) in #reference {
                            let rendered_key = ::kdl_config::render_value(
                                &::kdl_config::Value::from((*name).clone()),
                            );
                            let mut rendered = ::kdl_config::to_kdl(value, #map_node);
                            rendered = ::kdl_config::insert_arg(&rendered, &rendered_key);
                            child_nodes.push((#map_node.to_string(), idx, rendered));
                            idx += 1;
                        }
                    }
                });
            }
            (Some(map_node), crate::attrs::ChildrenMapKind::OptionVec) => {
                items.push(quote! {
                    if #cond {
                        if let Some(entries) = #reference {
                            for (name, value) in entries {
                                let rendered_key = ::kdl_config::render_value(
                                    &::kdl_config::Value::from((*name).clone()),
                                );
                                let mut rendered = ::kdl_config::to_kdl(value, #map_node);
                                rendered = ::kdl_config::insert_arg(&rendered, &rendered_key);
                                child_nodes.push((#map_node.to_string(), idx, rendered));
                                idx += 1;
                            }
                        }
                    }
                });
            }
            (None, crate::attrs::ChildrenMapKind::HashMap) => {
                items.push(quote! {
                    if #cond {
                        let mut entries: ::std::vec::Vec<(String, &_, &_)> = (#reference)
                            .iter()
                            .map(|(name, value)| (name.to_string(), name, value))
                            .collect();
                        entries.sort_by(|a, b| a.0.cmp(&b.0));
                        for (rendered_key, _name, value) in entries {
                            let rendered = ::kdl_config::to_kdl(value, &rendered_key);
                            child_nodes.push((rendered_key, idx, rendered));
                            idx += 1;
                        }
                    }
                });
            }
            (None, crate::attrs::ChildrenMapKind::Vec) => {
                items.push(quote! {
                    if #cond {
                        for (name, value) in #reference {
                            let rendered_key = name.to_string();
                            let rendered = ::kdl_config::to_kdl(value, &rendered_key);
                            child_nodes.push((rendered_key, idx, rendered));
                            idx += 1;
                        }
                    }
                });
            }
            (None, crate::attrs::ChildrenMapKind::OptionVec) => {
                items.push(quote! {
                    if #cond {
                        if let Some(entries) = #reference {
                            for (name, value) in entries {
                                let rendered_key = name.to_string();
                                let rendered = ::kdl_config::to_kdl(value, &rendered_key);
                                child_nodes.push((rendered_key, idx, rendered));
                                idx += 1;
                            }
                        }
                    }
                });
            }
        }
    }
    quote! { #(#items)* }
}

fn bool_mode_tokens(mode: BoolMode) -> TokenStream {
    match mode {
        BoolMode::PresenceAndValue => quote! { ::kdl_config::BoolMode::PresenceAndValue },
        BoolMode::ValueOnly => quote! { ::kdl_config::BoolMode::ValueOnly },
        BoolMode::PresenceOnly => quote! { ::kdl_config::BoolMode::PresenceOnly },
    }
}
