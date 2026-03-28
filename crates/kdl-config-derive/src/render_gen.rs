use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

use crate::attrs::{
    BoolMode, CollectionMode, DefaultPlacement, FieldInfo, FieldKind, FlagStyle, RenderPlacement,
    StructAttrs, field_kind,
};

/// Generate a `Value::from(...)` expression, optionally converting through an intermediate type.
fn value_from_expr(field: &FieldInfo, value_expr: TokenStream) -> TokenStream {
    if let Some(from_ty) = &field.from_type {
        quote! { ::kdl_config::Value::from(<#from_ty>::from(#value_expr)) }
    } else {
        quote! { ::kdl_config::Value::from(#value_expr) }
    }
}

pub(crate) struct FieldAccessor {
    pub(crate) value: TokenStream,
    pub(crate) reference: TokenStream,
    pub(crate) bool_value: TokenStream,
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
    let render_node_body = render_node_body_with_accessor(
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

            fn render_node(&self, name: &str) -> ::kdl_config::Node {
                #render_node_body
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
    let ordered_render = render_fields_in_order(struct_attrs, fields, accessor);
    let modifier_expr = modifier_expr.unwrap_or_else(|| modifier_expr_for(fields, accessor));

    quote! {
        {
            let mut renderer = ::kdl_config::NodeRenderer::new(#name_expr, #modifier_expr);

            let mut value_nodes: ::std::vec::Vec<(String, usize, String)> = ::std::vec::Vec::new();
            let mut child_nodes: ::std::vec::Vec<(String, usize, String)> = ::std::vec::Vec::new();
            let mut idx: usize = 0;

            #ordered_render

            value_nodes.sort_by(|(a_name, a_idx, _), (b_name, b_idx, _)| {
                a_name.cmp(b_name).then(a_idx.cmp(b_idx))
            });
            for (_, _, rendered) in value_nodes {
                renderer.child(rendered);
            }

            child_nodes.sort_by(|(a_name, a_idx, _), (b_name, b_idx, _)| {
                a_name.cmp(b_name).then(a_idx.cmp(b_idx))
            });
            for (_, _, rendered) in child_nodes {
                renderer.child(rendered);
            }

            renderer.render()
        }
    }
}

pub(crate) fn render_node_body_with_accessor(
    struct_attrs: &StructAttrs,
    fields: &[FieldInfo],
    name_expr: TokenStream,
    modifier_expr: Option<TokenStream>,
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let ordered_render = render_fields_in_order_node(struct_attrs, fields, accessor);
    let modifier_expr = modifier_expr.unwrap_or_else(|| modifier_expr_for(fields, accessor));

    quote! {
        let mut node = ::kdl_config::Node::named(#name_expr);
        node.modifier = #modifier_expr;

        let mut positional_args: ::std::vec::Vec<(usize, ::kdl_config::Value, ::core::option::Option<String>)> = ::std::vec::Vec::new();
        let mut flag_args: ::std::vec::Vec<String> = ::std::vec::Vec::new();
        let mut value_nodes: ::std::vec::Vec<(String, usize, ::kdl_config::Node)> = ::std::vec::Vec::new();
        let mut child_nodes: ::std::vec::Vec<(String, usize, ::kdl_config::Node)> = ::std::vec::Vec::new();
        let mut idx: usize = 0;

        #ordered_render

        positional_args.sort_by_key(|(idx, _, _)| *idx);
        for (_, value, repr) in positional_args {
            node.add_arg_with_repr(value, repr);
        }

        for flag in flag_args {
            let repr = ::kdl_config::render_key(&flag);
            node.add_arg_with_repr(::kdl_config::Value::String(flag), Some(repr));
        }

        value_nodes.sort_by(|(a_name, a_idx, _), (b_name, b_idx, _)| {
            a_name.cmp(b_name).then(a_idx.cmp(b_idx))
        });
        child_nodes.sort_by(|(a_name, a_idx, _), (b_name, b_idx, _)| {
            a_name.cmp(b_name).then(a_idx.cmp(b_idx))
        });
        for (_, _, child) in value_nodes {
            node.add_child(child);
        }
        for (_, _, child) in child_nodes {
            node.add_child(child);
        }

        node
    }
}

fn render_fields_in_order(
    struct_attrs: &StructAttrs,
    fields: &[FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();

    for field in fields {
        if field.is_modifier || field.is_skipped {
            continue;
        }

        let render = render_field_in_order(struct_attrs, field, accessor);
        items.push(render);
    }

    quote! { #(#items)* }
}

fn render_fields_in_order_node(
    struct_attrs: &StructAttrs,
    fields: &[FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();

    for field in fields {
        if field.is_modifier || field.is_skipped {
            continue;
        }

        let render = render_field_in_order_node(struct_attrs, field, accessor);
        items.push(render);
    }

    quote! { #(#items)* }
}

fn render_field_in_order(
    struct_attrs: &StructAttrs,
    field: &FieldInfo,
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    if field.flatten {
        return render_flatten_fields(&[field], accessor);
    }

    if let Some(collection) = field.collection.as_ref()
        && matches!(
            collection.mode,
            CollectionMode::ChildrenMapAll | CollectionMode::ChildrenMapNode { .. }
        )
    {
        return render_children_map_fields(&[field], accessor);
    }

    let kind = field_kind(field);
    match render_placement_for(struct_attrs, field, kind) {
        RenderPlacement::Attr => {
            if field.placement.positional_list {
                render_positional_list_fields(&[field], accessor)
            } else if field.placement.positional.is_some() {
                render_positional_fields(&[field], accessor)
            } else if field.placement.flag.is_some() || field.is_bool {
                render_flag_fields(&[field], struct_attrs, accessor)
            } else {
                render_keyed_fields(&[field], accessor)
            }
        }
        RenderPlacement::Value => {
            if is_presence_only_bool(field, struct_attrs) {
                render_flag_fields(&[field], struct_attrs, accessor)
            } else {
                render_value_fields(&[field], struct_attrs, accessor)
            }
        }
        RenderPlacement::Child => render_child_fields(&[field], accessor),
        RenderPlacement::Children => render_children_fields(&[field], accessor),
        RenderPlacement::Registry => render_registry_fields(&[field], accessor),
    }
}

fn render_field_in_order_node(
    struct_attrs: &StructAttrs,
    field: &FieldInfo,
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    if field.flatten {
        return render_flatten_fields_node(&[field], accessor);
    }

    if let Some(collection) = field.collection.as_ref()
        && matches!(
            collection.mode,
            CollectionMode::ChildrenMapAll | CollectionMode::ChildrenMapNode { .. }
        )
    {
        return render_children_map_fields_node(&[field], accessor);
    }

    let kind = field_kind(field);
    match render_placement_for(struct_attrs, field, kind) {
        RenderPlacement::Attr => {
            if field.placement.positional_list {
                render_positional_list_fields_node(&[field], accessor)
            } else if field.placement.positional.is_some() {
                render_positional_fields_node(&[field], accessor)
            } else if field.placement.flag.is_some() || field.is_bool {
                render_flag_fields_node(&[field], struct_attrs, accessor)
            } else {
                render_keyed_fields_node(&[field], accessor)
            }
        }
        RenderPlacement::Value => {
            if is_presence_only_bool(field, struct_attrs) {
                render_flag_fields_node(&[field], struct_attrs, accessor)
            } else {
                render_value_fields_node(&[field], struct_attrs, accessor)
            }
        }
        RenderPlacement::Child => render_child_fields_node(&[field], accessor),
        RenderPlacement::Children => render_children_fields_node(&[field], accessor),
        RenderPlacement::Registry => render_registry_fields_node(&[field], accessor),
    }
}

fn is_presence_only_bool(field: &FieldInfo, struct_attrs: &StructAttrs) -> bool {
    field.is_bool
        && matches!(
            field.bool_mode.as_ref().unwrap_or(
                &struct_attrs
                    .default_bool
                    .clone()
                    .unwrap_or(BoolMode::PresenceAndValue)
            ),
            BoolMode::PresenceOnly
        )
}

/// Determine the render placement for a field.
///
/// NOTE: This intentionally differs from `update_gen::render_placement_for` in
/// collection handling. Render dispatches on `CollectionMode` (Registry vs
/// ChildrenMap → Children) to produce the correct output shape, while update
/// always returns `Registry` because it patches collections via registry
/// semantics regardless of the underlying mode.
fn render_placement_for(
    struct_attrs: &StructAttrs,
    field: &FieldInfo,
    kind: FieldKind,
) -> RenderPlacement {
    if let Some(render) = field.render {
        return render;
    }

    if let Some(collection) = field.collection.as_ref() {
        return match collection.mode {
            CollectionMode::Registry { .. } => RenderPlacement::Registry,
            CollectionMode::ChildrenMapAll | CollectionMode::ChildrenMapNode { .. } => {
                RenderPlacement::Children
            }
        };
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
    // For scalar fields with both attr and value, prefer attr for compact rendering
    // For Vec fields, prefer value to avoid repeated attrs with same key
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
        // For scalar values, always render as attrs for compact inline format
        // The default_placement primarily affects parsing, not rendering
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

fn modifier_expr_for(
    fields: &[FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
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
        return quote! { !(#path(#reference)) };
    }

    let reference = &accessor.reference;
    let mut checks = Vec::<TokenStream>::new();

    if field.skip_serialize_none {
        checks.push(quote! { (#reference).is_some() });
    }

    if field.skip_serialize_empty_collections {
        if field.is_option_vec {
            checks.push(quote! { (#reference).as_ref().map_or(true, |values| !values.is_empty()) });
        } else if field.is_vec || field.is_hashmap || field.is_btreemap {
            checks.push(quote! { !(#reference).is_empty() });
        }
    }

    if checks.is_empty() {
        quote! { true }
    } else {
        quote! { #(#checks)&&* }
    }
}

fn render_positional_fields(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let idx = field.placement.positional.expect("positional field");
        let cond = render_condition(field, &access);

        let render_body = if field.is_optional {
            let reference = &access.reference;
            let val_expr = value_from_expr(field, quote! { value.clone() });
            quote! {
                if let Some(value) = #reference {
                    renderer.positional_raw(#idx, ::kdl_config::render_value(&#val_expr));
                }
            }
        } else {
            let value = &access.value;
            let val_expr = value_from_expr(field, quote! { #value.clone() });
            quote! {
                renderer.positional_raw(#idx, ::kdl_config::render_value(&#val_expr));
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

fn render_positional_list_fields(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let cond = render_condition(field, &access);

        let render_body = if field.is_option_vec {
            let reference = &access.reference;
            let val_expr = value_from_expr(field, quote! { value.clone() });
            quote! {
                if let Some(values) = #reference {
                    for (idx, value) in values.iter().enumerate() {
                        renderer.positional_raw(idx, ::kdl_config::render_value(&#val_expr));
                    }
                }
            }
        } else {
            let reference = &access.reference;
            let val_expr = value_from_expr(field, quote! { value.clone() });
            quote! {
                for (idx, value) in (#reference).iter().enumerate() {
                    renderer.positional_raw(idx, ::kdl_config::render_value(&#val_expr));
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

fn render_keyed_fields(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);

        let val_expr = value_from_expr(field, quote! { value.clone() });
        let render_body = if field.is_vec || field.is_option_vec {
            if field.is_option_vec {
                let reference = &access.reference;
                quote! {
                    if let Some(values) = #reference {
                        for value in values {
                            renderer.keyed_raw(#key, ::kdl_config::render_value(&#val_expr));
                        }
                    }
                }
            } else {
                let reference = &access.reference;
                quote! {
                    for value in #reference {
                        renderer.keyed_raw(#key, ::kdl_config::render_value(&#val_expr));
                    }
                }
            }
        } else if field.is_optional {
            let reference = &access.reference;
            quote! {
                if let Some(value) = #reference {
                    renderer.keyed_raw(#key, ::kdl_config::render_value(&#val_expr));
                }
            }
        } else {
            let value = &access.value;
            let owned_val_expr = value_from_expr(field, quote! { #value.clone() });
            quote! {
                renderer.keyed_raw(#key, ::kdl_config::render_value(&#owned_val_expr));
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

fn render_flag_fields(
    fields: &[&FieldInfo],
    struct_attrs: &StructAttrs,
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);

        let bool_mode = field.bool_mode.clone().unwrap_or(
            struct_attrs
                .default_bool
                .clone()
                .unwrap_or(BoolMode::PresenceAndValue),
        );
        let bool_mode_tokens = bool_mode_tokens(bool_mode);
        let flag_style = field.flag_style.clone().unwrap_or(
            struct_attrs
                .default_flag_style
                .clone()
                .unwrap_or(FlagStyle::Both),
        );

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

fn render_value_fields(
    fields: &[&FieldInfo],
    struct_attrs: &StructAttrs,
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);

        let bool_mode = field.bool_mode.clone().unwrap_or(
            struct_attrs
                .default_bool
                .clone()
                .unwrap_or(BoolMode::PresenceAndValue),
        );
        let bool_mode_tokens = bool_mode_tokens(bool_mode);

        let render_body = if field.is_vec || field.is_option_vec {
            let map_fn = if field.from_type.is_some() {
                let from_ty = field.from_type.as_ref().unwrap();
                quote! { |v| ::kdl_config::Value::from(<#from_ty>::from(v)) }
            } else {
                quote! { ::kdl_config::Value::from }
            };
            if field.is_option_vec {
                let reference = &access.reference;
                quote! {
                    if let Some(values) = #reference {
                        if !values.is_empty() {
                            let rendered = ::kdl_config::render_value_node(#key, &values.iter().cloned().map(#map_fn).collect::<::std::vec::Vec<_>>());
                            value_nodes.push((#key.to_string(), idx, rendered));
                            idx += 1;
                        }
                    }
                }
            } else {
                let reference = &access.reference;
                quote! {
                    if !(#reference).is_empty() {
                        let rendered = ::kdl_config::render_value_node(#key, &(#reference).iter().cloned().map(#map_fn).collect::<::std::vec::Vec<_>>());
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
            let val_expr = value_from_expr(field, quote! { value.clone() });
            quote! {
                if let Some(value) = #reference {
                    let rendered = ::kdl_config::render_value_node_scalar(#key, &#val_expr);
                    value_nodes.push((#key.to_string(), idx, rendered));
                    idx += 1;
                }
            }
        } else {
            let value = &access.value;
            let val_expr = value_from_expr(field, quote! { #value.clone() });
            quote! {
                let rendered = ::kdl_config::render_value_node_scalar(#key, &#val_expr);
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

fn render_child_fields(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
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

fn render_children_fields(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
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

fn render_registry_fields(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let container = field
            .collection
            .as_ref()
            .and_then(|spec| match &spec.mode {
                CollectionMode::Registry { container } => Some(container.clone()),
                _ => None,
            })
            .unwrap_or_else(|| field.kdl_key.clone());
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
            (Some(map_node), crate::attrs::ChildrenMapKind::BTreeMap) => {
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
            (None, crate::attrs::ChildrenMapKind::BTreeMap) => {
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

fn render_flatten_fields(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);
        let render_body = if field.is_optional {
            let reference = &access.reference;
            quote! {
                if let Some(value) = #reference {
                    let flattened = ::kdl_config::render_flatten(value, #key);
                    let pos_offset = renderer.next_positional_index();
                    for (offset, arg) in flattened.args().iter().enumerate() {
                        let idx = pos_offset + offset;
                        if let Some(repr) = flattened.arg_repr(offset) {
                            renderer.positional_raw(idx, repr.to_string());
                        } else {
                            renderer.positional(idx, arg);
                        }
                    }
                    for (attr_key, values) in flattened.attrs() {
                        let key_repr = flattened.attr_repr(attr_key);
                        for value in values {
                            let rendered = ::kdl_config::render_value(value);
                            renderer.keyed_raw_with_repr(attr_key, key_repr, rendered);
                        }
                    }
                    for child in flattened.children() {
                        let rendered = ::kdl_config::render_node(child);
                        child_nodes.push((child.name.clone(), idx, rendered));
                        idx += 1;
                    }
                }
            }
        } else {
            let reference = &access.reference;
            quote! {
                let flattened = ::kdl_config::render_flatten(#reference, #key);
                let pos_offset = renderer.next_positional_index();
                for (offset, arg) in flattened.args().iter().enumerate() {
                    let idx = pos_offset + offset;
                    if let Some(repr) = flattened.arg_repr(offset) {
                        renderer.positional_raw(idx, repr.to_string());
                    } else {
                        renderer.positional(idx, arg);
                    }
                }
                for (attr_key, values) in flattened.attrs() {
                    let key_repr = flattened.attr_repr(attr_key);
                    for value in values {
                        let rendered = ::kdl_config::render_value(value);
                        renderer.keyed_raw_with_repr(attr_key, key_repr, rendered);
                    }
                }
                for child in flattened.children() {
                    let rendered = ::kdl_config::render_node(child);
                    child_nodes.push((child.name.clone(), idx, rendered));
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

fn render_positional_fields_node(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let cond = render_condition(field, &access);
        let index = field.placement.positional.expect("positional index");

        let render_body = if field.is_optional {
            let reference = &access.reference;
            let val_expr = value_from_expr(field, quote! { value.clone() });
            quote! {
                if let Some(value) = #reference {
                    positional_args.push((#index, #val_expr, None));
                }
            }
        } else {
            let value = &access.value;
            let val_expr = value_from_expr(field, quote! { #value.clone() });
            quote! {
                positional_args.push((#index, #val_expr, None));
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

fn render_positional_list_fields_node(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let cond = render_condition(field, &access);
        let val_expr = value_from_expr(field, quote! { value.clone() });

        let render_body = if field.is_option_vec {
            let reference = &access.reference;
            quote! {
                if let Some(values) = #reference {
                    for (idx, value) in values.iter().enumerate() {
                        positional_args.push((idx, #val_expr, None));
                    }
                }
            }
        } else {
            let reference = &access.reference;
            quote! {
                for (idx, value) in (#reference).iter().enumerate() {
                    positional_args.push((idx, #val_expr, None));
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

fn render_keyed_fields_node(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);
        let val_expr = value_from_expr(field, quote! { value.clone() });

        let render_body = if field.is_vec || field.is_option_vec {
            if field.is_option_vec {
                let reference = &access.reference;
                quote! {
                    if let Some(values) = #reference {
                        for value in values {
                            node.set_attr(#key, #val_expr);
                        }
                    }
                }
            } else {
                let reference = &access.reference;
                quote! {
                    for value in #reference {
                        node.set_attr(#key, #val_expr);
                    }
                }
            }
        } else if field.is_optional {
            let reference = &access.reference;
            quote! {
                if let Some(value) = #reference {
                    node.set_attr(#key, #val_expr);
                }
            }
        } else {
            let value = &access.value;
            let owned_val_expr = value_from_expr(field, quote! { #value.clone() });
            quote! {
                node.set_attr(#key, #owned_val_expr);
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

fn render_flag_fields_node(
    fields: &[&FieldInfo],
    struct_attrs: &StructAttrs,
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);

        let bool_mode = field.bool_mode.clone().unwrap_or(
            struct_attrs
                .default_bool
                .clone()
                .unwrap_or(BoolMode::PresenceAndValue),
        );
        let bool_mode_tokens = bool_mode_tokens(bool_mode);
        let flag_style = field.flag_style.clone().unwrap_or(
            struct_attrs
                .default_flag_style
                .clone()
                .unwrap_or(FlagStyle::Both),
        );

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
                            node.set_attr(#key, ::kdl_config::Value::Bool(*value));
                        }
                        ::kdl_config::BoolMode::PresenceOnly => {
                            if *value {
                                flag_args.push(#pos_flag_lit.to_string());
                            }
                        }
                        ::kdl_config::BoolMode::PresenceAndValue => {
                            if *value {
                                flag_args.push(#pos_flag_lit.to_string());
                            } else {
                                flag_args.push(#neg_flag_lit.to_string());
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
                        node.set_attr(#key, ::kdl_config::Value::Bool(#bool_value));
                    }
                    ::kdl_config::BoolMode::PresenceOnly => {
                        if #bool_value {
                            flag_args.push(#pos_flag_lit.to_string());
                        }
                    }
                    ::kdl_config::BoolMode::PresenceAndValue => {
                        if #bool_value {
                            flag_args.push(#pos_flag_lit.to_string());
                        } else {
                            flag_args.push(#neg_flag_lit.to_string());
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

fn render_value_fields_node(
    fields: &[&FieldInfo],
    struct_attrs: &StructAttrs,
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);

        let bool_mode = field.bool_mode.clone().unwrap_or(
            struct_attrs
                .default_bool
                .clone()
                .unwrap_or(BoolMode::PresenceAndValue),
        );
        let bool_mode_tokens = bool_mode_tokens(bool_mode);

        let render_body = if field.is_vec || field.is_option_vec {
            let map_fn = if field.from_type.is_some() {
                let from_ty = field.from_type.as_ref().unwrap();
                quote! { |v| ::kdl_config::Value::from(<#from_ty>::from(v)) }
            } else {
                quote! { ::kdl_config::Value::from }
            };
            if field.is_option_vec {
                let reference = &access.reference;
                quote! {
                    if let Some(values) = #reference {
                        if !values.is_empty() {
                            let node_values = values.iter().cloned().map(#map_fn).collect::<::std::vec::Vec<_>>();
                            let node = ::kdl_config::value_node(#key, &node_values);
                            value_nodes.push((#key.to_string(), idx, node));
                            idx += 1;
                        }
                    }
                }
            } else {
                let reference = &access.reference;
                quote! {
                    if !(#reference).is_empty() {
                        let node_values = (#reference).iter().cloned().map(#map_fn).collect::<::std::vec::Vec<_>>();
                        let node = ::kdl_config::value_node(#key, &node_values);
                        value_nodes.push((#key.to_string(), idx, node));
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
                                    let node = ::kdl_config::Node::named(#key);
                                    value_nodes.push((#key.to_string(), idx, node));
                                    idx += 1;
                                }
                            }
                            _ => {
                                let node = ::kdl_config::value_node(#key, &[::kdl_config::Value::Bool(*value)]);
                                value_nodes.push((#key.to_string(), idx, node));
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
                                let node = ::kdl_config::Node::named(#key);
                                value_nodes.push((#key.to_string(), idx, node));
                                idx += 1;
                            }
                        }
                        _ => {
                            let node = ::kdl_config::value_node(#key, &[::kdl_config::Value::Bool(#bool_value)]);
                            value_nodes.push((#key.to_string(), idx, node));
                            idx += 1;
                        }
                    }
                }
            }
        } else if field.is_optional {
            let reference = &access.reference;
            let val_expr = value_from_expr(field, quote! { value.clone() });
            quote! {
                if let Some(value) = #reference {
                    let node = ::kdl_config::value_node(#key, &[#val_expr]);
                    value_nodes.push((#key.to_string(), idx, node));
                    idx += 1;
                }
            }
        } else {
            let value = &access.value;
            let val_expr = value_from_expr(field, quote! { #value.clone() });
            quote! {
                let node = ::kdl_config::value_node(#key, &[#val_expr]);
                value_nodes.push((#key.to_string(), idx, node));
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

fn render_child_fields_node(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);

        let render_body = if field.is_optional {
            let reference = &access.reference;
            quote! {
                if let Some(value) = #reference {
                    let node = value.render_node(#key);
                    child_nodes.push((#key.to_string(), idx, node));
                    idx += 1;
                }
            }
        } else {
            let value = &access.value;
            quote! {
                let node = (#value).render_node(#key);
                child_nodes.push((#key.to_string(), idx, node));
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

fn render_children_fields_node(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
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
                        let node = child.render_node(#key);
                        child_nodes.push((#key.to_string(), idx, node));
                        idx += 1;
                    }
                }
            }
        } else {
            let reference = &access.reference;
            quote! {
                for child in #reference {
                    let node = child.render_node(#key);
                    child_nodes.push((#key.to_string(), idx, node));
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

fn render_registry_fields_node(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let container = field
            .collection
            .as_ref()
            .and_then(|spec| match &spec.mode {
                CollectionMode::Registry { container } => Some(container.clone()),
                _ => None,
            })
            .unwrap_or_else(|| field.kdl_key.clone());
        let cond = render_condition(field, &access);
        let reference = &access.reference;
        let registry_vec = crate::attrs::extract_registry_vec_value(&field.ty);

        if let Some((_val_ty, is_option_vec)) = registry_vec {
            if is_option_vec {
                items.push(quote! {
                    if #cond {
                        if let Some(entries) = #reference {
                            for (name, value) in entries {
                                let mut child = value.render_node(#container);
                                let key_value = ::kdl_config::Value::String(name.clone());
                                let key_repr = ::kdl_config::render_value(&key_value);
                                child.insert_arg_with_repr(0, key_value, Some(key_repr));
                                child_nodes.push((#container.to_string(), idx, child));
                                idx += 1;
                            }
                        }
                    }
                });
            } else {
                items.push(quote! {
                    if #cond {
                        for (name, value) in #reference {
                            let mut child = value.render_node(#container);
                            let key_value = ::kdl_config::Value::String(name.clone());
                            let key_repr = ::kdl_config::render_value(&key_value);
                            child.insert_arg_with_repr(0, key_value, Some(key_repr));
                            child_nodes.push((#container.to_string(), idx, child));
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
                        let mut child = value.render_node(#container);
                        let key_value = ::kdl_config::Value::String(name.clone());
                        let key_repr = ::kdl_config::render_value(&key_value);
                        child.insert_arg_with_repr(0, key_value, Some(key_repr));
                        child_nodes.push((#container.to_string(), idx, child));
                        idx += 1;
                    }
                }
            });
        }
    }
    quote! { #(#items)* }
}

fn render_children_map_fields_node(
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
                        for (rendered_key, name, value) in entries {
                            let mut child = value.render_node(#map_node);
                            child.insert_arg_with_repr(
                                0,
                                ::kdl_config::Value::from((*name).clone()),
                                Some(rendered_key),
                            );
                            child_nodes.push((#map_node.to_string(), idx, child));
                            idx += 1;
                        }
                    }
                });
            }
            (Some(map_node), crate::attrs::ChildrenMapKind::BTreeMap) => {
                items.push(quote! {
                    if #cond {
                        for (name, value) in #reference {
                            let rendered_key = ::kdl_config::render_value(
                                &::kdl_config::Value::from((*name).clone()),
                            );
                            let mut child = value.render_node(#map_node);
                            child.insert_arg_with_repr(
                                0,
                                ::kdl_config::Value::from((*name).clone()),
                                Some(rendered_key),
                            );
                            child_nodes.push((#map_node.to_string(), idx, child));
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
                            let mut child = value.render_node(#map_node);
                            child.insert_arg_with_repr(
                                0,
                                ::kdl_config::Value::from((*name).clone()),
                                Some(rendered_key),
                            );
                            child_nodes.push((#map_node.to_string(), idx, child));
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
                                let mut child = value.render_node(#map_node);
                                child.insert_arg_with_repr(
                                    0,
                                    ::kdl_config::Value::from((*name).clone()),
                                    Some(rendered_key),
                                );
                                child_nodes.push((#map_node.to_string(), idx, child));
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
                            let child = value.render_node(&rendered_key);
                            child_nodes.push((rendered_key, idx, child));
                            idx += 1;
                        }
                    }
                });
            }
            (None, crate::attrs::ChildrenMapKind::BTreeMap) => {
                items.push(quote! {
                    if #cond {
                        for (name, value) in #reference {
                            let rendered_key = name.to_string();
                            let child = value.render_node(&rendered_key);
                            child_nodes.push((rendered_key, idx, child));
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
                            let child = value.render_node(&rendered_key);
                            child_nodes.push((rendered_key, idx, child));
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
                                let child = value.render_node(&rendered_key);
                                child_nodes.push((rendered_key, idx, child));
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

fn render_flatten_fields_node(
    fields: &[&FieldInfo],
    accessor: fn(&FieldInfo) -> FieldAccessor,
) -> TokenStream {
    let mut items = Vec::new();
    for field in fields {
        let access = accessor(field);
        let key = &field.kdl_key;
        let cond = render_condition(field, &access);
        let render_body = if field.is_optional {
            let reference = &access.reference;
            quote! {
                if let Some(value) = #reference {
                    let flattened = ::kdl_config::render_flatten(value, #key);
                    let pos_offset = positional_args
                        .iter()
                        .map(|(idx, _, _)| *idx)
                        .max()
                        .map(|idx| idx + 1)
                        .unwrap_or(0);
                    for (offset, arg) in flattened.args().iter().enumerate() {
                        let idx = pos_offset + offset;
                        let repr = flattened.arg_repr(offset).map(|repr| repr.to_string());
                        positional_args.push((idx, arg.clone(), repr));
                    }
                    for (attr_key, values) in flattened.attrs() {
                        let key_repr = flattened.attr_repr(attr_key).map(|repr| repr.to_string());
                        if let Some(key_repr) = key_repr {
                            node.set_attr_repr(attr_key, key_repr);
                        }
                        for value in values {
                            node.set_attr(attr_key, value.clone());
                        }
                    }
                    for child in flattened.children() {
                        child_nodes.push((child.name.clone(), idx, child.clone()));
                        idx += 1;
                    }
                }
            }
        } else {
            let reference = &access.reference;
            quote! {
                let flattened = ::kdl_config::render_flatten(#reference, #key);
                let pos_offset = positional_args
                    .iter()
                    .map(|(idx, _, _)| *idx)
                    .max()
                    .map(|idx| idx + 1)
                    .unwrap_or(0);
                for (offset, arg) in flattened.args().iter().enumerate() {
                    let idx = pos_offset + offset;
                    let repr = flattened.arg_repr(offset).map(|repr| repr.to_string());
                    positional_args.push((idx, arg.clone(), repr));
                }
                for (attr_key, values) in flattened.attrs() {
                    let key_repr = flattened.attr_repr(attr_key).map(|repr| repr.to_string());
                    if let Some(key_repr) = key_repr {
                        node.set_attr_repr(attr_key, key_repr);
                    }
                    for value in values {
                        node.set_attr(attr_key, value.clone());
                    }
                }
                for child in flattened.children() {
                    child_nodes.push((child.name.clone(), idx, child.clone()));
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

fn bool_mode_tokens(mode: BoolMode) -> TokenStream {
    match mode {
        BoolMode::PresenceAndValue => quote! { ::kdl_config::BoolMode::PresenceAndValue },
        BoolMode::ValueOnly => quote! { ::kdl_config::BoolMode::ValueOnly },
        BoolMode::PresenceOnly => quote! { ::kdl_config::BoolMode::PresenceOnly },
    }
}
