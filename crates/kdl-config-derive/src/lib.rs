mod attrs;
mod choice_gen;
mod enum_gen;
mod kdl_gen;
mod parse_gen;
mod render_gen;
mod schema_gen;
mod update_gen;
mod value_gen;

use proc_macro::TokenStream;
use syn::{Data, DeriveInput, Fields, parse_macro_input};

use attrs::{FieldInfo, FieldKind, field_kind, parse_struct_attrs};
use parse_gen::{generate_field_parser, generate_parse_impl, generate_struct_overrides};
use render_gen::{FieldAccessor, generate_render_impl, render_body_with_accessor};

#[proc_macro_derive(KdlNode, attributes(kdl))]
pub fn derive_kdl_node(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match derive_kdl_node_impl(&input) {
        Ok(mut tokens) => {
            // Always generate KdlSchema so nested types satisfy trait bounds
            match schema_gen::generate_schema_impl(&input) {
                Ok(schema_tokens) => tokens.extend(schema_tokens),
                Err(err) => return err.to_compile_error().into(),
            }
            tokens.into()
        }
        Err(err) => err.to_compile_error().into(),
    }
}

fn derive_kdl_node_impl(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = &input.ident;

    match &input.data {
        Data::Struct(data) => {
            let struct_attrs = parse_struct_attrs(&input.attrs)?;

            match &data.fields {
                Fields::Named(named) => {
                    let fields = &named.named;
                    let mut field_infos: Vec<FieldInfo> = Vec::new();
                    let mut skipped_infos: Vec<FieldInfo> = Vec::new();
                    let mut skipped_fields: Vec<syn::Ident> = Vec::new();

                    for field in fields {
                        match FieldInfo::from_field(field, struct_attrs.rename_all)? {
                            Some(info) => {
                                if info.is_skipped {
                                    skipped_fields.push(info.ident.clone());
                                    skipped_infos.push(info);
                                } else {
                                    field_infos.push(info);
                                }
                            }
                            None => {}
                        }
                    }

                    if field_infos.iter().filter(|info| info.is_modifier).count() > 1 {
                        return Err(syn::Error::new_spanned(
                            struct_name,
                            "KdlNode supports at most one #[kdl(modifier)] field",
                        ));
                    }

                    let parse_impl = generate_parse_impl(
                        struct_name,
                        &struct_attrs,
                        &field_infos,
                        &skipped_fields,
                        &skipped_infos,
                    );
                    let render_impl =
                        generate_render_impl(struct_name, &struct_attrs, &field_infos);
                    let update_impl = update_gen::generate_update_impl(
                        struct_name,
                        &struct_attrs,
                        &field_infos,
                        FieldAccessor::for_self,
                        None,
                    );

                    Ok(quote::quote! {
                        #parse_impl
                        #render_impl
                        #update_impl
                    })
                }
                Fields::Unnamed(unnamed) => {
                    let mut field_infos: Vec<FieldInfo> = Vec::new();
                    let mut skipped_infos: Vec<FieldInfo> = Vec::new();
                    let mut tuple_values: Vec<proc_macro2::TokenStream> = Vec::new();

                    for (index, field) in unnamed.unnamed.iter().enumerate() {
                        match FieldInfo::from_tuple_field(field, index)? {
                            Some(info) => {
                                if info.is_skipped {
                                    skipped_infos.push(info.clone());
                                    tuple_values
                                        .push(quote::quote! { ::std::default::Default::default() });
                                } else {
                                    let ident = &info.ident;
                                    tuple_values.push(quote::quote! { #ident });
                                    field_infos.push(info);
                                }
                            }
                            None => {}
                        }
                    }

                    let struct_name_str = struct_name.to_string();
                    let expected_node_name = struct_attrs.resolved_node_name(&struct_name_str);
                    let validate_name = if let Some(ref node_name) = expected_node_name {
                        quote::quote! {
                            if node.name_str() != #node_name {
                                return Err(::kdl_config::KdlConfigError::node_name_mismatch(
                                    #struct_name_str,
                                    #node_name,
                                    node.name_str(),
                                ));
                            }
                        }
                    } else {
                        quote::quote! {}
                    };

                    let struct_overrides = generate_struct_overrides(&struct_attrs);
                    let field_parsers: Vec<proc_macro2::TokenStream> = field_infos
                        .iter()
                        .map(|field| generate_field_parser(field, &struct_name_str))
                        .collect();
                    let skip_marks: Vec<proc_macro2::TokenStream> = skipped_infos
                        .iter()
                        .map(|field| crate::parse_gen::generate_skip_marks(field, &struct_name_str))
                        .collect();

                    let render_impl = {
                        let render_body = render_body_with_accessor(
                            &struct_attrs,
                            &field_infos,
                            quote::quote! { name.to_string() },
                            None,
                            FieldAccessor::binding,
                        );

                        let binding_defs: Vec<proc_macro2::TokenStream> = (0..tuple_values.len())
                            .map(|i| {
                                let ident = syn::Ident::new(
                                    &format!("__kdl_field_{}", i),
                                    proc_macro2::Span::call_site(),
                                );
                                let index = syn::Index::from(i);
                                quote::quote! { let #ident = &self.#index; }
                            })
                            .collect();

                        quote::quote! {
                            impl ::kdl_config::KdlRender for #struct_name {
                                fn render<W: ::std::fmt::Write>(&self, w: &mut W, name: &str, indent: usize) -> ::std::fmt::Result {
                                    #(#binding_defs)*
                                    let rendered = #render_body;
                                    ::kdl_config::write_indent(w, indent)?;
                                    w.write_str(&rendered)?;
                                    Ok(())
                                }
                            }
                        }
                    };

                    let update_impl = {
                        let binding_defs: Vec<proc_macro2::TokenStream> = (0..tuple_values.len())
                            .map(|i| {
                                let ident = syn::Ident::new(
                                    &format!("__kdl_field_{}", i),
                                    proc_macro2::Span::call_site(),
                                );
                                let index = syn::Index::from(i);
                                quote::quote! { let #ident = &self.#index; }
                            })
                            .collect();
                        let prelude = quote::quote! { #(#binding_defs)* };
                        update_gen::generate_update_impl(
                            struct_name,
                            &struct_attrs,
                            &field_infos,
                            FieldAccessor::binding,
                            Some(prelude),
                        )
                    };

                    let valid_attr_keys: Vec<&str> = field_infos
                        .iter()
                        .filter(|f| !f.is_skipped)
                        .filter(|f| {
                            matches!(field_kind(f), FieldKind::ValueScalar | FieldKind::ValueVec)
                        })
                        .map(|f| f.kdl_key.as_str())
                        .collect();
                    let valid_child_names: Vec<&str> = field_infos
                        .iter()
                        .filter(|f| !f.is_skipped)
                        .filter(|f| {
                            matches!(
                                field_kind(f),
                                FieldKind::Node
                                    | FieldKind::NodeVec
                                    | FieldKind::Flatten
                                    | FieldKind::Collection
                            )
                        })
                        .map(|f| f.kdl_key.as_str())
                        .collect();

                    let parse_impl = quote::quote! {
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

                                    Ok(Self(#(#tuple_values),*))
                                })();

                                result.map_err(|err| {
                                    err.with_context(ctx.source, ctx.path.as_ref(), Some(node.span().offset()))
                                })
                            }
                        }
                    };

                    Ok(quote::quote! {
                        #parse_impl
                        #render_impl
                        #update_impl
                    })
                }
                Fields::Unit => {
                    let struct_name_str = struct_name.to_string();
                    let expected_node_name = struct_attrs.resolved_node_name(&struct_name_str);
                    let struct_overrides = generate_struct_overrides(&struct_attrs);
                    let validate_name = if let Some(ref node_name) = expected_node_name {
                        quote::quote! {
                            if node.name_str() != #node_name {
                                return Err(::kdl_config::KdlConfigError::node_name_mismatch(
                                    #struct_name_str,
                                    #node_name,
                                    node.name_str(),
                                ));
                            }
                        }
                    } else {
                        quote::quote! {}
                    };

                    let parse_impl = quote::quote! {
                        impl ::kdl_config::KdlDecode for #struct_name {
                            fn decode(node: &::kdl_config::KdlNode, ctx: &::kdl_config::DecodeContext) -> ::core::result::Result<Self, ::kdl_config::KdlConfigError> {
                                let result = (|| {
                                    use ::kdl_config::KdlNodeExt as _;
                                    #validate_name
                                    if !node.args().is_empty() || !node.attrs().is_empty() || node.iter_children().next().is_some() {
                                        return Err(::kdl_config::KdlConfigError::custom(
                                            #struct_name_str,
                                            "unit structs do not accept values",
                                        ));
                                    }
                                    let struct_overrides = #struct_overrides;
                                    let struct_config = ::kdl_config::resolve_struct(ctx.config, struct_overrides);
                                    let claims = ::kdl_config::helpers::Claims::new();
                                    if struct_config.deny_unknown {
                                        claims.check_unknowns(node, #struct_name_str, &[], &[])?;
                                    }
                                    Ok(Self)
                                })();

                                result.map_err(|err| {
                                    err.with_context(ctx.source, ctx.path.as_ref(), Some(node.span().offset()))
                                })
                            }
                        }
                    };

                    let render_impl = quote::quote! {
                        impl ::kdl_config::KdlRender for #struct_name {
                            fn render<W: ::std::fmt::Write>(&self, w: &mut W, name: &str, indent: usize) -> ::std::fmt::Result {
                                let rendered = ::kdl_config::NodeRenderer::new(name.to_string(), ::kdl_config::Modifier::Inherit).render();
                                ::kdl_config::write_indent(w, indent)?;
                                w.write_str(&rendered)?;
                                Ok(())
                            }
                        }
                    };

                    let update_impl = quote::quote! {
                        impl ::kdl_config::KdlUpdate for #struct_name {
                            fn update(&self, node: &mut ::kdl_config::KdlNode, _ctx: &::kdl_config::UpdateContext) -> ::core::result::Result<(), ::kdl_config::KdlConfigError> {
                                node.entries_mut().clear();
                                if let Some(children) = node.children_mut() {
                                    children.nodes_mut().clear();
                                }
                                Ok(())
                            }
                        }
                    };

                    Ok(quote::quote! {
                        #parse_impl
                        #render_impl
                        #update_impl
                    })
                }
            }
        }
        Data::Enum(data) => enum_gen::generate_enum_impl(&input, data),
        Data::Union(_) => Err(syn::Error::new_spanned(
            struct_name,
            "KdlNode can only be derived for structs or enums, not unions",
        )),
    }
}

#[proc_macro_derive(KdlValue, attributes(kdl))]
pub fn derive_kdl_value(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match value_gen::generate_kdl_value_impl(&input) {
        Ok(mut tokens) => {
            // Always generate KdlSchema so nested types satisfy trait bounds
            match kdl_gen::generate_value_schema_impl(&input) {
                Ok(schema_tokens) => tokens.extend(schema_tokens),
                Err(err) => return err.to_compile_error().into(),
            }
            tokens.into()
        }
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro_derive(KdlChoice, attributes(kdl))]
pub fn derive_kdl_choice(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match choice_gen::generate_kdl_choice_impl(&input, true) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro_derive(Kdl, attributes(kdl))]
pub fn derive_kdl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match kdl_gen::generate_kdl_impl(&input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro_derive(KdlSchema, attributes(kdl))]
pub fn derive_kdl_schema(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match schema_gen::generate_schema_impl(&input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
