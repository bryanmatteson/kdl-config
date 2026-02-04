mod attrs;
mod choice_gen;
mod enum_gen;
mod kdl_gen;
mod parse_gen;
mod render_gen;
mod schema_gen;
mod value_gen;

use proc_macro::TokenStream;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

use attrs::{parse_struct_attrs, FieldInfo};
use parse_gen::{generate_field_parser, generate_parse_impl, generate_struct_overrides};
use render_gen::{generate_render_impl, render_body_with_accessor, FieldAccessor};

#[proc_macro_derive(KdlNode, attributes(kdl))]
pub fn derive_kdl_node(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match derive_kdl_node_impl(&input) {
        Ok(tokens) => tokens.into(),
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

                    Ok(quote::quote! {
                        #parse_impl
                        #render_impl
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
                            if node.name != #node_name {
                                return Err(::kdl_config::KdlConfigError::node_name_mismatch(
                                    #struct_name_str,
                                    #node_name,
                                    &node.name,
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
                        .map(|field| crate::parse_gen::generate_skip_marks(field))
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

                    let parse_impl = quote::quote! {
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

                                Ok(Self(#(#tuple_values),*))
                            }
                        }
                    };

                    Ok(quote::quote! {
                        #parse_impl
                        #render_impl
                    })
                }
                Fields::Unit => {
                    let struct_name_str = struct_name.to_string();
                    let expected_node_name = struct_attrs.resolved_node_name(&struct_name_str);
                    let validate_name = if let Some(ref node_name) = expected_node_name {
                        quote::quote! {
                            if node.name != #node_name {
                                return Err(::kdl_config::KdlConfigError::node_name_mismatch(
                                    #struct_name_str,
                                    #node_name,
                                    &node.name,
                                ));
                            }
                        }
                    } else {
                        quote::quote! {}
                    };

                    let parse_impl = quote::quote! {
                        impl ::kdl_config::KdlParse for #struct_name {
                            fn from_node(node: &::kdl_config::Node, _config: &::kdl_config::ParseConfig) -> ::core::result::Result<Self, ::kdl_config::KdlConfigError> {
                                #validate_name
                                if !node.args().is_empty() || !node.attrs().is_empty() || !node.children().is_empty() {
                                    return Err(::kdl_config::KdlConfigError::custom(
                                        #struct_name_str,
                                        "unit structs do not accept values",
                                    ));
                                }
                                Ok(Self)
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

                    Ok(quote::quote! {
                        #parse_impl
                        #render_impl
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
        Ok(tokens) => tokens.into(),
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
