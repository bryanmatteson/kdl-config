mod attrs;
mod parse_gen;
mod render_gen;
mod value_gen;

use proc_macro::TokenStream;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

use attrs::{FieldInfo, parse_struct_attrs};
use parse_gen::generate_parse_impl;
use render_gen::generate_render_impl;

#[proc_macro_derive(KdlNode, attributes(kdl))]
pub fn derive_kdl_node(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match derive_kdl_node_impl(input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn derive_kdl_node_impl(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = &input.ident;

    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(named) => &named.named,
            Fields::Unnamed(_) => {
                return Err(syn::Error::new_spanned(struct_name, "KdlNode can only be derived for structs with named fields"));
            }
            Fields::Unit => {
                return Err(syn::Error::new_spanned(struct_name, "KdlNode cannot be derived for unit structs"));
            }
        },
        Data::Enum(_) => {
            return Err(syn::Error::new_spanned(struct_name, "KdlNode can only be derived for structs, not enums"));
        }
        Data::Union(_) => {
            return Err(syn::Error::new_spanned(struct_name, "KdlNode can only be derived for structs, not unions"));
        }
    };

    let struct_attrs = parse_struct_attrs(&input.attrs)?;

    let mut field_infos: Vec<FieldInfo> = Vec::new();
    let mut skipped_fields: Vec<&syn::Ident> = Vec::new();

    for field in fields {
        match FieldInfo::from_field(field, struct_attrs.rename_all)? {
            Some(info) => field_infos.push(info),
            None => {
                if let Some(ident) = &field.ident {
                    skipped_fields.push(ident);
                }
            }
        }
    }

    let parse_impl = generate_parse_impl(struct_name, &struct_attrs, &field_infos, &skipped_fields);
    let render_impl = generate_render_impl(struct_name, &struct_attrs, &field_infos);

    Ok(quote::quote! {
        #parse_impl
        #render_impl
    })
}

#[proc_macro_derive(KdlValue, attributes(kdl))]
pub fn derive_kdl_value(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match value_gen::generate_kdl_value_impl(&input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
