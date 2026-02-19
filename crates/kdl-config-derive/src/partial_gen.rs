use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, LitStr};

pub fn generate_kdl_partial_impl(input: &DeriveInput) -> syn::Result<TokenStream> {
    let partial_name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let partial_for = parse_partial_for_attr(&input.attrs)?;
    let partial_for: syn::Path = syn::parse_str(&partial_for).map_err(|_| {
        syn::Error::new_spanned(
            partial_name,
            "invalid #[kdl(partial_for = \"...\")] type path",
        )
    })?;

    let expanded = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(named) => {
                let merged_fields = named
                    .named
                    .iter()
                    .map(|field| {
                        let ident = field.ident.as_ref().expect("named field has an identifier");
                        Ok(quote! {
                            #ident: ::kdl_config::merge::MergeOption::merge_with(base.#ident, self.#ident)
                        })
                    })
                    .collect::<syn::Result<Vec<_>>>()?;

                quote! {
                    impl #impl_generics ::kdl_config::merge::PartialConfig<#partial_for> for #partial_name #ty_generics #where_clause {
                        fn apply_to(self, base: #partial_for) -> #partial_for {
                            #partial_for {
                                #(#merged_fields,)*
                                ..base
                            }
                        }
                    }
                }
            }
            Fields::Unnamed(_) | Fields::Unit => {
                return Err(syn::Error::new_spanned(
                    partial_name,
                    "KdlPartial currently supports named-field structs only",
                ));
            }
        },
        Data::Enum(_) => {
            return Err(syn::Error::new_spanned(
                partial_name,
                "KdlPartial currently supports structs only",
            ));
        }
        Data::Union(_) => {
            return Err(syn::Error::new_spanned(
                partial_name,
                "KdlPartial does not support unions",
            ));
        }
    };

    Ok(expanded)
}

fn parse_partial_for_attr(attrs: &[syn::Attribute]) -> syn::Result<String> {
    let mut partial_for: Option<String> = None;

    for attr in attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }

        attr.parse_nested_meta(|meta| {
            if !meta.path.is_ident("partial_for") {
                if meta.input.peek(syn::Token![=]) {
                    let _: syn::Expr = meta.value()?.parse()?;
                } else if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
                    meta.parse_nested_meta(|_| Ok(()))?;
                }
                return Ok(());
            }

            if partial_for.is_some() {
                return Err(meta.error("duplicate partial_for attribute"));
            }

            let value: LitStr = meta.value()?.parse()?;
            partial_for = Some(value.value());
            Ok(())
        })?;
    }

    partial_for.ok_or_else(|| {
        syn::Error::new(
            proc_macro2::Span::call_site(),
            "KdlPartial requires #[kdl(partial_for = \"TypePath\")]",
        )
    })
}
