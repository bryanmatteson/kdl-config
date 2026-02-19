use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Field, Fields, LitStr};

#[derive(Debug, Clone)]
enum MergePolicy {
    Deep,
    Keep,
    Replace,
    Append,
    Func(String),
}

impl Default for MergePolicy {
    fn default() -> Self {
        Self::Deep
    }
}

pub fn generate_kdl_merge_impl(input: &DeriveInput) -> syn::Result<TokenStream> {
    let struct_name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    match &input.data {
        Data::Struct(data) => {
            let body = match &data.fields {
                Fields::Named(named) => {
                    let merged = named
                        .named
                        .iter()
                        .map(generate_named_field_merge)
                        .collect::<syn::Result<Vec<_>>>()?;

                    quote! {
                        Self {
                            #(#merged,)*
                        }
                    }
                }
                Fields::Unnamed(unnamed) => {
                    let merged = unnamed
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(idx, field)| generate_tuple_field_merge(idx, field))
                        .collect::<syn::Result<Vec<_>>>()?;

                    quote! {
                        Self(
                            #(#merged,)*
                        )
                    }
                }
                Fields::Unit => quote! { Self },
            };

            Ok(quote! {
                impl #impl_generics ::kdl_config::merge::DeepMerge for #struct_name #ty_generics #where_clause {
                    fn deep_merge(self, other: Self) -> Self {
                        #body
                    }
                }
            })
        }
        Data::Enum(_) => Err(syn::Error::new_spanned(
            &input.ident,
            "KdlMerge currently supports structs only",
        )),
        Data::Union(_) => Err(syn::Error::new_spanned(
            &input.ident,
            "KdlMerge does not support unions",
        )),
    }
}

fn generate_named_field_merge(field: &Field) -> syn::Result<TokenStream> {
    let ident = field.ident.as_ref().expect("named field should have ident");
    let policy = parse_merge_policy(&field.attrs)?;
    let expr = merge_expr(policy, quote! { self.#ident }, quote! { other.#ident })?;
    Ok(quote! { #ident: #expr })
}

fn generate_tuple_field_merge(index: usize, field: &Field) -> syn::Result<TokenStream> {
    let index = syn::Index::from(index);
    let policy = parse_merge_policy(&field.attrs)?;
    merge_expr(policy, quote! { self.#index }, quote! { other.#index })
}

fn merge_expr(
    policy: MergePolicy,
    left: TokenStream,
    right: TokenStream,
) -> syn::Result<TokenStream> {
    match policy {
        MergePolicy::Deep => {
            Ok(quote! { ::kdl_config::merge::DeepMerge::deep_merge(#left, #right) })
        }
        MergePolicy::Keep => Ok(left),
        MergePolicy::Replace => Ok(right),
        MergePolicy::Append => {
            Ok(quote! { ::kdl_config::merge::MergeAppend::merge_append(#left, #right) })
        }
        MergePolicy::Func(path) => {
            let merge_fn: syn::Path = syn::parse_str(&path).map_err(|_| {
                syn::Error::new(
                    proc_macro2::Span::call_site(),
                    "invalid merge(func = \"...\") path",
                )
            })?;
            Ok(quote! { #merge_fn(#left, #right) })
        }
    }
}

fn parse_merge_policy(attrs: &[syn::Attribute]) -> syn::Result<MergePolicy> {
    let mut policy: Option<MergePolicy> = None;

    for attr in attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }

        attr.parse_nested_meta(|meta| {
            if !meta.path.is_ident("merge") {
                if meta.input.peek(syn::Token![=]) {
                    let _: syn::Expr = meta.value()?.parse()?;
                } else if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
                    meta.parse_nested_meta(|_| Ok(()))?;
                }
                return Ok(());
            }

            if policy.is_some() {
                return Err(meta.error("duplicate merge policy for field"));
            }

            if meta.input.peek(syn::Token![=]) {
                let mode: LitStr = meta.value()?.parse()?;
                policy = Some(parse_merge_mode(&mode)?);
                return Ok(());
            }

            if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
                let mut nested_policy: Option<MergePolicy> = None;
                meta.parse_nested_meta(|nested| {
                    if nested.path.is_ident("func") {
                        let path: LitStr = nested.value()?.parse()?;
                        nested_policy = Some(MergePolicy::Func(path.value()));
                        return Ok(());
                    }
                    if nested.path.is_ident("mode") {
                        let mode: LitStr = nested.value()?.parse()?;
                        nested_policy = Some(parse_merge_mode(&mode)?);
                        return Ok(());
                    }
                    Err(nested.error("expected merge(mode = \"...\") or merge(func = \"...\")"))
                })?;
                policy = nested_policy.or(Some(MergePolicy::Deep));
                return Ok(());
            }

            policy = Some(MergePolicy::Deep);
            Ok(())
        })?;
    }

    Ok(policy.unwrap_or_default())
}

fn parse_merge_mode(mode: &LitStr) -> syn::Result<MergePolicy> {
    match mode.value().as_str() {
        "deep" => Ok(MergePolicy::Deep),
        "keep" => Ok(MergePolicy::Keep),
        "replace" => Ok(MergePolicy::Replace),
        "append" => Ok(MergePolicy::Append),
        _ => Err(syn::Error::new_spanned(
            mode,
            "invalid merge mode, expected one of: deep, keep, replace, append",
        )),
    }
}
