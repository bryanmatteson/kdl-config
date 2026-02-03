use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DataEnum, DeriveInput, Fields};
use syn::spanned::Spanned;

use crate::attrs::RenameStrategy;

#[derive(Debug, Default)]
struct EnumAttrs {
    rename_all: RenameStrategy,
}

#[derive(Debug, Default)]
struct VariantAttrs {
    name: Option<String>,
}

fn parse_enum_attrs(attrs: &[syn::Attribute]) -> syn::Result<EnumAttrs> {
    let mut result = EnumAttrs::default();
    for attr in attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("rename_all") {
                let value: syn::Expr = meta.value()?.parse()?;
                if let syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(lit), .. }) = value {
                    result.rename_all = match lit.value().as_str() {
                        "kebab-case" => RenameStrategy::KebabCase,
                        "snake_case" => RenameStrategy::SnakeCase,
                        "lowercase" => RenameStrategy::Lowercase,
                        "UPPERCASE" => RenameStrategy::Uppercase,
                        "none" => RenameStrategy::None,
                        other => {
                            return Err(syn::Error::new(lit.span(), format!("unknown rename_all value: '{other}'")));
                        }
                    };
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `rename_all`"));
                }
            } else {
                return Err(syn::Error::new(meta.path.span(), "unknown enum attribute for KdlValue"));
            }
            Ok(())
        })?;
    }
    Ok(result)
}

fn parse_variant_attrs(attrs: &[syn::Attribute]) -> syn::Result<VariantAttrs> {
    let mut result = VariantAttrs::default();
    for attr in attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("name") {
                let value: syn::Expr = meta.value()?.parse()?;
                if let syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(lit), .. }) = value {
                    result.name = Some(lit.value());
                } else {
                    return Err(syn::Error::new(value.span(), "expected string literal for `name`"));
                }
            } else {
                return Err(syn::Error::new(meta.path.span(), "unknown variant attribute for KdlValue"));
            }
            Ok(())
        })?;
    }
    Ok(result)
}

pub fn generate_kdl_value_impl(input: &DeriveInput) -> syn::Result<TokenStream> {
    let enum_name = &input.ident;

    match &input.data {
        Data::Enum(data) => generate_kdl_value_enum_impl(input, data),
        Data::Struct(data) => generate_kdl_value_struct_impl(input, data),
        Data::Union(_) => Err(syn::Error::new_spanned(enum_name, "KdlValue can only be derived for enums or newtype structs")),
    }
}

fn generate_kdl_value_enum_impl(input: &DeriveInput, data: &DataEnum) -> syn::Result<TokenStream> {
    let enum_name = &input.ident;
    let enum_attrs = parse_enum_attrs(&input.attrs)?;

    let mut variants = Vec::new();

    for variant in &data.variants {
        if !matches!(variant.fields, Fields::Unit) {
            return Err(syn::Error::new_spanned(variant, "KdlValue can only be derived for unit enums"));
        }
        let attrs = parse_variant_attrs(&variant.attrs)?;
        let name = attrs.name.unwrap_or_else(|| enum_attrs.rename_all.apply(&variant.ident.to_string()));
        variants.push((variant.ident.clone(), name));
    }

    let parse_arms: Vec<_> = variants
        .iter()
        .map(|(ident, name)| quote! { #name => Ok(Self::#ident), })
        .collect();

    let render_arms: Vec<_> = variants
        .iter()
        .map(|(ident, name)| quote! { Self::#ident => ::kdl_config_runtime::Value::String(#name.to_string()), })
        .collect();

    let variants_const: Vec<_> = variants.iter().map(|(_, name)| name.as_str()).collect();

    Ok(quote! {
        impl ::kdl_config_runtime::FromKdlValue for #enum_name {
            const TYPE_NAME: &'static str = "string";

            fn from_value(value: &::kdl_config_runtime::Value) -> ::core::option::Option<Self> {
                match value {
                    ::kdl_config_runtime::Value::String(s) => match s.as_str() {
                        #(#parse_arms)*
                        _ => None,
                    },
                    _ => None,
                }
            }
        }

        impl #enum_name {
            pub const VARIANTS: &'static [&'static str] = &[#(#variants_const),*];
        }

        impl ::core::convert::From<#enum_name> for ::kdl_config_runtime::Value {
            fn from(val: #enum_name) -> Self {
                match val {
                    #(#render_arms)*
                }
            }
        }
    })
}

fn generate_kdl_value_struct_impl(input: &DeriveInput, data: &syn::DataStruct) -> syn::Result<TokenStream> {
    let struct_name = &input.ident;

    let inner_type = match &data.fields {
        Fields::Unnamed(fields) if fields.unnamed.len() == 1 => fields.unnamed.first().unwrap().ty.clone(),
        _ => {
            return Err(syn::Error::new_spanned(struct_name, "KdlValue can only be derived for newtype structs"));
        }
    };

    Ok(quote! {
        impl ::kdl_config_runtime::FromKdlValue for #struct_name {
            const TYPE_NAME: &'static str = <#inner_type as ::kdl_config_runtime::FromKdlValue>::TYPE_NAME;

            fn from_value(value: &::kdl_config_runtime::Value) -> ::core::option::Option<Self> {
                <#inner_type as ::kdl_config_runtime::FromKdlValue>::from_value(value).map(Self)
            }
        }

        impl ::core::convert::From<#struct_name> for ::kdl_config_runtime::Value {
            fn from(val: #struct_name) -> Self {
                ::kdl_config_runtime::Value::from(val.0)
            }
        }
    })
}
