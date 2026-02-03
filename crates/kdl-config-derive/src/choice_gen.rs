use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{Attribute, Data, DeriveInput, Fields};

use crate::attrs::RenameStrategy;

#[derive(Debug, Default)]
struct ChoiceEnumAttrs {
    rename_all: RenameStrategy,
}

#[derive(Debug, Default)]
struct ChoiceVariantAttrs {
    name: Option<String>,
}

enum VariantKind {
    Newtype(syn::Type),
    Unit,
}

struct ChoiceVariantInfo {
    ident: syn::Ident,
    kdl_name: String,
    kind: VariantKind,
}

pub fn generate_kdl_choice_impl(input: &DeriveInput) -> syn::Result<TokenStream> {
    let enum_name = &input.ident;
    let enum_name_str = enum_name.to_string();

    let data_enum = match &input.data {
        Data::Enum(e) => e,
        _ => {
            return Err(syn::Error::new_spanned(
                input,
                "KdlChoice only supports enums",
            ))
        }
    };

    let enum_attrs = parse_choice_enum_attrs(&input.attrs)?;

    let mut variants: Vec<ChoiceVariantInfo> = Vec::new();

    for variant in &data_enum.variants {
        let variant_attrs = parse_choice_variant_attrs(&variant.attrs)?;
        let kdl_name = variant_attrs
            .name
            .unwrap_or_else(|| enum_attrs.rename_all.apply(&variant.ident.to_string()));

        let kind = match &variant.fields {
            Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                VariantKind::Newtype(fields.unnamed.first().unwrap().ty.clone())
            }
            Fields::Unit => VariantKind::Unit,
            _ => {
                return Err(syn::Error::new_spanned(
                    variant,
                    "KdlChoice variants must be newtype (single unnamed field) or unit",
                ))
            }
        };

        variants.push(ChoiceVariantInfo {
            ident: variant.ident.clone(),
            kdl_name,
            kind,
        });
    }

    let parse_arms: Vec<TokenStream> = variants
        .iter()
        .map(|variant| {
            let ident = &variant.ident;
            let kdl_name = &variant.kdl_name;
            match &variant.kind {
                VariantKind::Newtype(ty) => {
                    quote! {
                        #kdl_name => {
                            let inner = <#ty as ::kdl_config_runtime::KdlParse>::from_node(node, config)?;
                            return Ok(Self::#ident(inner));
                        }
                    }
                }
                VariantKind::Unit => {
                    quote! {
                        #kdl_name => {
                            if !node.args().is_empty() || !node.attrs().is_empty() || !node.children().is_empty() {
                                return Err(::kdl_config_runtime::KdlConfigError::custom(
                                    #enum_name_str,
                                    format!("choice '{}' does not accept values", #kdl_name),
                                ));
                            }
                            return Ok(Self::#ident);
                        }
                    }
                }
            }
        })
        .collect();

    let valid_variant_names: Vec<&str> = variants.iter().map(|v| v.kdl_name.as_str()).collect();

    let render_arms: Vec<TokenStream> = variants
        .iter()
        .map(|variant| {
            let ident = &variant.ident;
            let kdl_name = &variant.kdl_name;
            match &variant.kind {
                VariantKind::Newtype(_) => {
                    quote! {
                        Self::#ident(inner) => inner.render(w, #kdl_name, indent),
                    }
                }
                VariantKind::Unit => {
                    quote! {
                        Self::#ident => {
                            ::kdl_config_runtime::write_indent(w, indent)?;
                            let rendered = ::kdl_config_runtime::NodeRenderer::new(
                                #kdl_name.to_string(),
                                ::kdl_config_runtime::Modifier::Inherit,
                            ).render();
                            w.write_str(&rendered)?;
                            Ok(())
                        }
                    }
                }
            }
        })
        .collect();

    Ok(quote! {
        impl ::kdl_config_runtime::KdlParse for #enum_name {
            fn from_node(
                node: &::kdl_config_runtime::Node,
                config: &::kdl_config_runtime::ParseConfig,
            ) -> ::core::result::Result<Self, ::kdl_config_runtime::KdlConfigError> {
                let node_name = node.name.as_str();
                match node_name {
                    #(#parse_arms)*
                    _ => {}
                }

                Err(::kdl_config_runtime::KdlConfigError::no_matching_choice(
                    #enum_name_str,
                    node_name,
                    &[#(#valid_variant_names),*],
                ))
            }
        }

        impl ::kdl_config_runtime::KdlRender for #enum_name {
            fn render<W: ::std::fmt::Write>(
                &self,
                w: &mut W,
                _name: &str,
                indent: usize,
            ) -> ::std::fmt::Result {
                match self {
                    #(#render_arms)*
                }
            }
        }
    })
}

fn parse_choice_enum_attrs(attrs: &[Attribute]) -> syn::Result<ChoiceEnumAttrs> {
    let mut result = ChoiceEnumAttrs::default();

    for attr in attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("rename_all") {
                let lit: syn::LitStr = meta.value()?.parse()?;
                result.rename_all = match lit.value().as_str() {
                    "kebab-case" => RenameStrategy::KebabCase,
                    "snake_case" => RenameStrategy::SnakeCase,
                    "lowercase" => RenameStrategy::Lowercase,
                    "UPPERCASE" => RenameStrategy::Uppercase,
                    "none" => RenameStrategy::None,
                    other => {
                        return Err(syn::Error::new(
                            lit.span(),
                            format!("unknown rename_all value: '{other}'"),
                        ));
                    }
                };
            } else {
                return Err(syn::Error::new(
                    meta.path.span(),
                    "unknown enum attribute for KdlChoice",
                ));
            }
            Ok(())
        })?;
    }

    Ok(result)
}

fn parse_choice_variant_attrs(attrs: &[Attribute]) -> syn::Result<ChoiceVariantAttrs> {
    let mut result = ChoiceVariantAttrs::default();

    for attr in attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("name") || meta.path.is_ident("rename") {
                let lit: syn::LitStr = meta.value()?.parse()?;
                result.name = Some(lit.value());
            } else {
                return Err(syn::Error::new(
                    meta.path.span(),
                    "unknown variant attribute for KdlChoice",
                ));
            }
            Ok(())
        })?;
    }

    Ok(result)
}
