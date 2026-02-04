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

pub fn generate_kdl_choice_impl(
    input: &DeriveInput,
    include_schema: bool,
) -> syn::Result<TokenStream> {
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
                            let inner = <#ty as ::kdl_config::KdlParse>::from_node(node, config)?;
                            return Ok(Self::#ident(inner));
                        }
                    }
                }
                VariantKind::Unit => {
                    quote! {
                        #kdl_name => {
                            if !node.args().is_empty() || !node.attrs().is_empty() || !node.children().is_empty() {
                                return Err(::kdl_config::KdlConfigError::custom(
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
                            ::kdl_config::write_indent(w, indent)?;
                            let rendered = ::kdl_config::NodeRenderer::new(
                                #kdl_name.to_string(),
                                ::kdl_config::Modifier::Inherit,
                            ).render();
                            w.write_str(&rendered)?;
                            Ok(())
                        }
                    }
                }
            }
        })
        .collect();

    let schema_choices: Vec<TokenStream> = variants
        .iter()
        .map(|variant| {
            let kdl_name = &variant.kdl_name;
            match &variant.kind {
                VariantKind::Newtype(ty) => {
                    quote! {
                        {
                            let schema_ref = <#ty as ::kdl_config::schema::KdlSchema>::schema_ref();
                            let mut node_schema = ::kdl_config::schema::KdlNodeSchema::default();
                            node_schema.name = Some(#kdl_name.to_string());
                            match schema_ref {
                                ::kdl_config::schema::SchemaRef::Ref(r) => {
                                    node_schema.ref_type = Some(r);
                                }
                                ::kdl_config::schema::SchemaRef::Inline(s) => {
                                    node_schema.props = s.props;
                                    node_schema.values = s.values;
                                    node_schema.children = s.children;
                                }
                                ::kdl_config::schema::SchemaRef::Choice(choices) => {
                                    node_schema.children = Some(::std::boxed::Box::new(
                                        ::kdl_config::schema::ChildrenSchema {
                                            nodes: vec![::kdl_config::schema::SchemaRef::Choice(choices)],
                                        },
                                    ));
                                }
                            }
                            ::kdl_config::schema::SchemaRef::Inline(node_schema)
                        }
                    }
                }
                VariantKind::Unit => {
                    quote! {
                        {
                            let mut node_schema = ::kdl_config::schema::KdlNodeSchema::default();
                            node_schema.name = Some(#kdl_name.to_string());
                            ::kdl_config::schema::SchemaRef::Inline(node_schema)
                        }
                    }
                }
            }
        })
        .collect();

    let schema_register_calls: Vec<TokenStream> = variants
        .iter()
        .filter_map(|variant| match &variant.kind {
            VariantKind::Newtype(ty) => Some(quote! {
                <#ty as ::kdl_config::schema::KdlSchema>::register_definitions(registry);
            }),
            VariantKind::Unit => None,
        })
        .collect();

    let schema_impl = if include_schema {
        quote! {
            impl ::kdl_config::schema::KdlSchema for #enum_name {
                fn schema_ref() -> ::kdl_config::schema::SchemaRef {
                    ::kdl_config::schema::SchemaRef::Choice(vec![
                        #(#schema_choices),*
                    ])
                }

                fn register_definitions(registry: &mut ::kdl_config::schema::SchemaRegistry) {
                    #(#schema_register_calls)*
                }
            }
        }
    } else {
        quote! {}
    };

    Ok(quote! {
        impl ::kdl_config::KdlParse for #enum_name {
            fn from_node(
                node: &::kdl_config::Node,
                config: &::kdl_config::ParseConfig,
            ) -> ::core::result::Result<Self, ::kdl_config::KdlConfigError> {
                let node_name = node.name.as_str();
                match node_name {
                    #(#parse_arms)*
                    _ => {}
                }

                Err(::kdl_config::KdlConfigError::no_matching_choice(
                    #enum_name_str,
                    node_name,
                    &[#(#valid_variant_names),*],
                ))
            }
        }

        impl ::kdl_config::KdlRender for #enum_name {
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
        #schema_impl
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
            } else if meta.path.is_ident("choice") || meta.path.is_ident("schema") {
                if meta.input.peek(syn::Token![=]) {
                    let _: syn::Expr = meta.value()?.parse()?;
                } else if !meta.input.is_empty() {
                    meta.parse_nested_meta(|_| Ok(()))?;
                }
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
