use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{Attribute, Data, DeriveInput};

use crate::{choice_gen, schema_gen, value_gen};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DeriveMode {
    Node,
    Choice,
    Value,
}

#[derive(Debug, Default)]
struct DeriveConfig {
    mode: Option<DeriveMode>,
    schema: bool,
}

pub fn generate_kdl_impl(input: &DeriveInput) -> syn::Result<TokenStream> {
    let config = parse_kdl_derive_attrs(&input.attrs)?;

    if matches!(&input.data, Data::Union(_)) {
        if !config.schema {
            return Err(syn::Error::new_spanned(
                &input.ident,
                "Kdl derive only supports unions for schema generation; add #[kdl(schema)]",
            ));
        }
        if matches!(config.mode, Some(DeriveMode::Choice | DeriveMode::Value)) {
            return Err(syn::Error::new_spanned(
                &input.ident,
                "Kdl derive does not support choice/value modes for unions",
            ));
        }
        return schema_gen::generate_schema_impl(input);
    }

    let mode = config.mode.unwrap_or_else(|| default_mode_for(&input.data));

    let mut tokens = TokenStream::new();

    match mode {
        DeriveMode::Node => {
            tokens.extend(super::derive_kdl_node_impl(input)?);
            // Always generate KdlSchema so nested types satisfy trait bounds
            tokens.extend(schema_gen::generate_schema_impl(input)?);
        }
        DeriveMode::Choice => {
            tokens.extend(choice_gen::generate_kdl_choice_impl(input, config.schema)?);
        }
        DeriveMode::Value => {
            tokens.extend(value_gen::generate_kdl_value_impl(input)?);
            tokens.extend(generate_value_schema_impl(input)?);
        }
    }

    Ok(tokens)
}

fn has_nested_meta(meta: &syn::meta::ParseNestedMeta) -> bool {
    !meta.input.is_empty() && !meta.input.peek(syn::Token![,])
}

/// Consume arbitrary nested meta for `#[kdl(...)]` items that `Kdl` derive itself
/// does not interpret. This keeps the pre-pass permissive while allowing
/// `KdlNode` parsing to handle the real semantics later.
fn consume_meta(meta: syn::meta::ParseNestedMeta) -> syn::Result<()> {
    if meta.input.peek(syn::Token![=]) {
        let _: syn::Expr = meta.value()?.parse()?;
        return Ok(());
    }

    if has_nested_meta(&meta) {
        if meta.input.peek(syn::token::Paren) {
            let content;
            syn::parenthesized!(content in meta.input);
            let _: proc_macro2::TokenStream = content.parse()?;
        } else {
            let _: proc_macro2::TokenStream = meta.input.parse()?;
        }
    }

    Ok(())
}

fn default_mode_for(data: &Data) -> DeriveMode {
    match data {
        Data::Struct(_) => DeriveMode::Node,
        Data::Enum(_) => DeriveMode::Node,
        Data::Union(_) => DeriveMode::Node,
    }
}

fn parse_kdl_derive_attrs(attrs: &[Attribute]) -> syn::Result<DeriveConfig> {
    let mut config = DeriveConfig::default();

    for attr in attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }

        attr.parse_nested_meta(|meta| apply_kdl_derive_meta(meta, &mut config))?;
    }

    Ok(config)
}

fn apply_kdl_derive_meta(
    meta: syn::meta::ParseNestedMeta,
    config: &mut DeriveConfig,
) -> syn::Result<()> {
    if meta.path.is_ident("meta") || meta.path.is_ident("group") {
        if has_nested_meta(&meta) {
            meta.parse_nested_meta(|nested| apply_kdl_derive_meta(nested, config))?;
        }
        return Ok(());
    }

    if meta.path.is_ident("node") {
        if meta.input.peek(syn::Token![=]) {
            let _: syn::Expr = meta.value()?.parse()?;
        } else if has_nested_meta(&meta) {
            meta.parse_nested_meta(consume_meta)?;
        } else {
            set_mode(config, DeriveMode::Node, meta.path.span())?;
        }
        return Ok(());
    }

    if meta.path.is_ident("choice") {
        if meta.input.peek(syn::Token![=]) {
            return Err(meta.error("choice does not accept a value"));
        }
        if has_nested_meta(&meta) {
            meta.parse_nested_meta(consume_meta)?;
        }
        set_mode(config, DeriveMode::Choice, meta.path.span())?;
        return Ok(());
    }

    if meta.path.is_ident("value") {
        if meta.input.peek(syn::Token![=]) {
            return Err(meta.error("value does not accept a value"));
        }
        if has_nested_meta(&meta) {
            meta.parse_nested_meta(consume_meta)?;
        }
        set_mode(config, DeriveMode::Value, meta.path.span())?;
        return Ok(());
    }

    if meta.path.is_ident("schema") {
        if meta.input.peek(syn::Token![=]) {
            let lit: syn::LitBool = meta.value()?.parse()?;
            config.schema = lit.value;
        } else if has_nested_meta(&meta) {
            meta.parse_nested_meta(consume_meta)?;
            config.schema = true;
        } else {
            config.schema = true;
        }
        return Ok(());
    }

    consume_meta(meta)
}

fn set_mode(
    config: &mut DeriveConfig,
    mode: DeriveMode,
    span: proc_macro2::Span,
) -> syn::Result<()> {
    if let Some(existing) = config.mode {
        if existing != mode {
            return Err(syn::Error::new(
                span,
                "Kdl derive supports only one of: node, choice, value",
            ));
        }
    } else {
        config.mode = Some(mode);
    }
    Ok(())
}

pub fn generate_value_schema_impl(input: &DeriveInput) -> syn::Result<TokenStream> {
    let enum_name = &input.ident;

    match &input.data {
        Data::Enum(data) => {
            let variants = value_gen::collect_value_enum_variants(input, data)?;
            let enum_values: Vec<TokenStream> = variants
                .iter()
                .map(|(_, name)| {
                    quote! { ::kdl_config::schema::SchemaLiteral::String(#name.to_string()) }
                })
                .collect();
            Ok(quote! {
                impl ::kdl_config::schema::KdlSchema for #enum_name {
                    fn schema_ref() -> ::kdl_config::schema::SchemaRef {
                            ::kdl_config::schema::SchemaRef::Inline(
                                ::kdl_config::schema::KdlNodeSchema {
                                    values: vec![::kdl_config::schema::SchemaValue {
                                        ty: ::kdl_config::schema::SchemaType::String,
                                        required: true,
                                        description: None,
                                        enum_values: Some(vec![#(#enum_values),*]),
                                        validations: vec![],
                                    }],
                                    ..Default::default()
                                },
                            )
                        }

                    fn register_definitions(_registry: &mut ::kdl_config::schema::SchemaRegistry) {}
                }
            })
        }
        Data::Struct(data) => {
            let inner_ty = match &data.fields {
                syn::Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                    &fields.unnamed.first().unwrap().ty
                }
                _ => {
                    return Err(syn::Error::new_spanned(
                        &input.ident,
                        "Kdl value schema only supports newtype structs",
                    ));
                }
            };

            Ok(quote! {
                impl ::kdl_config::schema::KdlSchema for #enum_name {
                    fn schema_ref() -> ::kdl_config::schema::SchemaRef {
                        let schema_ref = <#inner_ty as ::kdl_config::schema::KdlSchema>::schema_ref();
                        let ty = match schema_ref {
                            ::kdl_config::schema::SchemaRef::Inline(s) => {
                                s.values
                                    .first()
                                    .map(|value| value.ty.clone())
                                    .unwrap_or(::kdl_config::schema::SchemaType::String)
                            }
                            _ => ::kdl_config::schema::SchemaType::String,
                        };

                        ::kdl_config::schema::SchemaRef::Inline(
                            ::kdl_config::schema::KdlNodeSchema {
                                values: vec![::kdl_config::schema::SchemaValue {
                                    ty,
                                    required: true,
                                    description: None,
                                    enum_values: None,
                                    validations: vec![],
                                }],
                                ..Default::default()
                            },
                        )
                    }

                    fn register_definitions(registry: &mut ::kdl_config::schema::SchemaRegistry) {
                        <#inner_ty as ::kdl_config::schema::KdlSchema>::register_definitions(registry);
                    }
                }
            })
        }
        Data::Union(_) => Err(syn::Error::new_spanned(
            &input.ident,
            "Kdl derive does not support unions",
        )),
    }
}
