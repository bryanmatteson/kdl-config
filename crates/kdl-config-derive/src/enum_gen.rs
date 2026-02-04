use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::{
    Attribute, DataEnum, DeriveInput, Expr, ExprLit, ExprUnary, Fields, Ident, Lit, Type, UnOp,
};

use crate::attrs::{
    extract_inner_type, is_option_type, parse_struct_attrs, serde_rename_from_attrs, FieldInfo,
    StructAttrs,
};
use crate::parse_gen::{generate_field_parser, generate_skip_marks, generate_struct_overrides};
use crate::render_gen::{render_body_with_accessor, FieldAccessor};

#[derive(Debug)]
enum LiteralTag {
    Int(i128),
    Float(f64),
    Bool(bool),
}

#[derive(Debug)]
enum VariantTag {
    String(String),
    Literal(LiteralTag),
}

#[derive(Debug, Default)]
struct VariantAttrs {
    name: Option<String>,
    tag: Option<VariantTag>,
}

fn parse_variant_attrs(attrs: &[Attribute]) -> syn::Result<VariantAttrs> {
    let mut result = VariantAttrs::default();
    for attr in attrs {
        if !attr.path().is_ident("kdl") {
            continue;
        }
        attr.parse_nested_meta(|meta| apply_variant_meta(meta, &mut result))?;
    }
    if result.name.is_none() {
        if let Some(rename) = serde_rename_from_attrs(attrs)? {
            result.name = Some(rename);
        }
    }
    Ok(result)
}

fn apply_variant_meta(
    meta: syn::meta::ParseNestedMeta,
    result: &mut VariantAttrs,
) -> syn::Result<()> {
    if meta.path.is_ident("meta") || meta.path.is_ident("group") {
        if !meta.input.is_empty() && !meta.input.peek(syn::Token![,]) {
            meta.parse_nested_meta(|nested| apply_variant_meta(nested, result))?;
        }
        return Ok(());
    }

    if meta.path.is_ident("name") || meta.path.is_ident("rename") {
        let value: Expr = meta.value()?.parse()?;
        if let Expr::Lit(ExprLit {
            lit: Lit::Str(lit), ..
        }) = value
        {
            result.name = Some(lit.value());
        } else {
            return Err(syn::Error::new(
                value.span(),
                "expected string literal for `name`",
            ));
        }
    } else if meta.path.is_ident("tag") {
        let value: Expr = meta.value()?.parse()?;
        let tag = parse_tag_expr(&value)?;
        result.tag = Some(tag);
    } else {
        return Err(syn::Error::new(
            meta.path.span(),
            "unknown enum variant attribute",
        ));
    }

    Ok(())
}

fn parse_tag_expr(expr: &Expr) -> syn::Result<VariantTag> {
    match expr {
        Expr::Lit(ExprLit { lit, .. }) => parse_tag_lit(lit),
        Expr::Unary(ExprUnary {
            op: UnOp::Neg(_),
            expr,
            ..
        }) => match &**expr {
            Expr::Lit(ExprLit { lit, .. }) => parse_negated_tag_lit(lit),
            other => Err(syn::Error::new(
                other.span(),
                "expected numeric literal for `tag`",
            )),
        },
        _ => Err(syn::Error::new(expr.span(), "expected literal for `tag`")),
    }
}

fn parse_tag_lit(lit: &Lit) -> syn::Result<VariantTag> {
    match lit {
        Lit::Str(lit) => Ok(VariantTag::String(lit.value())),
        Lit::Int(lit) => Ok(VariantTag::Literal(LiteralTag::Int(lit.base10_parse()?))),
        Lit::Float(lit) => Ok(VariantTag::Literal(LiteralTag::Float(lit.base10_parse()?))),
        Lit::Bool(lit) => Ok(VariantTag::Literal(LiteralTag::Bool(lit.value()))),
        _ => Err(syn::Error::new(
            lit.span(),
            "expected string, int, float, or bool literal for `tag`",
        )),
    }
}

fn parse_negated_tag_lit(lit: &Lit) -> syn::Result<VariantTag> {
    match lit {
        Lit::Int(lit) => Ok(VariantTag::Literal(LiteralTag::Int(
            -lit.base10_parse::<i128>()?,
        ))),
        Lit::Float(lit) => Ok(VariantTag::Literal(LiteralTag::Float(
            -lit.base10_parse::<f64>()?,
        ))),
        _ => Err(syn::Error::new(
            lit.span(),
            "expected numeric literal for `tag`",
        )),
    }
}

enum VariantKind {
    Unit,
    Newtype(Type),
    Tuple(Vec<Type>),
    Struct {
        fields: Vec<FieldInfo>,
        skipped: Vec<Ident>,
        skipped_infos: Vec<FieldInfo>,
    },
}

struct VariantInfo {
    ident: Ident,
    kdl_name: String,
    tag: VariantTag,
    kind: VariantKind,
}

pub fn generate_enum_impl(input: &DeriveInput, data: &DataEnum) -> syn::Result<TokenStream> {
    let enum_name = &input.ident;
    let enum_name_str = enum_name.to_string();
    let struct_attrs = parse_struct_attrs(&input.attrs)?;

    let mut variants: Vec<VariantInfo> = Vec::new();

    for variant in &data.variants {
        let attrs = parse_variant_attrs(&variant.attrs)?;
        let kdl_name = attrs
            .name
            .unwrap_or_else(|| struct_attrs.rename_all.apply(&variant.ident.to_string()));
        let tag = attrs
            .tag
            .unwrap_or_else(|| VariantTag::String(kdl_name.clone()));

        let kind = match &variant.fields {
            Fields::Unit => VariantKind::Unit,
            Fields::Unnamed(fields) => {
                if fields.unnamed.len() == 1 {
                    VariantKind::Newtype(fields.unnamed.first().unwrap().ty.clone())
                } else {
                    let types = fields.unnamed.iter().map(|f| f.ty.clone()).collect();
                    VariantKind::Tuple(types)
                }
            }
            Fields::Named(fields) => {
                let mut field_infos = Vec::new();
                let mut skipped = Vec::new();
                let mut skipped_infos = Vec::new();
                for field in &fields.named {
                    if let Some(info) = FieldInfo::from_field(field, struct_attrs.rename_all)? {
                        if info.is_skipped {
                            skipped.push(info.ident.clone());
                            skipped_infos.push(info);
                        } else {
                            field_infos.push(info);
                        }
                    }
                }
                if field_infos.iter().filter(|info| info.is_modifier).count() > 1 {
                    return Err(syn::Error::new_spanned(
                        variant,
                        "KdlNode supports at most one #[kdl(modifier)] field per variant",
                    ));
                }
                VariantKind::Struct {
                    fields: field_infos,
                    skipped,
                    skipped_infos,
                }
            }
        };

        variants.push(VariantInfo {
            ident: variant.ident.clone(),
            kdl_name,
            tag,
            kind,
        });
    }

    let expected_node_name = struct_attrs.resolved_node_name(&enum_name_str);
    let validate_name = if let Some(ref node_name) = expected_node_name {
        quote! {
            if node.name != #node_name {
                return Err(::kdl_config::KdlConfigError::node_name_mismatch(
                    #enum_name_str,
                    #node_name,
                    &node.name,
                ));
            }
        }
    } else {
        quote! {}
    };

    let struct_overrides = generate_struct_overrides(&struct_attrs);

    let valid_variants_tokens: Vec<TokenStream> = variants
        .iter()
        .map(|variant| tag_display_expr(&variant.tag))
        .collect();

    let parse_arms: Vec<TokenStream> = variants
        .iter()
        .map(|variant| generate_parse_arm(enum_name, &enum_name_str, variant))
        .collect();

    let render_arms: Vec<TokenStream> = variants
        .iter()
        .map(|variant| generate_render_arm(variant, &struct_attrs))
        .collect();

    Ok(quote! {
        impl ::kdl_config::KdlParse for #enum_name {
            fn from_node(node: &::kdl_config::Node, config: &::kdl_config::ParseConfig) -> ::core::result::Result<Self, ::kdl_config::KdlConfigError> {
                #validate_name

                let struct_overrides = #struct_overrides;
                let struct_config = ::kdl_config::resolve_struct(config, struct_overrides);
                let enum_config = ::kdl_config::ParseConfig {
                    default_placement: struct_config.default_placement,
                    default_bool: struct_config.bool_mode,
                    default_flag_style: struct_config.flag_style,
                    default_conflict: struct_config.conflict,
                    deny_unknown: struct_config.deny_unknown,
                };

                let discr = match node.arg(0) {
                    Some(value) => value,
                    None => {
                        return Err(::kdl_config::KdlConfigError::missing_required(
                            #enum_name_str,
                            "variant",
                            "arg[0]",
                            ::kdl_config::Placement::AttrPositional,
                        ));
                    }
                };
                let discr_display = match discr {
                    ::kdl_config::Value::String(s) => s.clone(),
                    _ => ::kdl_config::render_value(discr),
                };

                #(#parse_arms)*

                Err(::kdl_config::KdlConfigError::invalid_variant(
                    #enum_name_str,
                    discr_display,
                    vec![#(#valid_variants_tokens),*],
                ))
            }
        }

        impl ::kdl_config::KdlRender for #enum_name {
            fn render<W: ::std::fmt::Write>(&self, w: &mut W, name: &str, indent: usize) -> ::std::fmt::Result {
                let rendered = match self {
                    #(#render_arms)*
                };

                let mut iter = rendered.lines().peekable();
                while let Some(line) = iter.next() {
                    ::kdl_config::write_indent(w, indent)?;
                    w.write_str(line)?;
                    if iter.peek().is_some() {
                        w.write_str("\n")?;
                    }
                }

                Ok(())
            }

            fn render_node(&self, name: &str) -> ::kdl_config::Node {
                ::kdl_config::render_child_node(self, name)
            }
        }
    })
}

fn generate_parse_arm(
    enum_name: &Ident,
    enum_name_str: &str,
    variant: &VariantInfo,
) -> TokenStream {
    let variant_ident = &variant.ident;
    let kdl_name = &variant.kdl_name;
    let tag_match = tag_match_expr(&variant.tag);

    let parse_body = match &variant.kind {
        VariantKind::Unit => {
            quote! {
                let variant_node = node.without_first_arg();
                if !variant_node.args().is_empty() || !variant_node.attrs().is_empty() || !variant_node.children().is_empty() {
                    return Err(::kdl_config::KdlConfigError::custom(
                        #enum_name_str,
                        format!("variant '{}' does not accept values", #kdl_name),
                    ));
                }
                Ok(Self::#variant_ident)
            }
        }
        VariantKind::Newtype(inner_ty) => {
            quote! {
                let mut variant_node = node.without_first_arg();
                variant_node.name = #kdl_name.to_string();
                let value = <#inner_ty as ::kdl_config::KdlParse>::from_node(&variant_node, &enum_config)?;
                Ok(Self::#variant_ident(value))
            }
        }
        VariantKind::Tuple(types) => {
            let args_ident = format_ident!("__kdl_tuple_args");
            let mut value_idents = Vec::new();
            let mut parse_values = Vec::new();

            for (idx, ty) in types.iter().enumerate() {
                let arg_ident = format_ident!("__kdl_tuple_arg_{}", idx);
                let value_ident = format_ident!("__kdl_tuple_value_{}", idx);
                value_idents.push(value_ident.clone());
                let arg_index = idx;
                let arg_key = format!("arg[{}]", idx + 1);

                let is_option = is_option_type(ty);
                let value_ty = if is_option {
                    extract_inner_type(ty).unwrap_or(ty)
                } else {
                    ty
                };

                let missing_expr = if is_option {
                    quote! { None }
                } else {
                    quote! {
                        return Err(::kdl_config::KdlConfigError::missing_required(
                            #enum_name_str,
                            "variant",
                            #arg_key,
                            ::kdl_config::Placement::AttrPositional,
                        ));
                    }
                };

                let ty_tokens = if is_option {
                    quote! { Option<#value_ty> }
                } else {
                    quote! { #value_ty }
                };

                parse_values.push(quote! {
                    let #value_ident: #ty_tokens = if let Some(#arg_ident) = #args_ident.get(#arg_index) {
                        ::kdl_config::convert_value_checked::<#ty_tokens>(
                            #arg_ident,
                            #enum_name_str,
                            "variant",
                            #arg_key,
                            ::kdl_config::Placement::AttrPositional,
                        )?
                    } else {
                        #missing_expr
                    };
                });
            }

            let tuple_len = types.len();
            quote! {
                let variant_node = node.without_first_arg();
                if !variant_node.attrs().is_empty() || !variant_node.children().is_empty() {
                    return Err(::kdl_config::KdlConfigError::custom(
                        #enum_name_str,
                        format!("variant '{}' does not accept attributes or children", #kdl_name),
                    ));
                }
                let #args_ident = variant_node.args();
                if #args_ident.len() > #tuple_len {
                    return Err(::kdl_config::KdlConfigError::custom(
                        #enum_name_str,
                        format!("variant '{}' expected {tuple_len} values, found {}", #args_ident.len(), tuple_len = #tuple_len),
                    ));
                }
                #(#parse_values)*
                Ok(Self::#variant_ident(#(#value_idents),*))
            }
        }
        VariantKind::Struct {
            fields,
            skipped,
            skipped_infos,
        } => {
            let struct_name_str = format!("{}::{}", enum_name, variant_ident);
            let field_parsers: Vec<TokenStream> = fields
                .iter()
                .map(|field| generate_field_parser(field, &struct_name_str))
                .collect();
            let skip_marks: Vec<TokenStream> = skipped_infos
                .iter()
                .map(|field| generate_skip_marks(field))
                .collect();
            let field_names: Vec<&Ident> = fields.iter().map(|f| &f.ident).collect();

            let skipped_fields: Vec<TokenStream> = skipped
                .iter()
                .map(|ident| quote! { #ident: ::std::default::Default::default(), })
                .collect();

            quote! {
                let mut variant_node = node.without_first_arg();
                variant_node.name = #kdl_name.to_string();
                let node = &variant_node;
                let mut used_keys = ::kdl_config::helpers::UsedKeys::new();

                #(#field_parsers)*
                #(#skip_marks)*

                if struct_config.deny_unknown {
                    used_keys.check_unknowns(&variant_node, #struct_name_str)?;
                }

                Ok(Self::#variant_ident { #(#field_names,)* #(#skipped_fields)* })
            }
        }
    };

    quote! {
        if #tag_match {
            return { #parse_body };
        }
    }
}

fn generate_render_arm(variant: &VariantInfo, struct_attrs: &StructAttrs) -> TokenStream {
    let variant_ident = &variant.ident;
    let tag_render = tag_render_expr(&variant.tag);

    match &variant.kind {
        VariantKind::Unit => {
            quote! {
                Self::#variant_ident => {
                    let rendered = ::kdl_config::render_key(name);
                    ::kdl_config::insert_arg(&rendered, &#tag_render)
                }
            }
        }
        VariantKind::Newtype(_) => {
            quote! {
                Self::#variant_ident(inner) => {
                    let mut rendered = String::new();
                    inner.render(&mut rendered, name, 0)?;
                    ::kdl_config::insert_arg(&rendered, &#tag_render)
                }
            }
        }
        VariantKind::Tuple(types) => {
            let mut bindings = Vec::new();
            let mut positional = Vec::new();

            for (idx, ty) in types.iter().enumerate() {
                let binding = format_ident!("v{}", idx);
                bindings.push(binding.clone());
                let pos_index = idx + 1;

                if is_option_type(ty) {
                    positional.push(quote! {
                        let value = match #binding {
                            Some(val) => ::kdl_config::Value::from(val.clone()),
                            None => ::kdl_config::Value::Null,
                        };
                        renderer.positional_raw(#pos_index, ::kdl_config::render_value(&value));
                    });
                } else {
                    positional.push(quote! {
                        let value = ::kdl_config::Value::from(#binding.clone());
                        renderer.positional_raw(#pos_index, ::kdl_config::render_value(&value));
                    });
                }
            }

            quote! {
                Self::#variant_ident(#(#bindings),*) => {
                    let mut renderer = ::kdl_config::NodeRenderer::new(name.to_string(), ::kdl_config::Modifier::Inherit);
                    renderer.positional_raw(0, #tag_render);
                    #(#positional)*
                    renderer.render()
                }
            }
        }
        VariantKind::Struct { fields, .. } => {
            let bindings: Vec<&Ident> = fields.iter().map(|f| &f.ident).collect();
            let render_body = render_body_with_accessor(
                struct_attrs,
                fields,
                quote! { name.to_string() },
                None,
                FieldAccessor::binding,
            );

            quote! {
                Self::#variant_ident { #(#bindings),* } => {
                    let rendered = #render_body;
                    ::kdl_config::insert_arg(&rendered, &#tag_render)
                }
            }
        }
    }
}

fn tag_value_expr(tag: &VariantTag) -> TokenStream {
    match tag {
        VariantTag::String(s) => quote! { ::kdl_config::Value::String(#s.to_string()) },
        VariantTag::Literal(LiteralTag::Int(n)) => quote! { ::kdl_config::Value::Int(#n) },
        VariantTag::Literal(LiteralTag::Float(f)) => {
            quote! { ::kdl_config::Value::Float(#f) }
        }
        VariantTag::Literal(LiteralTag::Bool(b)) => {
            quote! { ::kdl_config::Value::Bool(#b) }
        }
    }
}

fn tag_match_expr(tag: &VariantTag) -> TokenStream {
    match tag {
        VariantTag::String(s) => {
            quote! { matches!(discr, ::kdl_config::Value::String(val) if val == #s) }
        }
        VariantTag::Literal(_) => {
            let value_expr = tag_value_expr(tag);
            quote! { discr == &#value_expr }
        }
    }
}

fn tag_render_expr(tag: &VariantTag) -> TokenStream {
    match tag {
        VariantTag::String(s) => quote! { ::kdl_config::render_key(#s) },
        VariantTag::Literal(_) => {
            let value_expr = tag_value_expr(tag);
            quote! { ::kdl_config::render_value(&#value_expr) }
        }
    }
}

fn tag_display_expr(tag: &VariantTag) -> TokenStream {
    match tag {
        VariantTag::String(s) => quote! { #s.to_string() },
        VariantTag::Literal(_) => {
            let value_expr = tag_value_expr(tag);
            quote! { ::kdl_config::render_value(&#value_expr) }
        }
    }
}
