use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, Fields, LitStr, Type};

use crate::attrs::{FieldInfo, RenameStrategy, StructAttrs, extract_inner_type, parse_struct_attrs};

pub fn generate_kdl_partial_impl(input: &DeriveInput) -> syn::Result<TokenStream> {
    // Two forms:
    //  - Explicit: `#[derive(KdlPartial)] #[kdl(partial_for = "Base")]` on a
    //    hand-written all-`Option` mirror -> emit only the `PartialConfig` impl.
    //  - Auto: `#[derive(KdlPartial)]` on the *base* struct (no `partial_for`)
    //    -> generate the recursive `*Partial` mirror type plus its impls.
    match parse_partial_for_attr(&input.attrs)? {
        Some(partial_for) => generate_explicit_impl(input, &partial_for),
        None => generate_auto_partial(input),
    }
}

// ---------------------------------------------------------------------------
// Explicit form (unchanged behavior, retained as an opt-in)
// ---------------------------------------------------------------------------

fn generate_explicit_impl(input: &DeriveInput, partial_for: &str) -> syn::Result<TokenStream> {
    let partial_name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let partial_for: syn::Path = syn::parse_str(partial_for).map_err(|_| {
        syn::Error::new_spanned(
            partial_name,
            "invalid #[kdl(partial_for = \"...\")] type path",
        )
    })?;

    match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(named) => {
                let merged_fields = named
                    .named
                    .iter()
                    .map(|field| {
                        let ident = field.ident.as_ref().expect("named field has an identifier");
                        quote! {
                            #ident: ::kdl_config::merge::MergeOption::merge_with(base.#ident, self.#ident)
                        }
                    })
                    .collect::<Vec<_>>();

                Ok(quote! {
                    impl #impl_generics ::kdl_config::merge::PartialConfig<#partial_for> for #partial_name #ty_generics #where_clause {
                        fn apply_to(self, base: #partial_for) -> #partial_for {
                            #partial_for {
                                #(#merged_fields,)*
                                ..base
                            }
                        }
                    }
                })
            }
            Fields::Unnamed(_) | Fields::Unit => Err(syn::Error::new_spanned(
                partial_name,
                "KdlPartial currently supports named-field structs only",
            )),
        },
        Data::Enum(_) => Err(syn::Error::new_spanned(
            partial_name,
            "KdlPartial currently supports structs only",
        )),
        Data::Union(_) => Err(syn::Error::new_spanned(
            partial_name,
            "KdlPartial does not support unions",
        )),
    }
}

// ---------------------------------------------------------------------------
// Auto form: generate the recursive `*Partial` mirror
// ---------------------------------------------------------------------------

fn generate_auto_partial(input: &DeriveInput) -> syn::Result<TokenStream> {
    let base_ident = &input.ident;
    let vis = &input.vis;

    if !input.generics.params.is_empty() {
        return Err(syn::Error::new_spanned(
            &input.generics,
            "KdlPartial auto-generation does not support generic types; use the explicit #[kdl(partial_for = \"...\")] form",
        ));
    }

    let named = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(named) => named,
            _ => {
                return Err(syn::Error::new_spanned(
                    base_ident,
                    "KdlPartial auto-generation requires a named-field struct",
                ));
            }
        },
        _ => {
            return Err(syn::Error::new_spanned(
                base_ident,
                "KdlPartial auto-generation requires a struct",
            ));
        }
    };

    let struct_attrs = parse_struct_attrs(&input.attrs)?;
    let rename_all = struct_attrs.rename_all;

    let mut partial_fields = Vec::new();
    let mut apply_exprs = Vec::new();

    for field in &named.named {
        let ident = field.ident.as_ref().expect("named field has an identifier");

        // FieldInfo is `None` for fields with no `#[kdl]` attribute (not decoded
        // from KDL); those — like `#[kdl(skip)]` fields — are omitted from the
        // partial and filled from `..base` in `apply_to`.
        let info = FieldInfo::from_field(field, rename_all)?;
        let Some(info) = info else { continue };
        if info.is_skipped {
            continue;
        }

        let kept_attrs: Vec<&syn::Attribute> = field
            .attrs
            .iter()
            .filter(|a| a.path().is_ident("kdl"))
            .collect();

        let mapping = map_field(&info, &field.ty)?;
        let partial_ty = mapping.partial_ty;
        let apply = mapping.apply;
        partial_fields.push(quote! { #(#kept_attrs)* pub #ident: #partial_ty });
        apply_exprs.push(apply);
    }

    let partial_ident = format_ident!("{}Partial", base_ident);
    let rename_attr = rename_all_attr(&struct_attrs);

    Ok(quote! {
        #[derive(::core::clone::Clone, ::core::fmt::Debug, ::core::default::Default, ::kdl_config::Kdl)]
        #rename_attr
        #vis struct #partial_ident {
            #(#partial_fields,)*
        }

        impl ::kdl_config::merge::PartialConfig<#base_ident> for #partial_ident {
            fn apply_to(self, base: #base_ident) -> #base_ident {
                #base_ident {
                    #(#apply_exprs,)*
                    ..base
                }
            }
        }
    })
}

struct FieldMapping {
    partial_ty: TokenStream,
    apply: TokenStream,
}

// Each `FieldInfo` shape is handled explicitly below; shapes where a leaf
// override would be silently *wrong* (modifier, registry-as-Vec, malformed
// `children`) yield a descriptive compile error rather than a fallthrough.
fn map_field(info: &FieldInfo, ty: &Type) -> syn::Result<FieldMapping> {
    let ident = &info.ident;

    // `#[kdl(partial = "whole")]` forces a leaf override regardless of
    // placement — the escape hatch for child-placed enums / foreign types.
    if info.partial.as_deref() == Some("whole") {
        return Ok(leaf_mapping(info, ty));
    }

    // Modifiers (+/!/- node markers) have no layered-merge meaning.
    if info.placement.modifier || info.is_modifier {
        return Err(syn::Error::new_spanned(
            ident,
            "KdlPartial: `modifier` fields are not supported by auto-generation; \
             use #[kdl(partial = \"whole\")] if a wholesale override is intended",
        ));
    }

    // Keyed maps (`children_map` / `registry` over a BTreeMap/HashMap) merge by key.
    if info.is_hashmap || info.is_btreemap {
        return Ok(FieldMapping {
            partial_ty: quote! { #ty },
            apply: quote! {
                #ident: ::kdl_config::merge::DeepMerge::deep_merge(base.#ident, self.#ident)
            },
        });
    }

    // A `registry` over a `Vec<(String, T)>` cannot merge by key with the
    // available traits (DeepMerge would replace the whole list, dropping prior
    // entries). Direct the author to a keyed map instead of silently replacing.
    if info.placement.registry {
        return Err(syn::Error::new_spanned(
            ident,
            "KdlPartial: a `registry` field must use a BTreeMap/HashMap to merge by key across \
             layers; a Vec<(String, T)> registry would replace rather than merge — use a map or \
             #[kdl(partial = \"whole\")]",
        ));
    }

    // Value-lists: append when `#[kdl(merge = "append")]`, else replace-unless-empty.
    if info.is_vec || info.is_option_vec {
        let apply = if info.merge.as_deref() == Some("append") {
            quote! { #ident: ::kdl_config::merge::MergeAppend::merge_append(base.#ident, self.#ident) }
        } else {
            quote! { #ident: ::kdl_config::merge::DeepMerge::deep_merge(base.#ident, self.#ident) }
        };
        return Ok(FieldMapping {
            partial_ty: quote! { #ty },
            apply,
        });
    }

    // Flattened nested struct -> recurse so its inlined fields merge per-field.
    if info.flatten {
        return recurse_mapping(info, ty, true);
    }

    // Single nested child struct -> recurse into its generated `*Partial`.
    if info.placement.child {
        return recurse_mapping(info, ty, false);
    }

    // `children` over a non-collection is malformed (collections handled above).
    if info.placement.children {
        return Err(syn::Error::new_spanned(
            ident,
            "KdlPartial: a `children` field must be a Vec or map; \
             use #[kdl(partial = \"whole\")] otherwise",
        ));
    }

    // Everything else is a scalar/value leaf: attr, value, flag, positional,
    // keyed, or a single `children_any` choice — a wholesale override.
    Ok(leaf_mapping(info, ty))
}

/// Leaf override: already-`Option` values keep their type and deep-merge (Some
/// wins); bare values wrap in `Option` to detect "set by this layer".
fn leaf_mapping(info: &FieldInfo, ty: &Type) -> FieldMapping {
    let ident = &info.ident;
    if info.is_optional {
        FieldMapping {
            partial_ty: quote! { #ty },
            apply: quote! {
                #ident: ::kdl_config::merge::DeepMerge::deep_merge(base.#ident, self.#ident)
            },
        }
    } else {
        FieldMapping {
            partial_ty: quote! { ::core::option::Option<#ty> },
            apply: quote! {
                #ident: ::kdl_config::merge::MergeOption::merge_with(base.#ident, self.#ident)
            },
        }
    }
}

/// Recurse into a nested struct's generated `*Partial` (child block or flatten),
/// giving sub-field-granular merge. A nested non-`KdlPartial` type (e.g. a
/// child-placed enum) leaves `#{Ident}Partial` unresolved — the intended
/// compile diagnostic; annotate that field `#[kdl(partial = "whole")]`.
fn recurse_mapping(info: &FieldInfo, ty: &Type, flatten: bool) -> syn::Result<FieldMapping> {
    let ident = &info.ident;
    let inner = if info.is_optional {
        extract_inner_type(ty)
    } else {
        Some(ty)
    };
    let sub_ident = inner.and_then(type_last_ident).ok_or_else(|| {
        syn::Error::new_spanned(
            ident,
            "KdlPartial: nested field type must be a named struct path",
        )
    })?;
    let sub_partial = format_ident!("{}Partial", sub_ident);

    if info.is_optional {
        // Optional nested: None inherits base, Some recurses onto base-or-default.
        return Ok(FieldMapping {
            partial_ty: quote! { ::core::option::Option<#sub_partial> },
            apply: quote! {
                #ident: match self.#ident {
                    ::core::option::Option::Some(p) => ::core::option::Option::Some(
                        ::kdl_config::merge::PartialConfig::apply_to(p, base.#ident.unwrap_or_default())
                    ),
                    ::core::option::Option::None => base.#ident,
                }
            },
        });
    }

    if flatten {
        // A flattened struct is always inline (no wrapper node), so its own
        // all-optional `*Partial` is always present and applied directly.
        return Ok(FieldMapping {
            partial_ty: quote! { #sub_partial },
            apply: quote! {
                #ident: ::kdl_config::merge::PartialConfig::apply_to(self.#ident, base.#ident)
            },
        });
    }

    // Non-optional child block: may still be absent in a layer fragment.
    Ok(FieldMapping {
        partial_ty: quote! { ::core::option::Option<#sub_partial> },
        apply: quote! {
            #ident: match self.#ident {
                ::core::option::Option::Some(p) => ::kdl_config::merge::PartialConfig::apply_to(p, base.#ident),
                ::core::option::Option::None => base.#ident,
            }
        },
    })
}

fn type_last_ident(ty: &Type) -> Option<&syn::Ident> {
    match ty {
        Type::Path(tp) => tp.path.segments.last().map(|s| &s.ident),
        _ => None,
    }
}

fn rename_all_attr(struct_attrs: &StructAttrs) -> TokenStream {
    if !struct_attrs.rename_all_explicit {
        return quote! {};
    }
    let s = match struct_attrs.rename_all {
        RenameStrategy::None => "none",
        RenameStrategy::KebabCase => "kebab-case",
        RenameStrategy::SnakeCase => "snake_case",
        RenameStrategy::Lowercase => "lowercase",
        RenameStrategy::Uppercase => "UPPERCASE",
    };
    quote! { #[kdl(rename_all = #s)] }
}

// ---------------------------------------------------------------------------

fn parse_partial_for_attr(attrs: &[syn::Attribute]) -> syn::Result<Option<String>> {
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

    Ok(partial_for)
}
