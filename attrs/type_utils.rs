//! Type analysis utilities for field types.

use syn::{Type, TypePath};

use super::types::ChildrenMapKind;

/// Check if a type is `Option<T>`.
pub fn is_option_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| s.ident == "Option")
            .unwrap_or(false),
        _ => false,
    }
}

/// Check if a type is `Vec<T>`.
pub fn is_vec_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| s.ident == "Vec")
            .unwrap_or(false),
        _ => false,
    }
}

/// Check if a type is `HashMap<K, V>`.
pub fn is_hashmap_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| s.ident == "HashMap")
            .unwrap_or(false),
        _ => false,
    }
}

/// Check if a type is `bool`.
pub fn is_bool_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| s.ident == "bool")
            .unwrap_or(false),
        _ => false,
    }
}

/// Check if a type is `String` or `PathBuf`.
pub fn is_string_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| s.ident == "String" || s.ident == "PathBuf")
            .unwrap_or(false),
        _ => false,
    }
}

/// Check if a type is a numeric primitive.
pub fn is_numeric_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| {
                matches!(
                    s.ident.to_string().as_str(),
                    "i128" | "i64" | "i32" | "u64" | "u32" | "usize" | "f64" | "f32"
                )
            })
            .unwrap_or(false),
        _ => false,
    }
}

/// Check if a type is `Modifier`.
pub fn is_modifier_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path
            .segments
            .last()
            .map(|s| s.ident == "Modifier")
            .unwrap_or(false),
        _ => false,
    }
}

/// Check if a type is a scalar value type (bool, string, or numeric).
pub fn is_value_type(ty: &Type) -> bool {
    if is_bool_type(ty) || is_string_type(ty) || is_numeric_type(ty) {
        return true;
    }

    if is_option_type(ty) {
        if let Some(inner) = extract_inner_type(ty) {
            return is_value_type(inner);
        }
    }

    false
}

/// Extract the inner type from `Option<T>` or `Vec<T>`.
pub fn extract_inner_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(segment) = path.segments.last() {
            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                    return Some(inner);
                }
            }
        }
    }
    None
}

/// Extract key and value types from `HashMap<K, V>`.
pub fn extract_hashmap_types(ty: &Type) -> Option<(&Type, &Type)> {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(segment) = path.segments.last() {
            if segment.ident == "HashMap" {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    let mut iter = args.args.iter();
                    if let (
                        Some(syn::GenericArgument::Type(key_ty)),
                        Some(syn::GenericArgument::Type(val_ty)),
                    ) = (iter.next(), iter.next())
                    {
                        return Some((key_ty, val_ty));
                    }
                }
            }
        }
    }
    None
}

/// Extract types from a 2-tuple `(A, B)`.
pub fn extract_tuple_types(ty: &Type) -> Option<(&Type, &Type)> {
    if let Type::Tuple(tuple) = ty {
        if tuple.elems.len() == 2 {
            let mut iter = tuple.elems.iter();
            if let (Some(first), Some(second)) = (iter.next(), iter.next()) {
                return Some((first, second));
            }
        }
    }
    None
}

/// Extract types from `Vec<(A, B)>`.
pub fn extract_vec_tuple_types(ty: &Type) -> Option<(&Type, &Type)> {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(segment) = path.segments.last() {
            if segment.ident == "Vec" {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                        return extract_tuple_types(inner);
                    }
                }
            }
        }
    }
    None
}

/// Extract types from children_map field types.
///
/// Supports:
/// - `HashMap<K, V>` -> `(ChildrenMapKind::HashMap, K, V)`
/// - `Vec<(K, V)>` -> `(ChildrenMapKind::Vec, K, V)`
/// - `Option<Vec<(K, V)>>` -> `(ChildrenMapKind::OptionVec, K, V)`
pub fn extract_children_map_types(ty: &Type) -> Option<(ChildrenMapKind, &Type, &Type)> {
    if let Some((key_ty, val_ty)) = extract_hashmap_types(ty) {
        return Some((ChildrenMapKind::HashMap, key_ty, val_ty));
    }

    if let Some((key_ty, val_ty)) = extract_vec_tuple_types(ty) {
        return Some((ChildrenMapKind::Vec, key_ty, val_ty));
    }

    if is_option_type(ty) {
        if let Some(inner) = extract_inner_type(ty) {
            if let Some((key_ty, val_ty)) = extract_vec_tuple_types(inner) {
                return Some((ChildrenMapKind::OptionVec, key_ty, val_ty));
            }
        }
    }

    None
}

/// Extract value type from registry Vec types.
///
/// Supports:
/// - `Vec<(String, V)>` -> `(V, false)`
/// - `Option<Vec<(String, V)>>` -> `(V, true)`
pub fn extract_registry_vec_value(ty: &Type) -> Option<(&Type, bool)> {
    if let Some((key_ty, val_ty)) = extract_vec_tuple_types(ty) {
        if is_string_type(key_ty) {
            return Some((val_ty, false));
        }
    }

    if is_option_type(ty) {
        if let Some(inner) = extract_inner_type(ty) {
            if let Some((key_ty, val_ty)) = extract_vec_tuple_types(inner) {
                if is_string_type(key_ty) {
                    return Some((val_ty, true));
                }
            }
        }
    }

    None
}
