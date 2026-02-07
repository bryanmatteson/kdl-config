//! Processed field information for code generation.

use proc_macro2::Span;
use syn::{spanned::Spanned, Field, Ident, Type};

use super::field::{FieldAttrs, FieldPlacement, FieldSchemaOverride};
use super::parse::parse_field_attrs;
use super::type_utils::*;
use super::types::{
    BoolMode, ChildrenMapKind, CollectionMode, CollectionSpec, ConflictPolicy, DefaultSpec,
    FlagStyle, RenameStrategy, RenderPlacement, SelectSpec, SelectorAst,
};

/// Processed field information used for code generation.
#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub ident: Ident,
    pub ty: Type,
    pub kdl_key: String,
    pub placement: FieldPlacement,
    pub required: bool,
    pub is_optional: bool,
    pub is_vec: bool,
    pub is_option_vec: bool,
    pub is_hashmap: bool,
    pub is_bool: bool,
    pub is_modifier: bool,
    pub is_scalar: bool,
    pub is_skipped: bool,
    pub flatten: bool,
    pub path: Option<super::field::FieldPath>,
    pub map_node: Option<String>,
    pub children_map_kind: Option<ChildrenMapKind>,
    pub collection: Option<CollectionSpec>,
    pub default: Option<DefaultSpec>,
    pub skip_serializing_if: Option<String>,
    pub bool_mode: Option<BoolMode>,
    pub flag_style: Option<FlagStyle>,
    pub conflict: Option<ConflictPolicy>,
    pub render: Option<RenderPlacement>,
    pub container: Option<String>,
    pub select: Option<SelectSpec>,
    pub schema: FieldSchemaOverride,
}

impl FieldInfo {
    /// Create FieldInfo from a syn::Field with named fields.
    pub fn from_field(field: &Field, rename_all: RenameStrategy) -> syn::Result<Option<Self>> {
        let attrs = parse_field_attrs(field)?;
        let attrs = match attrs {
            Some(a) => a,
            None => return Ok(None),
        };

        let ident = field
            .ident
            .clone()
            .ok_or_else(|| syn::Error::new(field.span(), "tuple structs are not supported"))?;

        let field_name = ident.to_string();
        let kdl_key = attrs
            .name
            .clone()
            .unwrap_or_else(|| rename_all.apply(&field_name));

        Self::from_attrs_and_type(ident, field.ty.clone(), kdl_key, attrs)
    }

    /// Create FieldInfo from a tuple struct field.
    pub fn from_tuple_field(field: &Field, index: usize) -> syn::Result<Option<Self>> {
        let attrs = parse_field_attrs(field)?;
        let attrs = match attrs {
            Some(a) => a,
            None => return Ok(None),
        };

        let err_span = attrs.span;

        // Validate tuple field restrictions
        if attrs.name.is_some()
            || attrs.container.is_some()
            || attrs.select.is_some()
            || attrs.path.is_some()
        {
            return Err(syn::Error::new(
                err_span,
                "tuple struct fields only support positional placement without names, paths, or selectors",
            ));
        }
        if attrs.children_map || attrs.map_node.is_some() {
            return Err(syn::Error::new(
                err_span,
                "tuple struct fields do not support children_map",
            ));
        }
        if attrs.flatten {
            return Err(syn::Error::new(
                err_span,
                "tuple struct fields do not support flatten",
            ));
        }

        if attrs.placement.attr
            || attrs.placement.keyed
            || attrs.placement.positional_list
            || attrs.placement.flag.is_some()
            || attrs.placement.value
            || attrs.placement.child
            || attrs.placement.children
            || attrs.placement.children_any
            || attrs.placement.registry
            || attrs.placement.modifier
        {
            return Err(syn::Error::new(
                err_span,
                "tuple struct fields only support positional placement",
            ));
        }

        let positional = attrs.placement.positional.unwrap_or(index);
        let mut placement = attrs.placement.clone();
        placement.positional = Some(positional);

        let is_optional = is_option_type(&field.ty);
        let is_vec = is_vec_type(&field.ty);
        let is_option_vec = is_option_type(&field.ty)
            && extract_inner_type(&field.ty)
                .map(is_vec_type)
                .unwrap_or(false);
        let is_hashmap = is_hashmap_type(&field.ty);
        let is_bool = is_bool_type(&field.ty)
            || (is_optional
                && extract_inner_type(&field.ty)
                    .map(is_bool_type)
                    .unwrap_or(false));
        let is_modifier = is_modifier_type(&field.ty)
            || (is_optional
                && extract_inner_type(&field.ty)
                    .map(is_modifier_type)
                    .unwrap_or(false));
        let is_scalar = attrs.scalar;

        if is_vec || is_option_vec {
            return Err(syn::Error::new(
                err_span,
                "tuple struct positional fields do not support Vec types",
            ));
        }

        if is_hashmap || is_modifier {
            return Err(syn::Error::new(
                err_span,
                "tuple struct positional fields must be scalar value types",
            ));
        }

        if !is_value_type(&field.ty) && !is_scalar {
            return Err(syn::Error::new(
                err_span,
                "tuple struct positional fields must be scalar value types",
            ));
        }

        if let Some((pos, neg)) = &attrs.placement.flag {
            if pos.is_some() ^ neg.is_some() {
                return Err(syn::Error::new(
                    err_span,
                    "flag override requires both flag and neg_flag",
                ));
            }
        }

        let required = match attrs.required {
            Some(true) => true,
            Some(false) => false,
            None => {
                if is_bool {
                    false
                } else {
                    !is_optional && !is_option_vec
                }
            }
        };

        let ident = Ident::new(
            &format!("__kdl_field_{index}"),
            proc_macro2::Span::call_site(),
        );
        let kdl_key = format!("arg[{positional}]");

        Ok(Some(FieldInfo {
            ident,
            ty: field.ty.clone(),
            kdl_key,
            placement,
            required,
            is_optional,
            is_vec,
            is_option_vec,
            is_hashmap,
            is_bool,
            is_modifier,
            is_scalar,
            is_skipped: attrs.skip,
            flatten: false,
            path: None,
            map_node: None,
            children_map_kind: None,
            collection: None,
            default: attrs.default,
            skip_serializing_if: attrs.skip_serializing_if,
            bool_mode: attrs.bool_mode,
            flag_style: attrs.flag_style,
            conflict: attrs.conflict,
            render: attrs.render,
            container: None,
            select: None,
            schema: attrs.schema,
        }))
    }

    /// Get the inner type for Option<T> or Vec<T>.
    pub fn inner_type(&self) -> Option<&Type> {
        extract_inner_type(&self.ty)
    }

    /// Build FieldInfo from parsed attributes and type information.
    fn from_attrs_and_type(
        ident: Ident,
        ty: Type,
        kdl_key: String,
        attrs: FieldAttrs,
    ) -> syn::Result<Option<Self>> {
        let is_optional = is_option_type(&ty);
        let is_vec = is_vec_type(&ty);
        let is_option_vec =
            is_optional && extract_inner_type(&ty).map(is_vec_type).unwrap_or(false);
        let is_hashmap = is_hashmap_type(&ty);
        let is_bool = is_bool_type(&ty)
            || (is_optional && extract_inner_type(&ty).map(is_bool_type).unwrap_or(false));
        let is_modifier = is_modifier_type(&ty)
            || (is_optional
                && extract_inner_type(&ty)
                    .map(is_modifier_type)
                    .unwrap_or(false));
        let is_scalar = attrs.scalar;
        let is_value = is_value_type(&ty) || (is_scalar && !is_vec && !is_option_vec);

        let vec_inner = if is_option_vec {
            extract_inner_type(&ty).and_then(extract_inner_type)
        } else if is_vec {
            extract_inner_type(&ty)
        } else {
            None
        };
        let is_vec_value = vec_inner.map(is_value_type).unwrap_or(false)
            || (is_scalar && (is_vec || is_option_vec));
        let is_value_like = is_value || is_vec_value;

        let required = match attrs.required {
            Some(true) => true,
            Some(false) => false,
            None => {
                if is_bool || attrs.placement.registry || attrs.children_map {
                    false
                } else {
                    !is_optional && !is_option_vec
                }
            }
        };

        let err_span = attrs.span;
        let is_registry_vec = extract_registry_vec_value(&ty).is_some();

        // Validation
        Self::validate_attrs(
            &attrs,
            &ty,
            is_value_like,
            is_modifier,
            is_bool,
            is_vec,
            is_option_vec,
            is_hashmap,
            is_registry_vec,
            err_span,
        )?;

        let collection = build_collection_spec(&attrs, &kdl_key);

        Ok(Some(FieldInfo {
            ident,
            ty: ty.clone(),
            kdl_key,
            placement: attrs.placement,
            required,
            is_optional,
            is_vec,
            is_option_vec,
            is_hashmap,
            is_bool,
            is_modifier,
            is_scalar,
            is_skipped: attrs.skip,
            flatten: attrs.flatten,
            path: attrs.path,
            map_node: attrs.map_node,
            children_map_kind: extract_children_map_types(&ty).map(|(kind, _, _)| kind),
            collection,
            default: attrs.default,
            skip_serializing_if: attrs.skip_serializing_if,
            bool_mode: attrs.bool_mode,
            flag_style: attrs.flag_style,
            conflict: attrs.conflict,
            render: attrs.render,
            container: attrs.container,
            select: attrs.select,
            schema: attrs.schema,
        }))
    }

    /// Validate field attributes against type constraints.
    #[allow(clippy::too_many_arguments)]
    fn validate_attrs(
        attrs: &FieldAttrs,
        ty: &Type,
        is_value_like: bool,
        is_modifier: bool,
        is_bool: bool,
        is_vec: bool,
        is_option_vec: bool,
        is_hashmap: bool,
        is_registry_vec: bool,
        err_span: Span,
    ) -> syn::Result<()> {
        // Schema type override validation
        if attrs.schema.kind.is_some() && !is_value_like {
            return Err(syn::Error::new(
                err_span,
                "schema(kind = ...) is only valid for scalar value fields",
            ));
        }

        // Flatten validation
        if attrs.flatten {
            if is_value_like {
                return Err(syn::Error::new(
                    err_span,
                    "flatten is only valid for nested node types",
                ));
            }
            if attrs.children_map || attrs.placement.registry {
                return Err(syn::Error::new(
                    err_span,
                    "flatten cannot be combined with registry or children_map placement",
                ));
            }
            if has_any_placement(&attrs.placement) {
                return Err(syn::Error::new(
                    err_span,
                    "flatten cannot be combined with explicit placement",
                ));
            }
            if attrs.container.is_some()
                || attrs.map_node.is_some()
                || attrs.select.is_some()
                || attrs.path.is_some()
            {
                return Err(syn::Error::new(
                    err_span,
                    "flatten cannot be combined with container, map_node, selector, or path options",
                ));
            }
        }

        if attrs.path.is_some() && attrs.placement.modifier {
            return Err(syn::Error::new(
                err_span,
                "path cannot be combined with modifier placement",
            ));
        }

        // Value placement validation
        if (attrs.placement.attr
            || attrs.placement.keyed
            || attrs.placement.positional.is_some()
            || attrs.placement.positional_list
            || attrs.placement.flag.is_some())
            && !is_value_like
        {
            return Err(syn::Error::new(
                err_span,
                "attr/keyed/positional/flag placement is incompatible with nested node types",
            ));
        }

        if attrs.placement.value && !is_value_like {
            return Err(syn::Error::new(
                err_span,
                "value placement is only valid for scalar types",
            ));
        }

        // Modifier validation
        if attrs.placement.modifier {
            if !is_modifier {
                return Err(syn::Error::new(
                    err_span,
                    "modifier placement requires Modifier or Option<Modifier> field type",
                ));
            }
            if has_any_placement(&attrs.placement) && !attrs.placement.modifier {
                return Err(syn::Error::new(
                    err_span,
                    "modifier placement cannot be combined with other placements",
                ));
            }
        }

        // Child placement validation
        if (attrs.placement.child || attrs.placement.children || attrs.placement.children_any)
            && is_value_like
        {
            return Err(syn::Error::new(
                err_span,
                "child/children placement is incompatible with scalar types",
            ));
        }

        // Registry validation
        if attrs.placement.registry && !is_hashmap && !is_registry_vec {
            return Err(syn::Error::new(
                err_span,
                "registry placement requires HashMap<String, V> or Vec<(String, V)> field type",
            ));
        }

        // Children map validation
        if attrs.children_map {
            let children_map_kind = extract_children_map_types(ty);
            if children_map_kind.is_none() {
                return Err(syn::Error::new(
                    err_span,
                    "children_map placement requires HashMap<K, V> or Vec<(K, V)> field type",
                ));
            }
            if has_any_placement(&attrs.placement) {
                return Err(syn::Error::new(
                    err_span,
                    "children_map placement cannot be combined with other placements",
                ));
            }
            if attrs.container.is_some() {
                return Err(syn::Error::new(
                    err_span,
                    "children_map does not support container override",
                ));
            }
            if attrs.select.is_some() && attrs.map_node.is_none() {
                return Err(syn::Error::new(
                    err_span,
                    "children_map selector options require map_node",
                ));
            }
        } else if attrs.map_node.is_some() {
            return Err(syn::Error::new(
                err_span,
                "map_node requires #[kdl(children_map)]",
            ));
        }

        // Registry key validation
        if attrs.select.is_some()
            && !attrs.placement.registry
            && !(attrs.children_map && attrs.map_node.is_some())
        {
            return Err(syn::Error::new(
                err_span,
                "selector options require #[kdl(registry)] or #[kdl(children_map, map_node = ...)]",
            ));
        }

        if let Some(select) = &attrs.select {
            if !(attrs.placement.registry
                || attrs.children_map
                || attrs.placement.child
                || attrs.placement.children
                || attrs.placement.children_any)
            {
                return Err(syn::Error::new(
                    err_span,
                    "selector options require child/children/registry/children_map placement",
                ));
            }

            if attrs.placement.registry {
                validate_selector_for_collection(&select.selector, err_span)?;
            }
            if attrs.children_map && attrs.map_node.is_some() {
                validate_selector_for_collection(&select.selector, err_span)?;
            }
        }

        // Children placement requires Vec
        if attrs.placement.children && !is_vec && !is_option_vec {
            return Err(syn::Error::new(
                err_span,
                "children placement requires Vec<T> field type",
            ));
        }

        // Flag requires bool
        if attrs.placement.flag.is_some() && !is_bool {
            return Err(syn::Error::new(
                err_span,
                "flag placement is only valid for bool fields",
            ));
        }

        // Positional list requires Vec
        if attrs.placement.positional_list && !is_vec && !is_option_vec {
            return Err(syn::Error::new(
                err_span,
                "positional list placement requires Vec<T> or Option<Vec<T>> field type",
            ));
        }

        // Flag override validation
        if let Some((pos, neg)) = &attrs.placement.flag {
            if pos.is_some() ^ neg.is_some() {
                return Err(syn::Error::new(
                    err_span,
                    "flag override requires both flag and neg_flag",
                ));
            }
        }

        // Append conflict requires Vec
        if matches!(attrs.conflict, Some(ConflictPolicy::Append)) && !is_vec && !is_option_vec {
            return Err(syn::Error::new(
                err_span,
                "append conflict policy is only valid for Vec<T> fields",
            ));
        }

        Ok(())
    }
}

/// Check if any placement is explicitly set.
fn has_any_placement(p: &FieldPlacement) -> bool {
    p.attr
        || p.keyed
        || p.positional.is_some()
        || p.positional_list
        || p.flag.is_some()
        || p.value
        || p.child
        || p.children
        || p.children_any
        || p.registry
        || p.modifier
}

fn validate_selector_for_collection(selector: &SelectorAst, err_span: Span) -> syn::Result<()> {
    match selector {
        SelectorAst::Arg(_) | SelectorAst::Attr(_) | SelectorAst::Func(_) => Ok(()),
        SelectorAst::Any(list) => {
            for item in list {
                validate_selector_for_collection(item, err_span)?;
            }
            Ok(())
        }
        SelectorAst::Name => Err(syn::Error::new(
            err_span,
            "name() selector is not valid for registry/children_map key extraction",
        )),
    }
}

fn build_collection_spec(attrs: &FieldAttrs, kdl_key: &str) -> Option<CollectionSpec> {
    if !(attrs.placement.registry || attrs.children_map) {
        return None;
    }

    let mode = if attrs.placement.registry {
        CollectionMode::Registry {
            container: attrs
                .container
                .clone()
                .unwrap_or_else(|| kdl_key.to_string()),
        }
    } else if let Some(node) = attrs.map_node.clone() {
        CollectionMode::ChildrenMapNode { node }
    } else {
        CollectionMode::ChildrenMapAll
    };

    let selector = if let Some(spec) = attrs.select.as_ref() {
        spec.selector.clone()
    } else if attrs.placement.registry || attrs.map_node.is_some() {
        SelectorAst::Arg(0)
    } else {
        SelectorAst::Name
    };

    let inject = attrs
        .select
        .as_ref()
        .and_then(|spec| spec.opts.inject.clone());

    let consume = attrs
        .select
        .as_ref()
        .and_then(|spec| spec.opts.consume)
        .unwrap_or(inject.is_some());

    Some(CollectionSpec {
        mode,
        selector,
        consume,
        inject,
    })
}

/// Check if any value placement is set.
pub fn has_value_placement(placement: &FieldPlacement) -> bool {
    placement.attr
        || placement.keyed
        || placement.positional.is_some()
        || placement.positional_list
        || placement.flag.is_some()
        || placement.value
}

/// Check if any child placement is set.
pub fn has_child_placement(placement: &FieldPlacement) -> bool {
    placement.child || placement.children || placement.children_any
}

/// Classification of a field for codegen dispatch.
#[derive(Debug, Clone, Copy)]
pub enum FieldKind {
    ValueScalar,
    ValueVec,
    Node,
    NodeVec,
    Flatten,
    Collection,
    Modifier,
}

/// Classify a field into its codegen kind.
pub fn field_kind(field: &FieldInfo) -> FieldKind {
    if field.is_modifier {
        return FieldKind::Modifier;
    }
    if field.flatten {
        return FieldKind::Flatten;
    }
    if field.collection.is_some() {
        return FieldKind::Collection;
    }

    let has_value = has_value_placement(&field.placement);
    let has_child = has_child_placement(&field.placement);

    if field.is_vec || field.is_option_vec {
        if has_value {
            return FieldKind::ValueVec;
        }
        if has_child {
            return FieldKind::NodeVec;
        }
        let inner = if field.is_option_vec {
            field
                .inner_type()
                .and_then(super::type_utils::extract_inner_type)
        } else {
            field.inner_type()
        };
        let is_value =
            inner.map(super::type_utils::is_value_type).unwrap_or(false) || field.is_scalar;
        if is_value {
            FieldKind::ValueVec
        } else {
            FieldKind::NodeVec
        }
    } else if has_value {
        FieldKind::ValueScalar
    } else if has_child {
        FieldKind::Node
    } else if super::type_utils::is_value_type(&field.ty) || field.is_scalar {
        FieldKind::ValueScalar
    } else {
        FieldKind::Node
    }
}
