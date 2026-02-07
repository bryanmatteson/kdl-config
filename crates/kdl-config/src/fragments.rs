use std::collections::{HashMap, HashSet};

use kdl::{KdlDocument, KdlEntry, KdlNode, KdlValue};

use crate::context::{LineIndex, Source};
use crate::error::{KdlConfigError, suggest_similar};
use crate::node_ext::{
    KdlNodeExt, remove_attr_entries, remove_positional_entry, update_or_insert_attr,
    update_or_insert_positional,
};
use crate::types::{Modifier, NodeLocation};

pub trait LocationProvider {
    fn location(&self, offset: usize) -> NodeLocation;
}

impl LocationProvider for LineIndex {
    fn location(&self, offset: usize) -> NodeLocation {
        LineIndex::location(self, offset)
    }
}

impl LocationProvider for Source {
    fn location(&self, offset: usize) -> NodeLocation {
        Source::location(self, offset)
    }
}

#[derive(Debug, Clone)]
struct FragmentDef {
    name: String,
    schema_type: Option<String>,
    defined_at: Option<NodeLocation>,
    inserts: Vec<KdlNode>,
    patches: Vec<FragmentPatch>,
}

#[derive(Debug, Clone)]
struct InferredFragmentType {
    schema_type: String,
    first_use: Option<NodeLocation>,
}

type InferredTypeMap = HashMap<String, InferredFragmentType>;

#[derive(Debug, Clone)]
struct ParentInfo {
    name: String,
    ty: Option<String>,
}

impl ParentInfo {
    fn from_node(node: &KdlNode) -> Self {
        Self {
            name: node.base_name().to_string(),
            ty: node.ty().map(|ident| ident.value().to_string()),
        }
    }

    fn expected_type(&self) -> &str {
        self.ty.as_deref().unwrap_or(&self.name)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct FragmentExpansion {
    pub had_fragments: bool,
    pub had_uses: bool,
}

impl FragmentExpansion {
    pub fn modified(&self) -> bool {
        self.had_fragments || self.had_uses
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct NamePath(Vec<(String, usize)>);

impl NamePath {
    fn root(name: &str, index: usize) -> Self {
        Self(vec![(name.to_string(), index)])
    }

    fn child(&self, name: &str, index: usize) -> Self {
        let mut next = self.0.clone();
        next.push((name.to_string(), index));
        Self(next)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FragmentUse {
    pub fragment_name: String,
    pub attr_overrides: Vec<AttrOverride>,
    pub parent_path: NamePath,
    pub insert_start: usize,
    pub patch_len: usize,
    pub fragment_len: usize,
    pub override_len: usize,
    pub attr_override_len: usize,
    pub use_node: KdlNode,
}

#[derive(Debug, Clone)]
pub(crate) struct FragmentPlacement {
    pub fragment_name: String,
    pub parent_path: Option<NamePath>,
    pub index: usize,
    pub node: KdlNode,
}

#[derive(Debug, Default, Clone)]
pub(crate) struct FragmentMap {
    pub uses: Vec<FragmentUse>,
    pub fragments: Vec<FragmentPlacement>,
}

#[derive(Debug, Clone)]
pub(crate) struct AttrOverride {
    pub key: String,
    pub path: Vec<String>,
}

#[derive(Debug, Clone)]
struct AttrOverrideValue {
    key: String,
    path: Vec<String>,
    value: KdlValue,
}

#[derive(Debug, Clone, Default)]
struct OverrideNode {
    name: String,
    value: Option<KdlValue>,
    children: Vec<OverrideNode>,
}

#[derive(Debug, Clone)]
struct FragmentPatch {
    target: String,
    discriminator: Option<String>,
    defined_at: Option<NodeLocation>,
    node: KdlNode,
}

pub fn expand_fragments(
    doc: &mut KdlDocument,
    location: Option<&dyn LocationProvider>,
) -> Result<FragmentExpansion, KdlConfigError> {
    let mut fragments = HashMap::new();
    let mut inferred_types: InferredTypeMap = HashMap::new();
    let mut had_fragments = false;
    collect_fragments_in_nodes(
        doc.nodes_mut(),
        &mut fragments,
        location,
        &mut had_fragments,
    )?;
    let mut expansion = FragmentExpansion {
        had_fragments,
        had_uses: false,
    };
    let mut stack = Vec::new();
    expand_document_nodes(
        doc.nodes_mut(),
        &fragments,
        &mut inferred_types,
        location,
        &mut expansion,
        &mut stack,
    )?;
    Ok(expansion)
}

pub(crate) fn expand_fragments_with_map(
    doc: &mut KdlDocument,
    location: Option<&dyn LocationProvider>,
) -> Result<(FragmentExpansion, FragmentMap), KdlConfigError> {
    let mut map = FragmentMap::default();
    let mut fragments = HashMap::new();
    let mut inferred_types: InferredTypeMap = HashMap::new();
    let mut had_fragments = false;
    collect_fragments_in_nodes_with_map(
        doc.nodes_mut(),
        None,
        &mut fragments,
        location,
        &mut had_fragments,
        &mut map,
    )?;

    let mut expansion = FragmentExpansion {
        had_fragments,
        had_uses: false,
    };
    let mut stack = Vec::new();
    expand_document_nodes_with_map(
        doc.nodes_mut(),
        &fragments,
        &mut inferred_types,
        location,
        &mut expansion,
        &mut stack,
        &mut map,
    )?;
    Ok((expansion, map))
}

fn collect_fragments_in_nodes(
    nodes: &mut Vec<KdlNode>,
    fragments: &mut HashMap<String, FragmentDef>,
    location: Option<&dyn LocationProvider>,
    had_fragments: &mut bool,
) -> Result<(), KdlConfigError> {
    let mut kept = Vec::with_capacity(nodes.len());
    for mut node in nodes.drain(..) {
        if node.base_name() == "fragment" {
            if node.modifier() != Modifier::Inherit {
                return Err(fragment_error(
                    "modifiers are not allowed on fragment nodes",
                    Some(&node),
                    location,
                ));
            }
            *had_fragments = true;
            let fragment = parse_fragment(&node, location)?;
            if let Some(existing) = fragments.get(&fragment.name) {
                let mut message = format!("duplicate fragment name {:?}", fragment.name);
                if let Some(loc) = format_location(existing.defined_at) {
                    message.push_str(&format!(" (first defined at {loc})"));
                }
                return Err(fragment_error(message, Some(&node), location));
            }
            fragments.insert(fragment.name.clone(), fragment);
        } else {
            if let Some(children) = node.children_mut().as_mut() {
                collect_fragments_in_nodes(
                    children.nodes_mut(),
                    fragments,
                    location,
                    had_fragments,
                )?;
            }
            kept.push(node);
        }
    }
    *nodes = kept;
    Ok(())
}

fn collect_fragments_in_nodes_with_map(
    nodes: &mut Vec<KdlNode>,
    parent_path: Option<NamePath>,
    fragments: &mut HashMap<String, FragmentDef>,
    location: Option<&dyn LocationProvider>,
    had_fragments: &mut bool,
    map: &mut FragmentMap,
) -> Result<(), KdlConfigError> {
    let mut kept = Vec::with_capacity(nodes.len());
    for mut node in nodes.drain(..) {
        if node.base_name() == "fragment" {
            if node.modifier() != Modifier::Inherit {
                return Err(fragment_error(
                    "modifiers are not allowed on fragment nodes",
                    Some(&node),
                    location,
                ));
            }
            *had_fragments = true;
            let fragment = parse_fragment(&node, location)?;
            if let Some(existing) = fragments.get(&fragment.name) {
                let mut message = format!("duplicate fragment name {:?}", fragment.name);
                if let Some(loc) = format_location(existing.defined_at) {
                    message.push_str(&format!(" (first defined at {loc})"));
                }
                return Err(fragment_error(message, Some(&node), location));
            }
            let fragment_name = fragment.name.clone();
            fragments.insert(fragment_name.clone(), fragment);
            map.fragments.push(FragmentPlacement {
                fragment_name,
                parent_path: parent_path.clone(),
                index: kept.len(),
                node,
            });
        } else {
            let child_path = match parent_path.as_ref() {
                Some(path) => path.child(node.base_name(), kept.len()),
                None => NamePath::root(node.base_name(), kept.len()),
            };
            if let Some(children) = node.children_mut().as_mut() {
                collect_fragments_in_nodes_with_map(
                    children.nodes_mut(),
                    Some(child_path),
                    fragments,
                    location,
                    had_fragments,
                    map,
                )?;
            }
            kept.push(node);
        }
    }
    *nodes = kept;
    Ok(())
}

fn parse_fragment(
    node: &KdlNode,
    location: Option<&dyn LocationProvider>,
) -> Result<FragmentDef, KdlConfigError> {
    let fragment_type = node.ty().map(|ident| ident.value().to_string());

    let name_value = node.arg(0).ok_or_else(|| {
        fragment_error(
            "fragment nodes must have a name argument",
            Some(node),
            location,
        )
    })?;

    let name = match name_value {
        KdlValue::String(value) => value.clone(),
        other => {
            return Err(fragment_error(
                format!(
                    "fragment name must be a string, found {}",
                    kdl_value_type(other)
                ),
                Some(node),
                location,
            ));
        }
    };

    if node.args().len() > 1 {
        return Err(fragment_error(
            "fragment nodes accept only a single name argument",
            Some(node),
            location,
        ));
    }

    if !node.attrs().is_empty() {
        return Err(fragment_error(
            "fragment nodes do not accept attributes",
            Some(node),
            location,
        ));
    }

    let children = node
        .children()
        .map(|doc| doc.nodes().to_vec())
        .unwrap_or_default();
    let mut inserts = Vec::new();
    let mut patches = Vec::new();
    for child in children {
        match child.modifier() {
            Modifier::Inherit => {
                inserts.push(child);
            }
            Modifier::Flatten => {
                patches.push(parse_fragment_patch(&child, location)?);
            }
            _ => {
                return Err(fragment_error(
                    "fragment entries only support ~ merge patches or unmodified nodes",
                    Some(&child),
                    location,
                ));
            }
        }
    }

    Ok(FragmentDef {
        name,
        schema_type: fragment_type,
        defined_at: node_location(node, location),
        inserts,
        patches,
    })
}

fn parent_discriminator(node: &KdlNode) -> Option<String> {
    match node.arg(1) {
        Some(KdlValue::String(value)) => Some(value.clone()),
        _ => None,
    }
}

fn strip_modifier(mut node: KdlNode) -> KdlNode {
    if node.modifier() == Modifier::Inherit {
        return node;
    }
    let base = node.base_name().to_string();
    node.set_name(base);
    node
}

fn select_fragment_patch<'a>(
    fragment: &'a FragmentDef,
    parent: &KdlNode,
    location: Option<&dyn LocationProvider>,
) -> Result<Option<&'a FragmentPatch>, KdlConfigError> {
    let target = parent.base_name();
    let candidates: Vec<&FragmentPatch> = fragment
        .patches
        .iter()
        .filter(|patch| patch.target == target)
        .collect();
    if candidates.is_empty() {
        return Ok(None);
    }

    let discriminator = parent_discriminator(parent);
    let mut matches: Vec<&FragmentPatch> = Vec::new();
    let mut fallback: Vec<&FragmentPatch> = Vec::new();
    for patch in candidates {
        match (&patch.discriminator, &discriminator) {
            (Some(patch_disc), Some(parent_disc)) if patch_disc == parent_disc => {
                matches.push(patch);
            }
            (None, _) => fallback.push(patch),
            _ => {}
        }
    }

    if matches.len() > 1 {
        let locations = matches
            .iter()
            .filter_map(|patch| format_location(patch.defined_at))
            .collect::<Vec<_>>();
        let mut message = format!(
            "duplicate fragment patch for {:?} with discriminator {:?}",
            target, discriminator
        );
        if !locations.is_empty() {
            message.push_str(&format!(" (defined at {})", locations.join(", ")));
        }
        return Err(fragment_error(message, Some(parent), location));
    }
    if let Some(found) = matches.pop() {
        return Ok(Some(found));
    }

    if fallback.len() > 1 {
        let locations = fallback
            .iter()
            .filter_map(|patch| format_location(patch.defined_at))
            .collect::<Vec<_>>();
        let mut message = format!(
            "duplicate fragment patch for {:?} with no discriminator",
            target
        );
        if !locations.is_empty() {
            message.push_str(&format!(" (defined at {})", locations.join(", ")));
        }
        return Err(fragment_error(message, Some(parent), location));
    }
    Ok(fallback.pop())
}

fn merge_patch_attrs(target: &mut KdlNode, patch: &KdlNode) {
    for entry in patch.entries() {
        if let Some(name) = entry.name() {
            let key = name.value().to_string();
            remove_attr_entries(target, &key);
            target.entries_mut().push(entry.clone());
        }
    }
}

fn merge_patch_attrs_if_missing(target: &mut KdlNode, patch: &KdlNode) {
    for entry in patch.entries() {
        if let Some(name) = entry.name() {
            let key = name.value();
            if !target
                .entries()
                .iter()
                .any(|entry| entry.name().map(|n| n.value() == key).unwrap_or(false))
            {
                target.entries_mut().push(entry.clone());
            }
        }
    }
}

fn remove_children_named(nodes: &mut Vec<KdlNode>, name: &str, insert_at: &mut usize) -> usize {
    let mut removed = 0usize;
    let mut idx = 0usize;
    while idx < nodes.len() {
        if nodes[idx].base_name() == name {
            if idx < *insert_at {
                *insert_at = insert_at.saturating_sub(1);
            }
            nodes.remove(idx);
            removed += 1;
            continue;
        }
        idx += 1;
    }
    removed
}

fn insert_child_at(
    nodes: &mut Vec<KdlNode>,
    insert_at: &mut usize,
    node: KdlNode,
    inserted: &mut usize,
) {
    let idx = (*insert_at).min(nodes.len());
    nodes.insert(idx, node);
    *insert_at = idx + 1;
    *inserted += 1;
}

fn merge_nodes(
    target: &mut KdlNode,
    patch: &KdlNode,
    location: Option<&dyn LocationProvider>,
    prefer_existing: bool,
) -> Result<(), KdlConfigError> {
    let patch = strip_modifier(patch.clone());
    if let Some(ty) = patch.ty() {
        if !prefer_existing || target.ty().is_none() {
            target.set_ty(ty.value().to_string());
        }
    }

    let mut pos = 0usize;
    for entry in patch.entries() {
        if entry.name().is_none() {
            if !prefer_existing || target.arg(pos).is_none() {
                update_or_insert_positional(target, pos, entry.value().clone());
            }
            pos += 1;
        }
    }

    if prefer_existing {
        merge_patch_attrs_if_missing(target, &patch);
    } else {
        merge_patch_attrs(target, &patch);
    }

    let patch_children = patch
        .children()
        .map(|doc| doc.nodes().to_vec())
        .unwrap_or_default();
    if patch_children.is_empty() {
        return Ok(());
    }

    let mut child_doc = target
        .children_mut()
        .take()
        .unwrap_or_else(KdlDocument::new);
    let mut insert_at = child_doc.nodes().len();
    let mut inserted = 0usize;
    merge_patch_children(
        child_doc.nodes_mut(),
        &patch_children,
        &mut insert_at,
        &mut inserted,
        location,
        prefer_existing,
        &mut |_| Ok(()),
    )?;
    if child_doc.nodes().is_empty() {
        target.clear_children();
    } else {
        target.set_children(child_doc);
    }
    Ok(())
}

fn merge_patch_children<F>(
    nodes: &mut Vec<KdlNode>,
    patch_children: &[KdlNode],
    insert_at: &mut usize,
    inserted: &mut usize,
    location: Option<&dyn LocationProvider>,
    prefer_existing: bool,
    expand_node: &mut F,
) -> Result<(), KdlConfigError>
where
    F: FnMut(&mut KdlNode) -> Result<(), KdlConfigError>,
{
    for patch_child in patch_children {
        let modifier = patch_child.modifier();
        let base_name = patch_child.base_name().to_string();
        match modifier {
            Modifier::Remove => {
                remove_children_named(nodes, &base_name, insert_at);
            }
            Modifier::Replace => {
                remove_children_named(nodes, &base_name, insert_at);
                let mut child = strip_modifier(patch_child.clone());
                expand_node(&mut child)?;
                insert_child_at(nodes, insert_at, child, inserted);
            }
            Modifier::Append => {
                let mut child = strip_modifier(patch_child.clone());
                expand_node(&mut child)?;
                insert_child_at(nodes, insert_at, child, inserted);
            }
            Modifier::Flatten | Modifier::Inherit => {
                if let Some(existing) = nodes.iter_mut().find(|node| node.base_name() == base_name)
                {
                    let mut child = strip_modifier(patch_child.clone());
                    expand_node(&mut child)?;
                    merge_nodes(existing, &child, location, prefer_existing)?;
                } else {
                    let mut child = strip_modifier(patch_child.clone());
                    expand_node(&mut child)?;
                    insert_child_at(nodes, insert_at, child, inserted);
                }
            }
        }
    }
    Ok(())
}

fn apply_fragment_patch_inner<F>(
    parent: &mut KdlNode,
    nodes: &mut Vec<KdlNode>,
    insert_at: &mut usize,
    fragment: &FragmentDef,
    location: Option<&dyn LocationProvider>,
    expand_node: &mut F,
) -> Result<usize, KdlConfigError>
where
    F: FnMut(&mut KdlNode) -> Result<(), KdlConfigError>,
{
    let patch = match select_fragment_patch(fragment, parent, location)? {
        Some(patch) => patch,
        None => return Ok(0),
    };

    if !patch.node.args().is_empty() {
        return Err(fragment_error(
            "fragment patch nodes do not accept positional arguments",
            Some(parent),
            location,
        ));
    }

    merge_patch_attrs_if_missing(parent, &patch.node);

    let patch_children = patch
        .node
        .children()
        .map(|doc| doc.nodes().to_vec())
        .unwrap_or_default();
    if patch_children.is_empty() {
        return Ok(0);
    }

    let mut inserted = 0usize;
    merge_patch_children(
        nodes,
        &patch_children,
        insert_at,
        &mut inserted,
        location,
        true,
        expand_node,
    )?;
    Ok(inserted)
}

fn apply_fragment_patch(
    parent: &mut KdlNode,
    nodes: &mut Vec<KdlNode>,
    insert_at: &mut usize,
    fragment: &FragmentDef,
    location: Option<&dyn LocationProvider>,
    fragments: &HashMap<String, FragmentDef>,
    inferred_types: &mut InferredTypeMap,
    expansion: &mut FragmentExpansion,
    stack: &mut Vec<String>,
) -> Result<usize, KdlConfigError> {
    apply_fragment_patch_inner(parent, nodes, insert_at, fragment, location, &mut |child| {
        expand_node(child, fragments, inferred_types, location, expansion, stack)
    })
}

fn apply_fragment_patch_with_map(
    parent: &mut KdlNode,
    nodes: &mut Vec<KdlNode>,
    insert_at: &mut usize,
    fragment: &FragmentDef,
    location: Option<&dyn LocationProvider>,
    parent_path: Option<&NamePath>,
    fragments: &HashMap<String, FragmentDef>,
    inferred_types: &mut InferredTypeMap,
    expansion: &mut FragmentExpansion,
    stack: &mut Vec<String>,
    map: &mut FragmentMap,
) -> Result<usize, KdlConfigError> {
    apply_fragment_patch_inner(parent, nodes, insert_at, fragment, location, &mut |child| {
        expand_node_with_map(
            child,
            parent_path,
            fragments,
            inferred_types,
            location,
            expansion,
            stack,
            map,
        )
    })
}

fn expand_document_nodes(
    nodes: &mut Vec<KdlNode>,
    fragments: &HashMap<String, FragmentDef>,
    inferred_types: &mut InferredTypeMap,
    location: Option<&dyn LocationProvider>,
    expansion: &mut FragmentExpansion,
    stack: &mut Vec<String>,
) -> Result<(), KdlConfigError> {
    for node in nodes.iter_mut() {
        if node.base_name() == "use" {
            return Err(fragment_error(
                "use nodes must be inside a parent node",
                Some(node),
                location,
            ));
        }
        expand_node(node, fragments, inferred_types, location, expansion, stack)?;
    }
    Ok(())
}

fn expand_node(
    node: &mut KdlNode,
    fragments: &HashMap<String, FragmentDef>,
    inferred_types: &mut InferredTypeMap,
    location: Option<&dyn LocationProvider>,
    expansion: &mut FragmentExpansion,
    stack: &mut Vec<String>,
) -> Result<(), KdlConfigError> {
    let parent_info = ParentInfo::from_node(node);
    let mut child_doc = node.children_mut().take().unwrap_or_else(KdlDocument::new);
    let nodes = child_doc.nodes_mut();
    expand_child_nodes(
        node,
        &parent_info,
        nodes,
        fragments,
        inferred_types,
        location,
        expansion,
        stack,
    )?;
    if nodes.is_empty() {
        node.clear_children();
    } else {
        node.set_children(child_doc);
    }
    Ok(())
}

fn expand_child_nodes(
    parent: &mut KdlNode,
    parent_info: &ParentInfo,
    nodes: &mut Vec<KdlNode>,
    fragments: &HashMap<String, FragmentDef>,
    inferred_types: &mut InferredTypeMap,
    location: Option<&dyn LocationProvider>,
    expansion: &mut FragmentExpansion,
    stack: &mut Vec<String>,
) -> Result<(), KdlConfigError> {
    let mut idx = 0usize;
    while idx < nodes.len() {
        if nodes[idx].base_name() == "use" {
            if nodes[idx].modifier() != Modifier::Inherit {
                return Err(fragment_error(
                    "modifiers are not allowed on use nodes",
                    Some(&nodes[idx]),
                    location,
                ));
            }

            let parsed_use = parse_use(&nodes[idx], location)?;
            let fragment_name = parsed_use.name.clone();
            let fragment = fragments.get(&fragment_name).ok_or_else(|| {
                let candidates: Vec<&str> = fragments.keys().map(|key| key.as_str()).collect();
                let mut message = format!("unknown fragment {:?}", fragment_name);
                if let Some(suggestion) = suggest_similar(&fragment_name, &candidates) {
                    message.push_str(&format!(" (did you mean {:?}?)", suggestion));
                }
                fragment_error(message, Some(&nodes[idx]), location)
            })?;

            let expected_type = parent_info.expected_type();
            if let Some(schema_type) = fragment.schema_type.as_deref() {
                if schema_type != expected_type {
                    let mut message = format!(
                        "fragment {:?} is typed as {:?} but used under {:?}",
                        fragment.name, schema_type, expected_type
                    );
                    if let Some(loc) = format_location(fragment.defined_at) {
                        message.push_str(&format!(" (defined at {loc})"));
                    }
                    return Err(fragment_error(message, Some(&nodes[idx]), location));
                }
            } else if let Some(inferred) = inferred_types.get(&fragment.name) {
                if inferred.schema_type != expected_type {
                    let mut message = format!(
                        "fragment {:?} is implicitly typed as {:?} but used under {:?}",
                        fragment.name, inferred.schema_type, expected_type
                    );
                    if let Some(loc) = format_location(inferred.first_use) {
                        message.push_str(&format!(" (first used at {loc})"));
                    }
                    return Err(fragment_error(message, Some(&nodes[idx]), location));
                }
            } else {
                inferred_types.insert(
                    fragment.name.clone(),
                    InferredFragmentType {
                        schema_type: expected_type.to_string(),
                        first_use: node_location(&nodes[idx], location),
                    },
                );
            }

            if stack.contains(&fragment_name) {
                let mut chain = stack.clone();
                chain.push(fragment_name.clone());
                return Err(fragment_error(
                    format!("fragment recursion detected: {}", chain.join(" -> ")),
                    Some(&nodes[idx]),
                    location,
                ));
            }

            expansion.had_uses = true;
            let use_node = nodes.remove(idx);
            let mut insert_at = idx.min(nodes.len());

            stack.push(fragment_name);
            let _patch_len = apply_fragment_patch(
                parent,
                nodes,
                &mut insert_at,
                fragment,
                location,
                fragments,
                inferred_types,
                expansion,
                stack,
            )?;

            let mut fragment_children = fragment.inserts.clone();
            let mut attr_override_children = parsed_use.attr_nodes;
            let mut child_override_children = parsed_use.child_overrides;

            for child in fragment_children.iter_mut() {
                expand_node(child, fragments, inferred_types, location, expansion, stack)?;
            }
            for child in attr_override_children.iter_mut() {
                expand_node(child, fragments, inferred_types, location, expansion, stack)?;
            }
            for child in child_override_children.iter_mut() {
                expand_node(child, fragments, inferred_types, location, expansion, stack)?;
            }
            stack.pop();

            let mut inserted = fragment_children;
            inserted.extend(attr_override_children);
            inserted.extend(child_override_children);
            let inserted_len = inserted.len();
            nodes.splice(insert_at..insert_at, inserted);
            idx = insert_at + inserted_len;
            let _ = use_node;
            continue;
        }

        expand_node(
            &mut nodes[idx],
            fragments,
            inferred_types,
            location,
            expansion,
            stack,
        )?;
        idx += 1;
    }

    Ok(())
}

fn expand_document_nodes_with_map(
    nodes: &mut Vec<KdlNode>,
    fragments: &HashMap<String, FragmentDef>,
    inferred_types: &mut InferredTypeMap,
    location: Option<&dyn LocationProvider>,
    expansion: &mut FragmentExpansion,
    stack: &mut Vec<String>,
    map: &mut FragmentMap,
) -> Result<(), KdlConfigError> {
    for (idx, node) in nodes.iter_mut().enumerate() {
        if node.base_name() == "use" {
            return Err(fragment_error(
                "use nodes must be inside a parent node",
                Some(node),
                location,
            ));
        }
        let path = NamePath::root(node.base_name(), idx);
        expand_node_with_map(
            node,
            Some(&path),
            fragments,
            inferred_types,
            location,
            expansion,
            stack,
            map,
        )?;
    }
    Ok(())
}

fn expand_node_with_map(
    node: &mut KdlNode,
    parent_path: Option<&NamePath>,
    fragments: &HashMap<String, FragmentDef>,
    inferred_types: &mut InferredTypeMap,
    location: Option<&dyn LocationProvider>,
    expansion: &mut FragmentExpansion,
    stack: &mut Vec<String>,
    map: &mut FragmentMap,
) -> Result<(), KdlConfigError> {
    let parent_info = ParentInfo::from_node(node);
    let mut child_doc = node.children_mut().take().unwrap_or_else(KdlDocument::new);
    let nodes = child_doc.nodes_mut();
    expand_child_nodes_with_map(
        node,
        &parent_info,
        parent_path,
        nodes,
        fragments,
        inferred_types,
        location,
        expansion,
        stack,
        map,
    )?;
    if nodes.is_empty() {
        node.clear_children();
    } else {
        node.set_children(child_doc);
    }
    Ok(())
}

fn expand_child_nodes_with_map(
    parent: &mut KdlNode,
    parent_info: &ParentInfo,
    parent_path: Option<&NamePath>,
    nodes: &mut Vec<KdlNode>,
    fragments: &HashMap<String, FragmentDef>,
    inferred_types: &mut InferredTypeMap,
    location: Option<&dyn LocationProvider>,
    expansion: &mut FragmentExpansion,
    stack: &mut Vec<String>,
    map: &mut FragmentMap,
) -> Result<(), KdlConfigError> {
    let mut idx = 0usize;
    while idx < nodes.len() {
        if nodes[idx].base_name() == "use" {
            if nodes[idx].modifier() != Modifier::Inherit {
                return Err(fragment_error(
                    "modifiers are not allowed on use nodes",
                    Some(&nodes[idx]),
                    location,
                ));
            }
            let parsed_use = parse_use(&nodes[idx], location)?;
            let fragment_name = parsed_use.name.clone();
            let fragment = fragments.get(&fragment_name).ok_or_else(|| {
                let candidates: Vec<&str> = fragments.keys().map(|key| key.as_str()).collect();
                let mut message = format!("unknown fragment {:?}", fragment_name);
                if let Some(suggestion) = suggest_similar(&fragment_name, &candidates) {
                    message.push_str(&format!(" (did you mean {:?}?)", suggestion));
                }
                fragment_error(message, Some(&nodes[idx]), location)
            })?;

            let expected_type = parent_info.expected_type();
            if let Some(schema_type) = fragment.schema_type.as_deref() {
                if schema_type != expected_type {
                    let mut message = format!(
                        "fragment {:?} is typed as {:?} but used under {:?}",
                        fragment.name, schema_type, expected_type
                    );
                    if let Some(loc) = format_location(fragment.defined_at) {
                        message.push_str(&format!(" (defined at {loc})"));
                    }
                    return Err(fragment_error(message, Some(&nodes[idx]), location));
                }
            } else if let Some(inferred) = inferred_types.get(&fragment.name) {
                if inferred.schema_type != expected_type {
                    let mut message = format!(
                        "fragment {:?} is implicitly typed as {:?} but used under {:?}",
                        fragment.name, inferred.schema_type, expected_type
                    );
                    if let Some(loc) = format_location(inferred.first_use) {
                        message.push_str(&format!(" (first used at {loc})"));
                    }
                    return Err(fragment_error(message, Some(&nodes[idx]), location));
                }
            } else {
                inferred_types.insert(
                    fragment.name.clone(),
                    InferredFragmentType {
                        schema_type: expected_type.to_string(),
                        first_use: node_location(&nodes[idx], location),
                    },
                );
            }

            if stack.contains(&fragment_name) {
                let mut chain = stack.clone();
                chain.push(fragment_name.clone());
                return Err(fragment_error(
                    format!("fragment recursion detected: {}", chain.join(" -> ")),
                    Some(&nodes[idx]),
                    location,
                ));
            }

            expansion.had_uses = true;
            let use_node = nodes.remove(idx);
            let mut insert_at = idx.min(nodes.len());

            stack.push(fragment_name.clone());
            let patch_len = apply_fragment_patch_with_map(
                parent,
                nodes,
                &mut insert_at,
                fragment,
                location,
                parent_path,
                fragments,
                inferred_types,
                expansion,
                stack,
                map,
            )?;

            let mut fragment_children = fragment.inserts.clone();
            let mut attr_override_children = parsed_use.attr_nodes;
            let mut child_override_children = parsed_use.child_overrides;

            for child in fragment_children.iter_mut() {
                expand_node_with_map(
                    child,
                    parent_path,
                    fragments,
                    inferred_types,
                    location,
                    expansion,
                    stack,
                    map,
                )?;
            }
            for child in attr_override_children.iter_mut() {
                expand_node_with_map(
                    child,
                    parent_path,
                    fragments,
                    inferred_types,
                    location,
                    expansion,
                    stack,
                    map,
                )?;
            }
            for child in child_override_children.iter_mut() {
                expand_node_with_map(
                    child,
                    parent_path,
                    fragments,
                    inferred_types,
                    location,
                    expansion,
                    stack,
                    map,
                )?;
            }
            stack.pop();

            let fragment_len = fragment_children.len();
            let attr_override_len = attr_override_children.len();
            let mut override_children = attr_override_children;
            override_children.extend(child_override_children);
            let override_len = override_children.len();
            let mut inserted = fragment_children;
            inserted.extend(override_children);
            let insert_at = insert_at.min(nodes.len());
            nodes.splice(insert_at..insert_at, inserted);
            if let Some(parent_path) = parent_path {
                map.uses.push(FragmentUse {
                    fragment_name,
                    attr_overrides: parsed_use.attr_overrides,
                    parent_path: parent_path.clone(),
                    insert_start: idx,
                    patch_len,
                    fragment_len,
                    override_len,
                    attr_override_len,
                    use_node,
                });
            }
            idx = insert_at + fragment_len + override_len;
            continue;
        }

        let child_path = child_path(parent_path, nodes, idx);
        expand_node_with_map(
            &mut nodes[idx],
            Some(&child_path),
            fragments,
            inferred_types,
            location,
            expansion,
            stack,
            map,
        )?;
        idx += 1;
    }

    Ok(())
}

fn child_path(parent: Option<&NamePath>, nodes: &[KdlNode], index: usize) -> NamePath {
    let name = nodes[index].base_name();
    let mut count = 0usize;
    for i in 0..index {
        if nodes[i].base_name() == name {
            count += 1;
        }
    }
    match parent {
        Some(parent) => parent.child(name, count),
        None => NamePath::root(name, count),
    }
}

fn find_node<'a>(nodes: &'a [KdlNode], path: &NamePath) -> Option<&'a KdlNode> {
    let mut current_nodes = nodes;
    let mut current_node: Option<&KdlNode> = None;
    for (seg_idx, (name, index)) in path.0.iter().enumerate() {
        let mut count = 0usize;
        let mut found: Option<&KdlNode> = None;
        for node in current_nodes {
            if node.base_name() == name {
                if count == *index {
                    found = Some(node);
                    break;
                }
                count += 1;
            }
        }
        let node = found?;
        current_node = Some(node);
        if seg_idx + 1 < path.0.len() {
            if let Some(children) = node.children() {
                current_nodes = children.nodes();
            } else {
                return None;
            }
        }
    }
    current_node
}

fn find_node_mut<'a>(nodes: &'a mut [KdlNode], path: &NamePath) -> Option<&'a mut KdlNode> {
    fn find_child_index(nodes: &[KdlNode], name: &str, occurrence: usize) -> Option<usize> {
        let mut count = 0usize;
        for (idx, node) in nodes.iter().enumerate() {
            if node.base_name() == name {
                if count == occurrence {
                    return Some(idx);
                }
                count += 1;
            }
        }
        None
    }

    fn helper<'a>(
        nodes: &'a mut [KdlNode],
        path: &[(String, usize)],
        seg_idx: usize,
    ) -> Option<&'a mut KdlNode> {
        let (name, occurrence) = path.get(seg_idx)?;
        let idx = find_child_index(nodes, name, *occurrence)?;
        if seg_idx + 1 == path.len() {
            return Some(&mut nodes[idx]);
        }
        let children = nodes[idx].children_mut().as_mut()?;
        helper(children.nodes_mut(), path, seg_idx + 1)
    }

    helper(nodes, &path.0, 0)
}

pub(crate) fn apply_fragment_aware_updates(
    doc: &mut KdlDocument,
    baseline: &KdlDocument,
    updated: &KdlDocument,
    map: &FragmentMap,
) -> Result<(), KdlConfigError> {
    if map.uses.is_empty() && map.fragments.is_empty() {
        return Ok(());
    }

    #[derive(Debug, Clone)]
    struct UseSlices {
        fragment_name: String,
        parent_path: NamePath,
        use_node: KdlNode,
        insert_start: usize,
        patch_len: usize,
        fragment_len: usize,
        override_len: usize,
        attr_overrides: Vec<AttrOverride>,
        baseline_patch: Vec<KdlNode>,
        baseline_fragment: Vec<KdlNode>,
        updated_patch: Vec<KdlNode>,
        updated_fragment: Vec<KdlNode>,
        updated_attr_overrides: Vec<KdlNode>,
        updated_child_overrides: Vec<KdlNode>,
        mismatch: bool,
    }

    let mut uses_by_parent: HashMap<NamePath, Vec<&FragmentUse>> = HashMap::new();
    for use_info in &map.uses {
        uses_by_parent
            .entry(use_info.parent_path.clone())
            .or_default()
            .push(use_info);
    }
    for uses in uses_by_parent.values_mut() {
        uses.sort_by_key(|info| info.insert_start);
    }

    let mut collected: Vec<UseSlices> = Vec::new();

    for (parent_path, uses) in uses_by_parent.iter() {
        let baseline_parent = find_node(baseline.nodes(), parent_path).ok_or_else(|| {
            KdlConfigError::custom("KDL Document", "failed to locate fragment parent")
        })?;
        let updated_parent = find_node(updated.nodes(), parent_path).ok_or_else(|| {
            KdlConfigError::custom("KDL Document", "failed to locate fragment parent")
        })?;
        let baseline_children = baseline_parent
            .children()
            .map(|doc| doc.nodes())
            .unwrap_or(&[]);
        let updated_children = updated_parent
            .children()
            .map(|doc| doc.nodes())
            .unwrap_or(&[]);

        let mut base_cursor = 0usize;
        let mut updated_cursor = 0usize;
        for use_info in uses.iter() {
            let before_len = use_info.insert_start.saturating_sub(base_cursor);
            let before_end = (updated_cursor + before_len).min(updated_children.len());
            base_cursor = use_info.insert_start.min(baseline_children.len());
            updated_cursor = before_end;

            let patch_len = use_info.patch_len;
            let fragment_len = use_info.fragment_len;
            let override_len = use_info.override_len;
            let attr_override_len = use_info.attr_override_len;

            let baseline_patch_end = (base_cursor + patch_len).min(baseline_children.len());
            let baseline_fragment_end =
                (baseline_patch_end + fragment_len).min(baseline_children.len());
            let baseline_patch = baseline_children[base_cursor..baseline_patch_end].to_vec();
            let baseline_fragment =
                baseline_children[baseline_patch_end..baseline_fragment_end].to_vec();

            let updated_patch_end = (updated_cursor + patch_len).min(updated_children.len());
            let updated_fragment_end =
                (updated_patch_end + fragment_len).min(updated_children.len());
            let updated_attr_override_end =
                (updated_fragment_end + attr_override_len).min(updated_children.len());
            let updated_override_end =
                (updated_fragment_end + override_len).min(updated_children.len());
            let updated_patch = updated_children[updated_cursor..updated_patch_end].to_vec();
            let updated_fragment =
                updated_children[updated_patch_end..updated_fragment_end].to_vec();
            let updated_attr_overrides =
                updated_children[updated_fragment_end..updated_attr_override_end].to_vec();
            let updated_child_overrides =
                updated_children[updated_attr_override_end..updated_override_end].to_vec();

            let mismatch =
                updated_patch.len() != patch_len || updated_fragment.len() != fragment_len;

            collected.push(UseSlices {
                fragment_name: use_info.fragment_name.clone(),
                parent_path: parent_path.clone(),
                use_node: use_info.use_node.clone(),
                insert_start: use_info.insert_start,
                patch_len,
                fragment_len,
                override_len,
                attr_overrides: use_info.attr_overrides.clone(),
                baseline_patch,
                baseline_fragment,
                updated_patch,
                updated_fragment,
                updated_attr_overrides,
                updated_child_overrides,
                mismatch,
            });

            base_cursor = (base_cursor + patch_len + fragment_len + override_len)
                .min(baseline_children.len());
            updated_cursor = (updated_cursor + patch_len + fragment_len + override_len)
                .min(updated_children.len());
        }
    }

    let mut fragment_slices: HashMap<String, Vec<Vec<KdlNode>>> = HashMap::new();
    let mut fragment_mismatch: HashMap<String, bool> = HashMap::new();
    for use_slice in &collected {
        if use_slice.mismatch {
            fragment_mismatch.insert(use_slice.fragment_name.clone(), true);
            continue;
        }
        fragment_slices
            .entry(use_slice.fragment_name.clone())
            .or_default()
            .push(use_slice.updated_fragment.clone());
    }

    let mut updated_fragments: HashMap<String, Vec<KdlNode>> = HashMap::new();
    for (name, slices) in fragment_slices.iter() {
        if fragment_mismatch.get(name).copied().unwrap_or(false) {
            continue;
        }
        if slices.is_empty() {
            continue;
        }
        let first = &slices[0];
        if slices.iter().all(|slice| slice == first) {
            updated_fragments.insert(name.clone(), first.clone());
        }
    }

    let mut collected_by_parent: HashMap<NamePath, Vec<UseSlices>> = HashMap::new();
    for use_slice in collected.into_iter() {
        collected_by_parent
            .entry(use_slice.parent_path.clone())
            .or_default()
            .push(use_slice);
    }
    for uses in collected_by_parent.values_mut() {
        uses.sort_by_key(|info| info.insert_start);
    }

    for (parent_path, uses) in collected_by_parent {
        let updated_parent = find_node(updated.nodes(), &parent_path).ok_or_else(|| {
            KdlConfigError::custom("KDL Document", "failed to locate fragment parent")
        })?;
        let updated_children = updated_parent
            .children()
            .map(|doc| doc.nodes())
            .unwrap_or(&[]);

        let mut new_children: Vec<KdlNode> = Vec::new();
        let mut base_cursor = 0usize;
        let mut updated_cursor = 0usize;
        for use_info in uses {
            let before_len = use_info.insert_start.saturating_sub(base_cursor);
            let before_end = (updated_cursor + before_len).min(updated_children.len());
            if updated_cursor < before_end {
                new_children.extend(updated_children[updated_cursor..before_end].iter().cloned());
            }
            base_cursor = use_info.insert_start;
            updated_cursor = before_end;

            let override_children: Vec<KdlNode> =
                if updated_fragments.contains_key(&use_info.fragment_name) {
                    let patch_diff =
                        diff_fragment_children(&use_info.baseline_patch, &use_info.updated_patch);
                    patch_diff
                        .into_iter()
                        .chain(use_info.updated_child_overrides.clone())
                        .collect()
                } else {
                    let mut baseline_combined = use_info.baseline_patch.clone();
                    baseline_combined.extend(use_info.baseline_fragment.clone());
                    let mut updated_combined = use_info.updated_patch.clone();
                    updated_combined.extend(use_info.updated_fragment.clone());
                    diff_fragment_children(&baseline_combined, &updated_combined)
                        .into_iter()
                        .chain(use_info.updated_child_overrides.clone())
                        .collect()
                };

            let mut use_node = use_info.use_node.clone();
            apply_attr_overrides(
                &mut use_node,
                &use_info.attr_overrides,
                &use_info.updated_attr_overrides,
            );
            if override_children.is_empty() {
                use_node.clear_children();
            } else {
                let mut child_doc = KdlDocument::new();
                child_doc.nodes_mut().extend(override_children);
                use_node.set_children(child_doc);
            }
            new_children.push(use_node);

            base_cursor += use_info.patch_len + use_info.fragment_len + use_info.override_len;
            updated_cursor = (updated_cursor
                + use_info.patch_len
                + use_info.fragment_len
                + use_info.override_len)
                .min(updated_children.len());
        }

        if updated_cursor < updated_children.len() {
            new_children.extend(updated_children[updated_cursor..].iter().cloned());
        }

        if let Some(parent) = find_node_mut(doc.nodes_mut(), &parent_path) {
            if new_children.is_empty() {
                parent.clear_children();
            } else {
                let mut child_doc = KdlDocument::new();
                child_doc.nodes_mut().extend(new_children);
                parent.set_children(child_doc);
            }
        }
    }

    if !map.fragments.is_empty() {
        let mut placements_by_parent: HashMap<Option<NamePath>, Vec<&FragmentPlacement>> =
            HashMap::new();
        for placement in &map.fragments {
            placements_by_parent
                .entry(placement.parent_path.clone())
                .or_default()
                .push(placement);
        }

        for placements in placements_by_parent.values_mut() {
            placements.sort_by_key(|placement| placement.index);
        }

        for (parent_path, placements) in placements_by_parent {
            let nodes = match parent_path {
                None => doc.nodes_mut(),
                Some(path) => {
                    let parent = find_node_mut(doc.nodes_mut(), &path).ok_or_else(|| {
                        KdlConfigError::custom("KDL Document", "failed to locate fragment parent")
                    })?;
                    parent.ensure_children().nodes_mut()
                }
            };

            let mut offset = 0usize;
            for placement in placements {
                let mut node = placement.node.clone();
                if let Some(updated_children) = updated_fragments.get(&placement.fragment_name) {
                    update_fragment_inserts(&mut node, updated_children);
                }
                let insert_at = (placement.index + offset).min(nodes.len());
                nodes.insert(insert_at, node);
                offset += 1;
            }
        }
    }

    Ok(())
}

#[derive(Debug, Clone)]
struct ParsedUse {
    name: String,
    attr_overrides: Vec<AttrOverride>,
    attr_nodes: Vec<KdlNode>,
    child_overrides: Vec<KdlNode>,
}

fn parse_fragment_patch(
    node: &KdlNode,
    location: Option<&dyn LocationProvider>,
) -> Result<FragmentPatch, KdlConfigError> {
    let mut patch = strip_modifier(node.clone());
    let discriminator = match patch.arg(0) {
        Some(KdlValue::String(value)) => Some(value.clone()),
        Some(other) => {
            return Err(fragment_error(
                format!(
                    "fragment patch discriminator must be a string, found {}",
                    kdl_value_type(other)
                ),
                Some(node),
                location,
            ));
        }
        None => None,
    };

    if patch.args().len() > 1 {
        return Err(fragment_error(
            "fragment patch nodes accept at most one discriminator argument",
            Some(node),
            location,
        ));
    }

    if patch.args().len() == 1 {
        remove_positional_entry(&mut patch, 0);
    }

    Ok(FragmentPatch {
        target: node.base_name().to_string(),
        discriminator,
        defined_at: node_location(node, location),
        node: patch,
    })
}

fn parse_use(
    node: &KdlNode,
    location: Option<&dyn LocationProvider>,
) -> Result<ParsedUse, KdlConfigError> {
    let name_value = node.arg(0).ok_or_else(|| {
        fragment_error(
            "use nodes must have a fragment name argument",
            Some(node),
            location,
        )
    })?;

    let name = match name_value {
        KdlValue::String(value) => value.clone(),
        other => {
            return Err(fragment_error(
                format!(
                    "fragment name must be a string, found {}",
                    kdl_value_type(other)
                ),
                Some(node),
                location,
            ));
        }
    };

    let mut token_overrides = Vec::new();
    for (arg_index, arg) in node.args().into_iter().enumerate().skip(1) {
        match arg {
            KdlValue::String(token) => {
                token_overrides.push(KdlNode::new(token.clone()));
            }
            other => {
                return Err(fragment_error(
                    format!(
                        "use override token at arg[{arg_index}] must be a string, found {}",
                        kdl_value_type(other)
                    ),
                    Some(node),
                    location,
                ));
            }
        }
    }

    let (attr_overrides, attr_nodes) = parse_attr_overrides(node, location)?;
    let mut child_overrides = token_overrides;
    child_overrides.extend(
        node.children()
            .map(|doc| doc.nodes().to_vec())
            .unwrap_or_default(),
    );

    Ok(ParsedUse {
        name,
        attr_overrides,
        attr_nodes,
        child_overrides,
    })
}

fn parse_attr_overrides(
    node: &KdlNode,
    location: Option<&dyn LocationProvider>,
) -> Result<(Vec<AttrOverride>, Vec<KdlNode>), KdlConfigError> {
    let mut seen = HashSet::new();
    let mut overrides = Vec::new();
    for entry in node.entries() {
        if let Some(name) = entry.name() {
            let key = name.value().to_string();
            if !seen.insert(key.clone()) {
                return Err(fragment_error(
                    format!("duplicate use attribute override {:?}", key),
                    Some(node),
                    location,
                ));
            }
            let path = parse_attr_override_path(&key, node, location)?;
            overrides.push(AttrOverrideValue {
                key,
                path,
                value: entry.value().clone(),
            });
        }
    }

    let nodes = build_attr_override_nodes(&overrides, node, location)?;
    let attr_overrides = overrides
        .iter()
        .map(|override_value| AttrOverride {
            key: override_value.key.clone(),
            path: override_value.path.clone(),
        })
        .collect();

    Ok((attr_overrides, nodes))
}

fn parse_attr_override_path(
    key: &str,
    node: &KdlNode,
    location: Option<&dyn LocationProvider>,
) -> Result<Vec<String>, KdlConfigError> {
    let parts: Vec<&str> = key.split('.').collect();
    if parts.iter().any(|part| part.is_empty()) {
        return Err(fragment_error(
            format!("invalid use attribute override {:?}", key),
            Some(node),
            location,
        ));
    }
    Ok(parts.into_iter().map(|part| part.to_string()).collect())
}

fn build_attr_override_nodes(
    overrides: &[AttrOverrideValue],
    node: &KdlNode,
    location: Option<&dyn LocationProvider>,
) -> Result<Vec<KdlNode>, KdlConfigError> {
    let mut roots: Vec<OverrideNode> = Vec::new();
    for override_value in overrides {
        insert_attr_override(
            &mut roots,
            &override_value.path,
            &override_value.value,
            node,
            location,
        )?;
    }
    Ok(roots.into_iter().map(override_node_to_kdl).collect())
}

fn insert_attr_override(
    nodes: &mut Vec<OverrideNode>,
    path: &[String],
    value: &KdlValue,
    node: &KdlNode,
    location: Option<&dyn LocationProvider>,
) -> Result<(), KdlConfigError> {
    let mut current = nodes;
    for (idx, segment) in path.iter().enumerate() {
        let is_leaf = idx + 1 == path.len();
        match current.iter().position(|node| node.name == *segment) {
            Some(index) => {
                if is_leaf {
                    if !current[index].children.is_empty() {
                        return Err(fragment_error(
                            format!(
                                "conflicting use attribute overrides for {:?}",
                                path.join(".")
                            ),
                            Some(node),
                            location,
                        ));
                    }
                    current[index].value = Some(value.clone());
                } else {
                    if current[index].value.is_some() {
                        return Err(fragment_error(
                            format!(
                                "conflicting use attribute overrides for {:?}",
                                path.join(".")
                            ),
                            Some(node),
                            location,
                        ));
                    }
                    let children = &mut current[index].children;
                    current = children;
                }
            }
            None => {
                let mut created = OverrideNode {
                    name: segment.clone(),
                    value: None,
                    children: Vec::new(),
                };
                if is_leaf {
                    created.value = Some(value.clone());
                }
                current.push(created);
                if !is_leaf {
                    let last_index = current.len().saturating_sub(1);
                    current = &mut current[last_index].children;
                }
            }
        }
    }
    Ok(())
}

fn override_node_to_kdl(node: OverrideNode) -> KdlNode {
    let mut kdl_node = KdlNode::new(node.name);
    if let Some(value) = node.value {
        kdl_node.entries_mut().push(KdlEntry::new(value));
    }
    if !node.children.is_empty() {
        let mut doc = KdlDocument::new();
        doc.nodes_mut()
            .extend(node.children.into_iter().map(override_node_to_kdl));
        kdl_node.set_children(doc);
    }
    kdl_node
}

fn apply_attr_overrides(
    use_node: &mut KdlNode,
    overrides: &[AttrOverride],
    override_nodes: &[KdlNode],
) {
    for override_entry in overrides {
        if let Some(value) = find_override_value(override_nodes, &override_entry.path) {
            update_or_insert_attr(use_node, &override_entry.key, value);
        } else {
            remove_attr_entries(use_node, &override_entry.key);
        }
    }
}

fn find_override_value(nodes: &[KdlNode], path: &[String]) -> Option<KdlValue> {
    if path.is_empty() {
        return None;
    }
    let mut current = nodes;
    for (idx, segment) in path.iter().enumerate() {
        let node = current.iter().find(|node| node.base_name() == segment)?;
        if idx + 1 == path.len() {
            return node.arg(0).cloned();
        }
        let children = node.children()?;
        current = children.nodes();
    }
    None
}

fn diff_fragment_children(baseline: &[KdlNode], updated: &[KdlNode]) -> Vec<KdlNode> {
    let mut ordered_names = Vec::new();
    let mut seen = HashSet::new();
    for node in baseline.iter().chain(updated.iter()) {
        let name = node.base_name().to_string();
        if seen.insert(name.clone()) {
            ordered_names.push(name);
        }
    }

    let mut diff = Vec::new();
    for name in ordered_names {
        let baseline_nodes: Vec<&KdlNode> = baseline
            .iter()
            .filter(|node| node.base_name() == name)
            .collect();
        let updated_nodes: Vec<&KdlNode> = updated
            .iter()
            .filter(|node| node.base_name() == name)
            .collect();

        if baseline_nodes == updated_nodes {
            continue;
        }

        if !baseline_nodes.is_empty() {
            diff.push(KdlNode::new(format!("-{}", name)));
        }

        for node in updated_nodes {
            diff.push(node.clone());
        }
    }

    diff
}

fn update_fragment_inserts(node: &mut KdlNode, updated_inserts: &[KdlNode]) {
    let existing_children = node.children_mut().take().unwrap_or_else(KdlDocument::new);
    let mut new_children: Vec<KdlNode> = Vec::new();
    let mut inserts = updated_inserts.iter();

    for child in existing_children.nodes().iter() {
        match child.modifier() {
            Modifier::Inherit => {
                if let Some(updated) = inserts.next() {
                    new_children.push(updated.clone());
                }
            }
            Modifier::Flatten => {
                new_children.push(child.clone());
            }
            _ => {
                new_children.push(child.clone());
            }
        }
    }

    for remaining in inserts {
        new_children.push(remaining.clone());
    }

    if new_children.is_empty() {
        node.clear_children();
    } else {
        let mut child_doc = KdlDocument::new();
        child_doc.nodes_mut().extend(new_children);
        node.set_children(child_doc);
    }
}

fn kdl_value_type(value: &KdlValue) -> &'static str {
    match value {
        KdlValue::String(_) => "string",
        KdlValue::Integer(_) => "int",
        KdlValue::Float(_) => "float",
        KdlValue::Bool(_) => "bool",
        KdlValue::Null => "null",
    }
}

fn node_location(node: &KdlNode, location: Option<&dyn LocationProvider>) -> Option<NodeLocation> {
    location.map(|source| source.location(node.span().offset()))
}

fn format_location(location: Option<NodeLocation>) -> Option<String> {
    location.map(|loc| format!("line {}, column {}", loc.line, loc.column))
}

fn fragment_error(
    message: impl Into<String>,
    node: Option<&KdlNode>,
    location: Option<&dyn LocationProvider>,
) -> KdlConfigError {
    let mut err = KdlConfigError::custom("KDL Document", message);
    if let (Some(node), Some(location)) = (node, location) {
        let offset = node.span().offset();
        err.location = Some(location.location(offset));
    }
    err
}
