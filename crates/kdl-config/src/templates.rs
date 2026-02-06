use std::collections::HashMap;

use kdl::{KdlDocument, KdlNode, KdlValue};

use crate::context::{LineIndex, Source};
use crate::error::{suggest_similar, KdlConfigError};
use crate::node_ext::KdlNodeExt;
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
struct TemplateDef {
    name: String,
    schema_type: String,
    children: Vec<KdlNode>,
}

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
pub struct TemplateExpansion {
    pub had_templates: bool,
    pub had_uses: bool,
}

impl TemplateExpansion {
    pub fn modified(&self) -> bool {
        self.had_templates || self.had_uses
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
pub(crate) struct TemplateUse {
    pub template_name: String,
    pub parent_path: NamePath,
    pub insert_start: usize,
    pub insert_len: usize,
    pub use_node: KdlNode,
}

#[derive(Debug, Clone)]
pub(crate) struct TemplatePlacement {
    pub template_name: String,
    pub parent_path: Option<NamePath>,
    pub index: usize,
    pub node: KdlNode,
}

#[derive(Debug, Default, Clone)]
pub(crate) struct TemplateMap {
    pub uses: Vec<TemplateUse>,
    pub templates: Vec<TemplatePlacement>,
}

pub fn expand_templates(
    doc: &mut KdlDocument,
    location: Option<&dyn LocationProvider>,
) -> Result<TemplateExpansion, KdlConfigError> {
    let mut templates = HashMap::new();
    let mut had_templates = false;
    collect_templates_in_nodes(doc.nodes_mut(), &mut templates, location, &mut had_templates)?;
    if doc.nodes().len() == 1 {
        if let Some(children) = doc.nodes_mut()[0].children_mut().as_mut() {
            collect_templates_in_nodes(
                children.nodes_mut(),
                &mut templates,
                location,
                &mut had_templates,
            )?;
        }
    }
    let mut expansion = TemplateExpansion {
        had_templates,
        had_uses: false,
    };
    let mut stack = Vec::new();
    expand_nodes(
        doc.nodes_mut(),
        None,
        &templates,
        location,
        &mut expansion,
        &mut stack,
    )?;
    Ok(expansion)
}

pub(crate) fn expand_templates_with_map(
    doc: &mut KdlDocument,
    location: Option<&dyn LocationProvider>,
) -> Result<(TemplateExpansion, TemplateMap), KdlConfigError> {
    let mut map = TemplateMap::default();
    let mut templates = HashMap::new();
    let mut had_templates = false;
    collect_templates_in_nodes_with_map(
        doc.nodes_mut(),
        None,
        &mut templates,
        location,
        &mut had_templates,
        &mut map,
    )?;

    let root_path = if doc.nodes().len() == 1 {
        let node = &doc.nodes()[0];
        Some(NamePath::root(node.base_name(), 0))
    } else {
        None
    };

    if let Some(root_path) = root_path.clone() {
        if let Some(children) = doc.nodes_mut()[0].children_mut().as_mut() {
            collect_templates_in_nodes_with_map(
                children.nodes_mut(),
                Some(root_path),
                &mut templates,
                location,
                &mut had_templates,
                &mut map,
            )?;
        }
    }

    let mut expansion = TemplateExpansion {
        had_templates,
        had_uses: false,
    };
    let mut stack = Vec::new();
    expand_nodes_with_map(
        doc.nodes_mut(),
        None,
        &templates,
        location,
        &mut expansion,
        &mut stack,
        &mut map,
    )?;
    Ok((expansion, map))
}

fn collect_templates_in_nodes(
    nodes: &mut Vec<KdlNode>,
    templates: &mut HashMap<String, TemplateDef>,
    location: Option<&dyn LocationProvider>,
    had_templates: &mut bool,
) -> Result<(), KdlConfigError> {
    let mut kept = Vec::with_capacity(nodes.len());
    for node in nodes.iter() {
        if node.base_name() == "template" {
            if node.modifier() != Modifier::Inherit {
                return Err(template_error(
                    "modifiers are not allowed on template nodes",
                    Some(node),
                    location,
                ));
            }
            *had_templates = true;
            let template = parse_template(node, location)?;
            if templates.contains_key(&template.name) {
                return Err(template_error(
                    format!("duplicate template name {:?}", template.name),
                    Some(node),
                    location,
                ));
            }
            templates.insert(template.name.clone(), template);
        } else {
            kept.push(node.clone());
        }
    }
    *nodes = kept;
    Ok(())
}

fn collect_templates_in_nodes_with_map(
    nodes: &mut Vec<KdlNode>,
    parent_path: Option<NamePath>,
    templates: &mut HashMap<String, TemplateDef>,
    location: Option<&dyn LocationProvider>,
    had_templates: &mut bool,
    map: &mut TemplateMap,
) -> Result<(), KdlConfigError> {
    let mut kept = Vec::with_capacity(nodes.len());
    for node in nodes.iter() {
        if node.base_name() == "template" {
            if node.modifier() != Modifier::Inherit {
                return Err(template_error(
                    "modifiers are not allowed on template nodes",
                    Some(node),
                    location,
                ));
            }
            *had_templates = true;
            let template = parse_template(node, location)?;
            if templates.contains_key(&template.name) {
                return Err(template_error(
                    format!("duplicate template name {:?}", template.name),
                    Some(node),
                    location,
                ));
            }
            let template_name = template.name.clone();
            templates.insert(template_name.clone(), template);
            map.templates.push(TemplatePlacement {
                template_name,
                parent_path: parent_path.clone(),
                index: kept.len(),
                node: node.clone(),
            });
        } else {
            kept.push(node.clone());
        }
    }
    *nodes = kept;
    Ok(())
}

fn parse_template(
    node: &KdlNode,
    location: Option<&dyn LocationProvider>,
) -> Result<TemplateDef, KdlConfigError> {
    let template_type = node.ty().map(|ident| ident.value().to_string()).ok_or_else(|| {
        template_error(
            "template nodes must have a type annotation like (source)template",
            Some(node),
            location,
        )
    })?;

    let name_value = node.arg(0).ok_or_else(|| {
        template_error(
            "template nodes must have a name argument",
            Some(node),
            location,
        )
    })?;

    let name = match name_value {
        KdlValue::String(value) => value.clone(),
        other => {
            return Err(template_error(
                format!(
                    "template name must be a string, found {}",
                    kdl_value_type(other)
                ),
                Some(node),
                location,
            ))
        }
    };

    if node.args().len() > 1 {
        return Err(template_error(
            "template nodes accept only a single name argument",
            Some(node),
            location,
        ));
    }

    if !node.attrs().is_empty() {
        return Err(template_error(
            "template nodes do not accept attributes",
            Some(node),
            location,
        ));
    }

    let children = node
        .children()
        .map(|doc| doc.nodes().to_vec())
        .unwrap_or_default();

    Ok(TemplateDef {
        name,
        schema_type: template_type,
        children,
    })
}

fn expand_nodes(
    nodes: &mut Vec<KdlNode>,
    parent: Option<&ParentInfo>,
    templates: &HashMap<String, TemplateDef>,
    location: Option<&dyn LocationProvider>,
    expansion: &mut TemplateExpansion,
    stack: &mut Vec<String>,
) -> Result<(), KdlConfigError> {
    let mut idx = 0usize;
    while idx < nodes.len() {
        if nodes[idx].base_name() == "use" {
            if nodes[idx].modifier() != Modifier::Inherit {
                return Err(template_error(
                    "modifiers are not allowed on use nodes",
                    Some(&nodes[idx]),
                    location,
                ));
            }
            let template_name = parse_use_name(&nodes[idx], location)?;
            let template = templates.get(&template_name).ok_or_else(|| {
                let candidates: Vec<&str> = templates.keys().map(|key| key.as_str()).collect();
                let mut message = format!("unknown template {:?}", template_name);
                if let Some(suggestion) = suggest_similar(&template_name, &candidates) {
                    message.push_str(&format!(" (did you mean {:?}?)", suggestion));
                }
                template_error(message, Some(&nodes[idx]), location)
            })?;

            let parent = parent.ok_or_else(|| {
                template_error(
                    "use nodes must be inside a parent node",
                    Some(&nodes[idx]),
                    location,
                )
            })?;

            let expected_type = parent.expected_type();
            if template.schema_type != expected_type {
                return Err(template_error(
                    format!(
                        "template {:?} is typed as {:?} but used under {:?}",
                        template.name, template.schema_type, expected_type
                    ),
                    Some(&nodes[idx]),
                    location,
                ));
            }

            if stack.contains(&template_name) {
                let mut chain = stack.clone();
                chain.push(template_name.clone());
                return Err(template_error(
                    format!("template recursion detected: {}", chain.join(" -> ")),
                    Some(&nodes[idx]),
                    location,
                ));
            }

            expansion.had_uses = true;
            let mut inserted = template.children.clone();
            stack.push(template_name);
            expand_nodes(
                &mut inserted,
                Some(parent),
                templates,
                location,
                expansion,
                stack,
            )?;
            stack.pop();

            let inserted_len = inserted.len();
            nodes.splice(idx..idx + 1, inserted);
            idx += inserted_len;
            continue;
        }

        let parent_info = ParentInfo::from_node(&nodes[idx]);
        if let Some(children) = nodes[idx].children_mut().as_mut() {
            expand_nodes(
                children.nodes_mut(),
                Some(&parent_info),
                templates,
                location,
                expansion,
                stack,
            )?;
        }
        idx += 1;
    }

    Ok(())
}

fn expand_nodes_with_map(
    nodes: &mut Vec<KdlNode>,
    parent_path: Option<&NamePath>,
    templates: &HashMap<String, TemplateDef>,
    location: Option<&dyn LocationProvider>,
    expansion: &mut TemplateExpansion,
    stack: &mut Vec<String>,
    map: &mut TemplateMap,
) -> Result<(), KdlConfigError> {
    let mut idx = 0usize;
    while idx < nodes.len() {
        if nodes[idx].base_name() == "use" {
            if nodes[idx].modifier() != Modifier::Inherit {
                return Err(template_error(
                    "modifiers are not allowed on use nodes",
                    Some(&nodes[idx]),
                    location,
                ));
            }
            let template_name = parse_use_name(&nodes[idx], location)?;
            let template = templates.get(&template_name).ok_or_else(|| {
                let candidates: Vec<&str> = templates.keys().map(|key| key.as_str()).collect();
                let mut message = format!("unknown template {:?}", template_name);
                if let Some(suggestion) = suggest_similar(&template_name, &candidates) {
                    message.push_str(&format!(" (did you mean {:?}?)", suggestion));
                }
                template_error(message, Some(&nodes[idx]), location)
            })?;

            let parent_path = parent_path.ok_or_else(|| {
                template_error(
                    "use nodes must be inside a parent node",
                    Some(&nodes[idx]),
                    location,
                )
            })?;

            if stack.contains(&template_name) {
                let mut chain = stack.clone();
                chain.push(template_name.clone());
                return Err(template_error(
                    format!("template recursion detected: {}", chain.join(" -> ")),
                    Some(&nodes[idx]),
                    location,
                ));
            }

            expansion.had_uses = true;
            let mut inserted = template.children.clone();
            stack.push(template_name.clone());
            expand_nodes_with_map(
                &mut inserted,
                Some(parent_path),
                templates,
                location,
                expansion,
                stack,
                map,
            )?;
            stack.pop();

            let inserted_len = inserted.len();
            let use_node = nodes[idx].clone();
            nodes.splice(idx..idx + 1, inserted);
            map.uses.push(TemplateUse {
                template_name,
                parent_path: parent_path.clone(),
                insert_start: idx,
                insert_len: inserted_len,
                use_node,
            });
            idx += inserted_len;
            continue;
        }

        let child_path = child_path(parent_path, nodes, idx);
        if let Some(children) = nodes[idx].children_mut().as_mut() {
            expand_nodes_with_map(
                children.nodes_mut(),
                Some(&child_path),
                templates,
                location,
                expansion,
                stack,
                map,
            )?;
        }
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

    fn helper<'a>(nodes: &'a mut [KdlNode], path: &[(String, usize)], seg_idx: usize) -> Option<&'a mut KdlNode> {
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

pub(crate) fn apply_template_aware_updates(
    doc: &mut KdlDocument,
    expanded: &KdlDocument,
    map: &TemplateMap,
) -> Result<(), KdlConfigError> {
    if map.uses.is_empty() && map.templates.is_empty() {
        return Ok(());
    }

    let mut template_slices: HashMap<String, Vec<Vec<KdlNode>>> = HashMap::new();
    for use_info in &map.uses {
        let parent = find_node(expanded.nodes(), &use_info.parent_path).ok_or_else(|| {
            KdlConfigError::custom("KDL Document", "failed to locate template parent")
        })?;
        let children = parent
            .children()
            .map(|doc| doc.nodes())
            .unwrap_or(&[]);
        let start = use_info.insert_start.min(children.len());
        let end = (use_info.insert_start + use_info.insert_len).min(children.len());
        let slice = children[start..end].to_vec();
        template_slices
            .entry(use_info.template_name.clone())
            .or_default()
            .push(slice);
    }

    let mut updated_templates: HashMap<String, Vec<KdlNode>> = HashMap::new();
    for (name, slices) in template_slices.iter() {
        if slices.is_empty() {
            continue;
        }
        let first = &slices[0];
        if slices.iter().all(|slice| slice == first) {
            updated_templates.insert(name.clone(), first.clone());
        }
    }

    let mut uses_by_parent: HashMap<NamePath, Vec<&TemplateUse>> = HashMap::new();
    for use_info in &map.uses {
        uses_by_parent
            .entry(use_info.parent_path.clone())
            .or_default()
            .push(use_info);
    }
    for uses in uses_by_parent.values_mut() {
        uses.sort_by_key(|info| info.insert_start);
    }

    for (parent_path, uses) in uses_by_parent {
        let expanded_parent = find_node(expanded.nodes(), &parent_path).ok_or_else(|| {
            KdlConfigError::custom("KDL Document", "failed to locate template parent")
        })?;
        let expanded_children = expanded_parent
            .children()
            .map(|doc| doc.nodes())
            .unwrap_or(&[]);

        let mut new_children: Vec<KdlNode> = Vec::new();
        let mut cursor = 0usize;
        for use_info in uses {
            let start = use_info.insert_start.min(expanded_children.len());
            let end = (use_info.insert_start + use_info.insert_len).min(expanded_children.len());
            if cursor < start {
                new_children.extend(expanded_children[cursor..start].iter().cloned());
            }
            new_children.push(use_info.use_node.clone());
            if !updated_templates.contains_key(&use_info.template_name) {
                new_children.extend(expanded_children[start..end].iter().cloned());
            }
            cursor = end;
        }
        if cursor < expanded_children.len() {
            new_children.extend(expanded_children[cursor..].iter().cloned());
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

    if !map.templates.is_empty() {
        let mut placements_by_parent: HashMap<Option<NamePath>, Vec<&TemplatePlacement>> =
            HashMap::new();
        for placement in &map.templates {
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
                    let parent =
                        find_node_mut(doc.nodes_mut(), &path).ok_or_else(|| {
                            KdlConfigError::custom(
                                "KDL Document",
                                "failed to locate template parent",
                            )
                        })?;
                    parent.ensure_children().nodes_mut()
                }
            };

            let mut offset = 0usize;
            for placement in placements {
                let mut node = placement.node.clone();
                if let Some(updated_children) = updated_templates.get(&placement.template_name) {
                    if updated_children.is_empty() {
                        node.clear_children();
                    } else {
                        let mut child_doc = KdlDocument::new();
                        child_doc.nodes_mut().extend(updated_children.clone());
                        node.set_children(child_doc);
                    }
                }
                let insert_at = (placement.index + offset).min(nodes.len());
                nodes.insert(insert_at, node);
                offset += 1;
            }
        }
    }

    Ok(())
}

fn parse_use_name(
    node: &KdlNode,
    location: Option<&dyn LocationProvider>,
) -> Result<String, KdlConfigError> {
    let name_value = node.arg(0).ok_or_else(|| {
        template_error(
            "use nodes must have a template name argument",
            Some(node),
            location,
        )
    })?;

    let name = match name_value {
        KdlValue::String(value) => value.clone(),
        other => {
            return Err(template_error(
                format!(
                    "template name must be a string, found {}",
                    kdl_value_type(other)
                ),
                Some(node),
                location,
            ))
        }
    };

    if node.args().len() > 1 {
        return Err(template_error(
            "use nodes accept only a single name argument",
            Some(node),
            location,
        ));
    }

    if !node.attrs().is_empty() {
        return Err(template_error(
            "use nodes do not accept attributes",
            Some(node),
            location,
        ));
    }

    if node.children().is_some() {
        return Err(template_error(
            "use nodes cannot have children",
            Some(node),
            location,
        ));
    }

    Ok(name)
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

fn template_error(
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
