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

pub fn expand_templates(
    doc: &mut KdlDocument,
    location: Option<&dyn LocationProvider>,
) -> Result<TemplateExpansion, KdlConfigError> {
    let (templates, had_templates) = collect_templates(doc, location)?;
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

fn collect_templates(
    doc: &mut KdlDocument,
    location: Option<&dyn LocationProvider>,
) -> Result<(HashMap<String, TemplateDef>, bool), KdlConfigError> {
    let mut templates = HashMap::new();
    let mut kept = Vec::with_capacity(doc.nodes().len());
    let mut had_templates = false;

    for node in doc.nodes().iter() {
        if node.base_name() == "template" {
            if node.modifier() != Modifier::Inherit {
                return Err(template_error(
                    "modifiers are not allowed on template nodes",
                    Some(node),
                    location,
                ));
            }
            had_templates = true;
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

    *doc.nodes_mut() = kept;
    Ok((templates, had_templates))
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
