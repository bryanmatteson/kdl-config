use crate::context::LineIndex;
use crate::error::KdlConfigError;
use crate::fragments::expand_fragments;
use crate::node_ext::KdlNodeExt;
use crate::render::is_valid_identifier;
use crate::types::{Node, Value};
use kdl::{KdlDocument, KdlNode, KdlValue};

pub fn parse_config(contents: &str) -> Result<Node, KdlConfigError> {
    let mut document: KdlDocument = contents
        .parse()
        .map_err(|e: kdl::KdlError| KdlConfigError::from_kdl_parse_error("KDL Document", e))?;

    let mut root = Node::new();
    let index = LineIndex::new(contents);

    expand_fragments(&mut document, Some(&index))?;

    for (idx, node) in document.nodes().iter().enumerate() {
        let child = parse_kdl_node(node, &index, None, idx)?;
        root.add_child(child);
    }

    Ok(root)
}

pub fn parse_node(contents: &str) -> Result<Node, KdlConfigError> {
    let mut document: KdlDocument = contents
        .parse()
        .map_err(|e: kdl::KdlError| KdlConfigError::from_kdl_parse_error("KDL Node", e))?;

    expand_fragments(&mut document, Some(&LineIndex::new(contents)))?;
    let nodes = document.nodes();
    match nodes.len() {
        0 => Err(KdlConfigError::custom(
            "KDL Node",
            "expected a single node, found none",
        )),
        1 => {
            let index = LineIndex::new(contents);
            parse_kdl_node(&nodes[0], &index, None, 0)
        }
        count => Err(KdlConfigError::custom(
            "KDL Node",
            format!("expected a single node, found {count}"),
        )),
    }
}

pub(crate) fn parse_kdl_node(
    node: &KdlNode,
    index: &LineIndex,
    parent_path: Option<&str>,
    child_index: usize,
) -> Result<Node, KdlConfigError> {
    let name = node.base_name().to_string();
    let modifier = node.modifier();
    let repr = node.name().repr();
    let path = match parent_path {
        Some(parent) => format!("{parent}/{name}[{child_index}]"),
        None => format!("/{name}[{child_index}]"),
    };
    let location = node.span().offset();
    let mut result = Node::named(name)
        .with_modifier(modifier)
        .with_location(index.location(location))
        .with_path(path);
    if let Some(repr) = repr {
        if repr.starts_with('"') || repr.starts_with('#') || repr.starts_with('r') {
            result = result.with_name_repr(repr.to_string());
        }
    }

    for entry in node.entries() {
        if let Some(name) = entry.name() {
            let key = name.value().to_string();
            if let Some(repr) = name.repr() {
                if repr.starts_with('"') || repr.starts_with('#') || repr.starts_with('r') {
                    result.set_attr_repr(key.clone(), repr.to_string());
                }
            }
            let value = kdl_value_to_value(entry.value())?;
            result.set_attr(key, value);
        } else {
            let value = kdl_value_to_value(entry.value())?;
            let repr = match &value {
                Value::String(s) if is_valid_identifier(s) => Some(s.clone()),
                _ => None,
            };
            result.add_arg_with_repr(value, repr);
        }
    }

    if let Some(children) = node.children() {
        for (idx, child) in children.nodes().iter().enumerate() {
            let child_node = parse_kdl_node(child, index, result.path(), idx)?;
            result.add_child(child_node);
        }
    }

    Ok(result)
}

fn kdl_value_to_value(val: &KdlValue) -> Result<Value, KdlConfigError> {
    match val {
        KdlValue::String(s) => Ok(Value::String(s.to_string())),
        KdlValue::Integer(i) => Ok(Value::Int(*i)),
        KdlValue::Float(f) => Ok(Value::Float(*f)),
        KdlValue::Bool(b) => Ok(Value::Bool(*b)),
        KdlValue::Null => Ok(Value::Null),
    }
}
