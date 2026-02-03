use crate::error::{ErrorKind, KdlConfigError, Placement};
use crate::types::{Modifier, Node, Value};
use kdl::{KdlDocument, KdlNode, KdlValue};

pub fn parse_config(contents: &str) -> Result<Node, KdlConfigError> {
    let document: KdlDocument = contents.parse().map_err(|e: kdl::KdlError| KdlConfigError {
        struct_name: "KDL Document".into(),
        field_name: None,
        kdl_key: None,
        placement: Placement::Unknown,
        required: true,
        kind: ErrorKind::Parse(e.to_string()),
    })?;

    let mut root = Node::new();

    for node in document.nodes() {
        let child = parse_kdl_node(node)?;
        root.add_child(child);
    }

    Ok(root)
}

fn parse_kdl_node(node: &KdlNode) -> Result<Node, KdlConfigError> {
    let (name, modifier) = parse_node_name(node.name().value());
    let mut result = Node::named(name).with_modifier(modifier);

    for entry in node.entries() {
        if let Some(name) = entry.name() {
            let key = name.value().to_string();
            if let Some(value) = kdl_value_to_value(entry.value()) {
                result.set_attr(key, value);
            }
        } else if let Some(value) = kdl_value_to_value(entry.value()) {
            result.add_arg(value);
        }
    }

    if let Some(children) = node.children() {
        for child in children.nodes() {
            let child_node = parse_kdl_node(child)?;
            result.add_child(child_node);
        }
    }

    Ok(result)
}

fn parse_node_name(raw: &str) -> (String, Modifier) {
    if let Some(stripped) = raw.strip_prefix('+') {
        (stripped.to_string(), Modifier::Append)
    } else if let Some(stripped) = raw.strip_prefix('-') {
        (stripped.to_string(), Modifier::Remove)
    } else if let Some(stripped) = raw.strip_prefix('!') {
        (stripped.to_string(), Modifier::Replace)
    } else {
        (raw.to_string(), Modifier::Inherit)
    }
}

fn kdl_value_to_value(val: &KdlValue) -> Option<Value> {
    match val {
        KdlValue::String(s) => Some(Value::String(s.to_string())),
        KdlValue::Integer(i) => Some(Value::Int(*i as i64)),
        KdlValue::Float(f) => Some(Value::Float(*f)),
        KdlValue::Bool(b) => Some(Value::Bool(*b)),
        KdlValue::Null => Some(Value::Null),
    }
}
