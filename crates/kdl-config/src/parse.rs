use crate::error::{ErrorKind, KdlConfigError, Placement};
use crate::render::is_valid_identifier;
use crate::types::{Modifier, Node, Value};
use kdl::{KdlDocument, KdlNode, KdlValue};

pub fn parse_config(contents: &str) -> Result<Node, KdlConfigError> {
    let document: KdlDocument = contents
        .parse()
        .map_err(|e: kdl::KdlError| KdlConfigError {
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
    let name_ident = node.name();
    let raw_name = name_ident.value();
    let repr = name_ident.repr();
    let is_quoted = repr
        .map(|repr| {
            repr.starts_with('"')
                || repr.starts_with('#')
                || repr.starts_with('r')
                || repr.is_empty()
        })
        .unwrap_or(false);
    let (name, modifier) = if is_quoted {
        (raw_name.to_string(), Modifier::Inherit)
    } else {
        parse_node_name(raw_name)
    };
    let mut result = Node::named(name).with_modifier(modifier);
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
            let mut repr = entry.value().as_string().map(|repr| repr.to_string());
            if repr.is_none() {
                if let Value::String(s) = &value {
                    if is_valid_identifier(s) {
                        repr = Some(s.clone());
                    }
                }
            }
            result.add_arg_with_repr(value, repr);
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

fn kdl_value_to_value(val: &KdlValue) -> Result<Value, KdlConfigError> {
    match val {
        KdlValue::String(s) => Ok(Value::String(s.to_string())),
        KdlValue::Integer(i) => Ok(Value::Int(*i)),
        KdlValue::Float(f) => Ok(Value::Float(*f)),
        KdlValue::Bool(b) => Ok(Value::Bool(*b)),
        KdlValue::Null => Ok(Value::Null),
    }
}
