use std::fmt::Write;

use crate::types::{Modifier, Node, Value};
use kdl::KdlValue;

pub fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 2);
    result.push('"');

    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            c if c.is_control() => {
                write!(result, "\\u{{{:x}}}", c as u32).unwrap();
            }
            c => result.push(c),
        }
    }

    result.push('"');
    result
}

pub fn is_valid_identifier(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    let mut chars = s.chars();

    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => return false,
    }

    for c in chars {
        if !c.is_ascii_alphanumeric() && c != '_' && c != '-' {
            return false;
        }
    }

    true
}

pub fn render_key(key: &str) -> String {
    if is_valid_identifier(key) {
        key.to_string()
    } else {
        escape_string(key)
    }
}

pub fn render_key_with_repr(key: &str, repr: Option<&str>) -> String {
    if let Some(repr) = repr {
        repr.to_string()
    } else {
        render_key(key)
    }
}

fn render_value_atom(value: &Value) -> String {
    match value {
        Value::Null => "#null".to_string(),
        Value::Bool(true) => "#true".to_string(),
        Value::Bool(false) => "#false".to_string(),
        Value::Int(n) => n.to_string(),
        Value::Float(f) => {
            let s = f.to_string();
            if s.contains('.') || s.contains('e') || s.contains('E') {
                s
            } else {
                format!("{s}.0")
            }
        }
        Value::String(s) => escape_string(s),
        Value::Path(p) => escape_string(&p.to_string_lossy()),
        Value::Array(arr) => arr.iter().map(render_value_atom).collect::<Vec<_>>().join(" "),
    }
}

pub fn render_value(value: &Value) -> String {
    render_value_atom(value)
}

fn render_value_tokens(value: &Value) -> Vec<String> {
    match value {
        Value::Array(values) => values.iter().flat_map(render_value_tokens).collect(),
        _ => vec![render_value_atom(value)],
    }
}

fn value_to_kdl_atom(value: &Value) -> KdlValue {
    match value {
        Value::Null => KdlValue::Null,
        Value::Bool(b) => KdlValue::Bool(*b),
        Value::Int(n) => KdlValue::Integer(*n),
        Value::Float(f) => KdlValue::Float(*f),
        Value::String(s) => KdlValue::String(s.clone()),
        Value::Path(p) => KdlValue::String(p.to_string_lossy().to_string()),
        Value::Array(_) => KdlValue::String(render_value_atom(value)),
    }
}

pub fn value_to_kdl(value: &Value) -> KdlValue {
    match value {
        Value::Array(values) if values.len() == 1 => value_to_kdl_atom(&values[0]),
        Value::Array(_) => value_to_kdl_atom(value),
        _ => value_to_kdl_atom(value),
    }
}

pub fn value_to_kdl_values(value: &Value) -> Vec<KdlValue> {
    match value {
        Value::Array(values) => values.iter().flat_map(value_to_kdl_values).collect(),
        _ => vec![value_to_kdl_atom(value)],
    }
}

pub fn render_value_with_repr(value: &Value, repr: Option<&str>) -> String {
    if let Some(repr) = repr {
        repr.to_string()
    } else {
        render_value(value)
    }
}

fn render_modifier(modifier: Modifier) -> &'static str {
    match modifier {
        Modifier::Inherit => "",
        Modifier::Append => "+",
        Modifier::Remove => "-",
        Modifier::Replace => "!",
        Modifier::Flatten => "~",
    }
}

#[derive(Debug, Default)]
pub struct NodeRenderer {
    name: String,
    modifier: Modifier,
    name_repr: Option<String>,
    positional: Vec<(usize, String)>,
    flags: Vec<String>,
    keyed: Vec<(String, String)>,
    children: Vec<String>,
}

impl NodeRenderer {
    pub fn new(name: impl Into<String>, modifier: Modifier) -> Self {
        Self {
            name: name.into(),
            modifier,
            name_repr: None,
            ..Default::default()
        }
    }

    pub fn with_name_repr(&mut self, repr: Option<String>) -> &mut Self {
        self.name_repr = repr;
        self
    }

    pub fn positional(&mut self, index: usize, value: &Value) -> &mut Self {
        let mut offset = 0usize;
        for rendered in render_value_tokens(value) {
            self.positional.push((index + offset, rendered));
            offset += 1;
        }
        self
    }

    pub fn positional_raw(&mut self, index: usize, rendered: String) -> &mut Self {
        self.positional.push((index, rendered));
        self
    }

    pub fn next_positional_index(&self) -> usize {
        self.positional
            .iter()
            .map(|(idx, _)| *idx)
            .max()
            .map(|idx| idx + 1)
            .unwrap_or(0)
    }

    pub fn flag(&mut self, token: impl Into<String>) -> &mut Self {
        self.flags.push(token.into());
        self
    }

    pub fn keyed(&mut self, key: impl Into<String>, value: &Value) -> &mut Self {
        let key = key.into();
        let rendered_key = render_key(&key);
        for rendered_value in render_value_tokens(value) {
            self.keyed.push((rendered_key.clone(), rendered_value));
        }
        self
    }

    pub fn keyed_raw(
        &mut self,
        key: impl Into<String>,
        rendered_value: impl Into<String>,
    ) -> &mut Self {
        let key = key.into();
        let rendered_key = render_key(&key);
        self.keyed.push((rendered_key, rendered_value.into()));
        self
    }

    pub fn keyed_raw_with_repr(
        &mut self,
        key: impl Into<String>,
        key_repr: Option<&str>,
        rendered_value: impl Into<String>,
    ) -> &mut Self {
        let key = key.into();
        let rendered_key = render_key_with_repr(&key, key_repr);
        self.keyed.push((rendered_key, rendered_value.into()));
        self
    }

    pub fn child(&mut self, rendered: impl Into<String>) -> &mut Self {
        self.children.push(rendered.into());
        self
    }

    pub fn render(&self) -> String {
        let mut result = String::new();

        let modifier = if self.modifier != Modifier::Inherit
            && (self.name_repr.is_some() || !is_valid_identifier(&self.name))
        {
            ""
        } else {
            render_modifier(self.modifier)
        };
        let name_repr = self.name_repr.as_deref();
        let name = format!(
            "{}{}",
            modifier,
            render_key_with_repr(&self.name, name_repr)
        );
        result.push_str(&name);

        let mut positional = self.positional.clone();
        positional.sort_by_key(|(idx, _)| *idx);
        for (_, value) in positional {
            result.push(' ');
            result.push_str(&value);
        }

        for flag in &self.flags {
            result.push(' ');
            result.push_str(&render_key(flag));
        }

        let mut keyed = self.keyed.clone();
        keyed.sort_by(|(a, _), (b, _)| a.cmp(b));
        for (key, value) in keyed {
            result.push(' ');
            result.push_str(&key);
            result.push('=');
            result.push_str(&value);
        }

        if !self.children.is_empty() {
            result.push_str(" {\n");
            for child in &self.children {
                for line in child.lines() {
                    result.push_str("    ");
                    result.push_str(line);
                    result.push('\n');
                }
            }
            result.push('}');
        }

        result
    }
}

impl crate::KdlRender for Node {
    fn render<W: std::fmt::Write>(&self, w: &mut W, name: &str, indent: usize) -> std::fmt::Result {
        write_indent(w, indent)?;

        let use_self_name = name == self.name;
        let (name_for_modifier, repr_for_modifier) = if use_self_name {
            (self.name.as_str(), self.name_repr.as_deref())
        } else {
            (name, None)
        };
        let render_name = if use_self_name {
            render_key_with_repr(&self.name, self.name_repr.as_deref())
        } else {
            render_key(name)
        };
        let modifier = if self.modifier != Modifier::Inherit
            && (repr_for_modifier.is_some() || !is_valid_identifier(name_for_modifier))
        {
            ""
        } else {
            render_modifier(self.modifier)
        };
        write!(w, "{}{}", modifier, render_name)?;

        for arg in self.args() {
            for rendered in render_value_tokens(arg) {
                write!(w, " {}", rendered)?;
            }
        }

        let mut keyed: Vec<_> = self.attrs().iter().collect();
        keyed.sort_by(|(a, _), (b, _)| a.cmp(b));
        for (key, values) in keyed {
            let rendered_key = render_key_with_repr(key, self.attr_repr(key));
            for value in values {
                for rendered in render_value_tokens(value) {
                    write!(w, " {}={}", rendered_key, rendered)?;
                }
            }
        }

        if !self.children().is_empty() {
            writeln!(w, " {{")?;
            for child in self.children() {
                child.render(w, &child.name, indent + 1)?;
                writeln!(w)?;
            }
            write_indent(w, indent)?;
            write!(w, "}}")?;
        }

        Ok(())
    }

    fn render_node(&self, name: &str) -> Node {
        if name == self.name {
            self.clone()
        } else {
            let mut node = self.clone();
            node.name = name.to_string();
            node.name_repr = None;
            node
        }
    }
}

pub fn render_value_node(name: &str, values: &[Value]) -> String {
    let mut result = render_key(name);
    for value in values {
        result.push(' ');
        result.push_str(&render_value(value));
    }
    result
}

pub fn render_value_node_scalar(name: &str, value: &Value) -> String {
    render_value_node(name, std::slice::from_ref(value))
}

pub fn value_node(name: &str, values: &[Value]) -> Node {
    let mut node = Node::named(name);
    for value in values {
        node.add_arg(value.clone());
    }
    node
}

pub fn render_node(node: &Node) -> String {
    let mut renderer = NodeRenderer::new(node.name.clone(), node.modifier);
    renderer.with_name_repr(node.name_repr().map(|repr| repr.to_string()));

    let mut pos = 0usize;
    for (idx, value) in node.args().iter().enumerate() {
        let repr = node.arg_repr(idx);
        if let Some(repr) = repr {
            renderer.positional_raw(pos, repr.to_string());
            pos += 1;
        } else {
            for rendered in render_value_tokens(value) {
                renderer.positional_raw(pos, rendered);
                pos += 1;
            }
        }
    }

    for (key, values) in node.attrs() {
        let key_repr = node.attr_repr(key);
        for value in values {
            for rendered in render_value_tokens(value) {
                renderer.keyed_raw_with_repr(key, key_repr, rendered);
            }
        }
    }

    for child in node.children() {
        renderer.child(render_node(child));
    }

    renderer.render()
}

pub fn render_flatten<T: crate::KdlRender>(value: &T, name: &str) -> Node {
    value.render_node(name)
}

pub fn render_child_node<T: crate::KdlRender>(value: &T, name: &str) -> Node {
    let rendered = crate::to_kdl(value, name);
    crate::parse_node(&rendered).expect("rendered node is invalid KDL")
}

pub fn insert_arg(rendered: &str, arg: &str) -> String {
    if let Some(pos) = rendered.find(' ') {
        let mut s = String::with_capacity(rendered.len() + arg.len() + 1);
        s.push_str(&rendered[..pos]);
        s.push(' ');
        s.push_str(arg);
        s.push_str(&rendered[pos..]);
        s
    } else if let Some(pos) = rendered.find('{') {
        let mut s = String::with_capacity(rendered.len() + arg.len() + 1);
        s.push_str(&rendered[..pos]);
        s.push(' ');
        s.push_str(arg);
        s.push_str(&rendered[pos..]);
        s
    } else {
        format!("{} {}", rendered, arg)
    }
}
pub fn write_indent<W: std::fmt::Write>(w: &mut W, indent: usize) -> std::fmt::Result {
    for _ in 0..indent {
        w.write_str("    ")?;
    }
    Ok(())
}
