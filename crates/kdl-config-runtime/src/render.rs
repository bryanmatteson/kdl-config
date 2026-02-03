use std::fmt::Write;

use crate::types::{Modifier, Value};

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

pub fn render_value(value: &Value) -> String {
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
        Value::Array(arr) => arr.iter().map(render_value).collect::<Vec<_>>().join(" "),
    }
}

fn render_modifier(modifier: Modifier) -> &'static str {
    match modifier {
        Modifier::Inherit => "",
        Modifier::Append => "+",
        Modifier::Remove => "-",
        Modifier::Replace => "!",
    }
}

#[derive(Debug, Default)]
pub struct NodeRenderer {
    name: String,
    modifier: Modifier,
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
            ..Default::default()
        }
    }

    pub fn positional(&mut self, index: usize, value: &Value) -> &mut Self {
        self.positional.push((index, render_value(value)));
        self
    }

    pub fn positional_raw(&mut self, index: usize, rendered: String) -> &mut Self {
        self.positional.push((index, rendered));
        self
    }

    pub fn flag(&mut self, token: impl Into<String>) -> &mut Self {
        self.flags.push(token.into());
        self
    }

    pub fn keyed(&mut self, key: impl Into<String>, value: &Value) -> &mut Self {
        let key = key.into();
        let rendered_key = render_key(&key);
        let rendered_value = render_value(value);
        self.keyed.push((rendered_key, rendered_value));
        self
    }

    pub fn keyed_raw(&mut self, key: impl Into<String>, rendered_value: impl Into<String>) -> &mut Self {
        let key = key.into();
        let rendered_key = render_key(&key);
        self.keyed.push((rendered_key, rendered_value.into()));
        self
    }

    pub fn child(&mut self, rendered: impl Into<String>) -> &mut Self {
        self.children.push(rendered.into());
        self
    }

    pub fn render(&self) -> String {
        let mut result = String::new();

        let name = format!("{}{}", render_modifier(self.modifier), render_key(&self.name));
        result.push_str(&name);

        let mut positional = self.positional.clone();
        positional.sort_by_key(|(idx, _)| *idx);
        for (_, value) in positional {
            result.push(' ');
            result.push_str(&value);
        }

        for flag in &self.flags {
            result.push(' ');
            result.push_str(&escape_string(flag));
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
