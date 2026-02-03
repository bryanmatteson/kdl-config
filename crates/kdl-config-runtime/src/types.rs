use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Modifier {
    #[default]
    Inherit,
    Replace,
    Append,
    Remove,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Array(Vec<Value>),
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::String(value.to_string())
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Int(value)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Int(value as i64)
    }
}

impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Value::Int(value as i64)
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Value::Int(value as i64)
    }
}

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        Value::Int(value as i64)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(value)
    }
}

impl From<f32> for Value {
    fn from(value: f32) -> Self {
        Value::Float(value as f64)
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(values: Vec<T>) -> Self {
        Value::Array(values.into_iter().map(Into::into).collect())
    }
}

impl Value {
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Value::Int(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        match self {
            Value::Int(n) if *n >= 0 => Some(*n as u64),
            _ => None,
        }
    }

    pub fn as_u32(&self) -> Option<u32> {
        match self {
            Value::Int(n) if *n >= 0 && *n <= u32::MAX as i64 => Some(*n as u32),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Value::Float(f) => Some(*f),
            Value::Int(n) => Some(*n as f64),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&[Value]> {
        match self {
            Value::Array(arr) => Some(arr),
            _ => None,
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Null => "null",
            Value::Bool(_) => "bool",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Array(_) => "array",
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Node {
    pub name: String,
    pub modifier: Modifier,
    args: Vec<Value>,
    attrs: HashMap<String, Value>,
    children: Vec<Node>,
}

impl Node {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn named(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }

    pub fn with_modifier(mut self, modifier: Modifier) -> Self {
        self.modifier = modifier;
        self
    }

    pub fn with_arg(mut self, arg: Value) -> Self {
        self.args.push(arg);
        self
    }

    pub fn with_attr(mut self, key: impl Into<String>, value: Value) -> Self {
        self.attrs.insert(key.into(), value);
        self
    }

    pub fn with_child(mut self, child: Node) -> Self {
        self.children.push(child);
        self
    }

    pub fn add_arg(&mut self, value: Value) {
        self.args.push(value);
    }

    pub fn add_child(&mut self, child: Node) {
        self.children.push(child);
    }

    pub fn set_attr(&mut self, key: impl Into<String>, value: Value) {
        self.attrs.insert(key.into(), value);
    }

    pub fn set_child(&mut self, child: Node) {
        self.children.push(child);
    }

    pub fn args(&self) -> &[Value] {
        &self.args
    }

    pub fn attrs(&self) -> &HashMap<String, Value> {
        &self.attrs
    }

    pub fn children(&self) -> &[Node] {
        &self.children
    }

    pub fn attr(&self, key: &str) -> Option<&Value> {
        self.attrs.get(key)
    }

    pub fn arg(&self, index: usize) -> Option<&Value> {
        self.args.get(index)
    }

    pub fn child(&self, name: &str) -> Option<&Node> {
        self.children.iter().find(|c| c.name == name)
    }

    pub fn children_named<'a>(&'a self, name: &'a str) -> impl Iterator<Item = &'a Node> + 'a {
        self.children.iter().filter(move |c| c.name == name)
    }

    pub fn without_first_arg(&self) -> Self {
        if self.args.is_empty() {
            return self.clone();
        }
        let mut copy = self.clone();
        copy.args.remove(0);
        copy
    }
}
