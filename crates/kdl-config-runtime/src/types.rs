use std::collections::HashMap;
use std::path::PathBuf;

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
    Int(i128),
    Float(f64),
    String(String),
    Path(PathBuf),
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

impl From<PathBuf> for Value {
    fn from(value: PathBuf) -> Self {
        Value::Path(value)
    }
}

impl From<&std::path::Path> for Value {
    fn from(value: &std::path::Path) -> Self {
        Value::Path(value.to_path_buf())
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Int(value as i128)
    }
}

impl From<i128> for Value {
    fn from(value: i128) -> Self {
        Value::Int(value)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Int(value as i128)
    }
}

impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Value::Int(value as i128)
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Value::Int(value as i128)
    }
}

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        Value::Int(value as i128)
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
            Value::Int(n) if *n >= i64::MIN as i128 && *n <= i64::MAX as i128 => Some(*n as i64),
            _ => None,
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        match self {
            Value::Int(n) if *n >= 0 && *n <= u64::MAX as i128 => Some(*n as u64),
            _ => None,
        }
    }

    pub fn as_u32(&self) -> Option<u32> {
        match self {
            Value::Int(n) if *n >= 0 && *n <= u32::MAX as i128 => Some(*n as u32),
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

    pub fn as_path(&self) -> Option<&PathBuf> {
        match self {
            Value::Path(p) => Some(p),
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
            Value::Path(_) => "path",
            Value::Array(_) => "array",
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Node {
    pub name: String,
    pub modifier: Modifier,
    pub name_repr: Option<String>,
    args: Vec<Value>,
    attrs: HashMap<String, Vec<Value>>,
    attr_reprs: HashMap<String, String>,
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

    pub fn with_name_repr(mut self, repr: impl Into<String>) -> Self {
        self.name_repr = Some(repr.into());
        self
    }

    pub fn with_arg(mut self, arg: Value) -> Self {
        self.args.push(arg);
        self
    }

    pub fn with_attr(mut self, key: impl Into<String>, value: Value) -> Self {
        self.attrs.entry(key.into()).or_default().push(value);
        self
    }

    pub fn with_attr_repr(mut self, key: impl Into<String>, repr: impl Into<String>) -> Self {
        self.attr_reprs.insert(key.into(), repr.into());
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
        self.attrs.entry(key.into()).or_default().push(value);
    }

    pub fn set_attr_repr(&mut self, key: impl Into<String>, repr: impl Into<String>) {
        self.attr_reprs.insert(key.into(), repr.into());
    }

    pub fn set_child(&mut self, child: Node) {
        self.children.push(child);
    }

    pub fn args(&self) -> &[Value] {
        &self.args
    }

    pub fn attrs(&self) -> &HashMap<String, Vec<Value>> {
        &self.attrs
    }

    pub fn name_repr(&self) -> Option<&str> {
        self.name_repr.as_deref()
    }

    pub fn attr_repr(&self, key: &str) -> Option<&str> {
        self.attr_reprs.get(key).map(|v| v.as_str())
    }

    pub fn children(&self) -> &[Node] {
        &self.children
    }

    pub fn attr_values(&self, key: &str) -> Option<&[Value]> {
        self.attrs.get(key).map(|values| values.as_slice())
    }

    pub fn attr(&self, key: &str) -> Option<&Value> {
        self.attr_values(key).and_then(|values| values.first())
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
        self.without_arg(0)
    }

    pub fn without_arg(&self, index: usize) -> Self {
        if index >= self.args.len() {
            return self.clone();
        }
        let mut copy = self.clone();
        copy.args.remove(index);
        copy
    }

    pub fn without_attr(&self, key: &str) -> Self {
        if !self.attrs.contains_key(key) {
            return self.clone();
        }
        let mut copy = self.clone();
        copy.attrs.remove(key);
        copy.attr_reprs.remove(key);
        copy
    }
}
