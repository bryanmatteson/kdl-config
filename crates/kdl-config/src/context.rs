use crate::config::ParseConfig;
use crate::node_path::NodePath;
use crate::types::NodeLocation;

#[derive(Debug, Clone)]
pub struct LineIndex {
    line_starts: Vec<usize>,
    len: usize,
}

impl LineIndex {
    pub fn new(contents: &str) -> Self {
        let mut line_starts = vec![0];
        for (idx, byte) in contents.bytes().enumerate() {
            if byte == b'\n' {
                line_starts.push(idx + 1);
            }
        }
        Self {
            line_starts,
            len: contents.len(),
        }
    }

    pub fn location(&self, offset: usize) -> NodeLocation {
        let offset = offset.min(self.len);
        let line_idx = match self.line_starts.binary_search(&offset) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };
        let line_start = self.line_starts.get(line_idx).copied().unwrap_or(0);
        NodeLocation {
            line: line_idx + 1,
            column: offset.saturating_sub(line_start) + 1,
        }
    }
}

#[derive(Debug)]
pub struct Source {
    pub text: String,
    index: LineIndex,
}

impl Source {
    pub fn new(text: String) -> Self {
        let index = LineIndex::new(&text);
        Self { text, index }
    }

    pub fn location(&self, offset: usize) -> NodeLocation {
        self.index.location(offset)
    }
}

#[derive(Debug, Clone)]
pub struct DecodeContext<'a> {
    pub config: &'a ParseConfig,
    pub source: Option<&'a Source>,
    pub path: Option<NodePath>,
    pub allow_any_name: bool,
}

impl<'a> DecodeContext<'a> {
    pub fn new(config: &'a ParseConfig, source: Option<&'a Source>) -> Self {
        Self {
            config,
            source,
            path: None,
            allow_any_name: false,
        }
    }

    pub fn with_root(&self, name: impl Into<String>, index: usize) -> Self {
        Self {
            config: self.config,
            source: self.source,
            path: Some(NodePath::root(name, index)),
            allow_any_name: self.allow_any_name,
        }
    }

    pub fn with_child(&self, name: impl Into<String>, index: usize) -> Self {
        let path = match &self.path {
            Some(path) => Some(path.child(name, index)),
            None => Some(NodePath::root(name, index)),
        };
        Self {
            config: self.config,
            source: self.source,
            path,
            allow_any_name: self.allow_any_name,
        }
    }

    pub fn with_any_name(&self) -> Self {
        Self {
            config: self.config,
            source: self.source,
            path: self.path.clone(),
            allow_any_name: true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UpdateContext<'a> {
    pub config: &'a ParseConfig,
}

impl<'a> UpdateContext<'a> {
    pub fn new(config: &'a ParseConfig) -> Self {
        Self { config }
    }
}
