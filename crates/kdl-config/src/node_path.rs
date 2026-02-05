use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodePath {
    segments: Vec<NodePathSegment>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodePathSegment {
    pub name: String,
    pub index: usize,
}

impl NodePath {
    pub fn root(name: impl Into<String>, index: usize) -> Self {
        Self {
            segments: vec![NodePathSegment {
                name: name.into(),
                index,
            }],
        }
    }

    pub fn child(&self, name: impl Into<String>, index: usize) -> Self {
        let mut segments = self.segments.clone();
        segments.push(NodePathSegment {
            name: name.into(),
            index,
        });
        Self { segments }
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }
}

impl fmt::Display for NodePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for segment in &self.segments {
            write!(f, "/{}[{}]", segment.name, segment.index)?;
        }
        Ok(())
    }
}
