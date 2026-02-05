use crate::config::ConflictPolicy;

#[derive(Debug, Clone)]
pub enum SelectorAst {
    Arg(u32),
    Attr(String),
    Name,
    Func(String),
    Any(Vec<SelectorAst>),
}

#[derive(Debug, Clone)]
pub enum InjectOpt {
    Implicit,
    Field(String),
}

#[derive(Debug, Clone, Default)]
pub struct SelectOpts {
    pub consume: Option<bool>,
    pub inject: Option<InjectOpt>,
}

#[derive(Debug, Clone)]
pub struct SelectSpec {
    pub selector: SelectorAst,
    pub opts: SelectOpts,
}

impl SelectSpec {
    pub fn new(selector: SelectorAst) -> Self {
        Self {
            selector,
            opts: SelectOpts::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CollectMode {
    Registry { container: String },
    ChildrenMapAll,
    ChildrenMapNode { node: String },
}

#[derive(Debug, Clone)]
pub struct CollectionSpec {
    pub mode: CollectMode,
    pub key: SelectorAst,
    pub consume: bool,
    pub conflict: ConflictPolicy,
}
