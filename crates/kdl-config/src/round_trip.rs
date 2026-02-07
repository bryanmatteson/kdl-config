use crate::{
    DecodeContext, KdlDecode, KdlNodeExt, KdlUpdate, ParseConfig, Source, UpdateContext,
};
use crate::{KdlConfigError, KdlDocument};

#[derive(Debug, Clone)]
pub struct RoundTripAst<T> {
    value: T,
    doc: KdlDocument,
    root_index: usize,
    original: String,
    dirty: bool,
    config: ParseConfig,
}

impl<T> RoundTripAst<T> {
    pub fn value(&self) -> &T {
        &self.value
    }

    pub fn value_mut(&mut self) -> &mut T {
        self.dirty = true;
        &mut self.value
    }

    pub fn into_inner(self) -> T {
        self.value
    }

    pub fn original(&self) -> &str {
        &self.original
    }
}

impl<T: KdlUpdate> RoundTripAst<T> {
    pub fn to_kdl(&mut self) -> Result<String, KdlConfigError> {
        if !self.dirty {
            return Ok(self.original.clone());
        }
        let ctx = UpdateContext::new(&self.config);
        if let Some(node) = self.doc.nodes_mut().get_mut(self.root_index) {
            self.value.update(node, &ctx)?;
        }
        Ok(self.doc.to_string())
    }

    pub fn to_kdl_fragment_aware(&mut self) -> Result<String, KdlConfigError> {
        if !self.dirty {
            return Ok(self.original.clone());
        }

        let source = Source::new(self.original.clone());
        let mut expanded = self.doc.clone();
        let (_expansion, map) =
            crate::fragments::expand_fragments_with_map(&mut expanded, Some(&source))?;
        let baseline = expanded.clone();

        let ctx = UpdateContext::new(&self.config);
        let nodes = expanded.nodes_mut();
        if nodes.is_empty() {
            return Err(KdlConfigError::custom(
                "KDL Document",
                "expected a single top-level node, found none",
            ));
        }
        if nodes.len() != 1 {
            return Err(KdlConfigError::custom(
                "KDL Document",
                format!("expected a single top-level node, found {}", nodes.len()),
            ));
        }
        self.value.update(&mut nodes[0], &ctx)?;

        let mut result = expanded.clone();
        crate::fragments::apply_fragment_aware_updates(&mut result, &baseline, &expanded, &map)?;
        self.doc = result.clone();
        Ok(result.to_string())
    }
}

pub fn parse_str_roundtrip<T: KdlDecode + KdlUpdate>(
    contents: &str,
) -> Result<RoundTripAst<T>, KdlConfigError> {
    parse_str_roundtrip_with_config(contents, &ParseConfig::default())
}

pub fn parse_str_roundtrip_with_config<T: KdlDecode + KdlUpdate>(
    contents: &str,
    config: &ParseConfig,
) -> Result<RoundTripAst<T>, KdlConfigError> {
    let doc: KdlDocument = contents
        .parse()
        .map_err(|e: kdl::KdlError| KdlConfigError::custom("KDL Document", e.to_string()))?;
    let source = Source::new(contents.to_string());
    let mut expanded = doc.clone();
    crate::fragments::expand_fragments(&mut expanded, Some(&source))?;
    let ctx = DecodeContext::new(config, Some(&source));
    let nodes = expanded.nodes();
    if nodes.is_empty() {
        return Err(KdlConfigError::custom(
            "KDL Document",
            "expected a single top-level node, found none",
        ));
    }
    if nodes.len() != 1 {
        return Err(KdlConfigError::custom(
            "KDL Document",
            format!("expected a single top-level node, found {}", nodes.len()),
        ));
    }

    let root_index = find_root_index(&doc)?;
    let node = &nodes[0];
    let value = T::decode(node, &ctx.with_root_node(node, 0))?;
    Ok(RoundTripAst {
        value,
        doc,
        root_index,
        original: contents.to_string(),
        dirty: false,
        config: *config,
    })
}

fn find_root_index(doc: &KdlDocument) -> Result<usize, KdlConfigError> {
    let mut root_index = None;
    for (idx, node) in doc.nodes().iter().enumerate() {
        if node.base_name() == "fragment" {
            continue;
        }
        if root_index.is_some() {
            return Err(KdlConfigError::custom(
                "KDL Document",
                "expected a single top-level node, found multiple",
            ));
        }
        root_index = Some(idx);
    }
    root_index.ok_or_else(|| {
        KdlConfigError::custom("KDL Document", "expected a single top-level node, found none")
    })
}
