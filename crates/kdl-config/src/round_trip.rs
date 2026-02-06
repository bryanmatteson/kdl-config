use crate::{DecodeContext, KdlDecode, KdlUpdate, ParseConfig, Source, UpdateContext};
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
    let mut doc: KdlDocument = contents
        .parse()
        .map_err(|e: kdl::KdlError| KdlConfigError::custom("KDL Document", e.to_string()))?;
    let source = Source::new(contents.to_string());
    crate::templates::expand_templates(&mut doc, Some(&source))?;
    let ctx = DecodeContext::new(config, Some(&source));
    let nodes = doc.nodes();
    match nodes.len() {
        0 => Err(KdlConfigError::custom(
            "KDL Document",
            "expected a single top-level node, found none",
        )),
        1 => {
            let node = &nodes[0];
            let value = T::decode(node, &ctx.with_root(node.name().value(), 0))?;
            Ok(RoundTripAst {
                value,
                doc,
                root_index: 0,
                original: contents.to_string(),
                dirty: false,
                config: *config,
            })
        }
        count => Err(KdlConfigError::custom(
            "KDL Document",
            format!("expected a single top-level node, found {count}"),
        )),
    }
}
