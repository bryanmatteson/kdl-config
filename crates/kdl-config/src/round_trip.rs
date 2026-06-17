use crate::{
    DecodeContext, KdlConfigError, KdlDecode, KdlDocument, KdlNodeExt, KdlUpdate, ParseConfig,
    RootMode, Source, UpdateContext,
};

/// How the top-level document was wrapped (if at all) when the AST was
/// parsed. Round-trip render uses this to decide whether to emit the
/// inner-body form (flat) or the full document (wrapped/strict).
#[derive(Debug, Clone)]
enum RootWrap {
    /// Single explicit top-level node (Strict or successful WrapExpectedNode
    /// non-fallback path). Render emits the full document.
    Single,
    /// Document was wrapped in `name { ... }` at parse time. Render emits
    /// the body of the wrapper (flat top-level nodes, like the original
    /// source). The `name` is the synthetic wrapper name; it is kept for
    /// diagnostics and round-trip parity even though render strips it.
    Wrapped {
        #[allow(dead_code)]
        name: String,
    },
}

#[derive(Debug, Clone)]
pub struct RoundTripAst<T> {
    value: T,
    doc: KdlDocument,
    root_index: usize,
    original: String,
    dirty: bool,
    config: ParseConfig,
    wrap: RootWrap,
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
        match &self.wrap {
            RootWrap::Single => Ok(self.doc.to_string()),
            RootWrap::Wrapped { .. } => Ok(render_wrapped_body(&self.doc, self.root_index)),
        }
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
    // `RootMode::Document` always wraps the document body before parsing.
    // Round-trip uses `RootWrap::Wrapped` so render emits the body back as
    // a flat document (matching the on-disk shape).
    let (effective_contents, wrap) = match &config.root_mode {
        RootMode::Document { name } => {
            let wrapped = format!("{name} {{\n{contents}\n}}\n");
            let mut strict = config.clone();
            strict.root_mode = RootMode::Strict;
            return parse_str_roundtrip_with_config_inner::<T>(
                &wrapped,
                &strict,
                contents.to_string(),
                RootWrap::Wrapped { name: name.clone() },
            );
        }
        _ => (contents.to_string(), RootWrap::Single),
    };
    parse_str_roundtrip_with_config_inner::<T>(
        &effective_contents,
        config,
        contents.to_string(),
        wrap,
    )
}

fn parse_str_roundtrip_with_config_inner<T: KdlDecode + KdlUpdate>(
    parser_input: &str,
    config: &ParseConfig,
    original: String,
    wrap: RootWrap,
) -> Result<RoundTripAst<T>, KdlConfigError> {
    let doc: KdlDocument = parser_input
        .parse()
        .map_err(|e: kdl::KdlError| KdlConfigError::from_kdl_parse_error("KDL Document", e))?;
    let source = Source::new(parser_input.to_string());
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
        original,
        dirty: false,
        config: config.clone(),
        wrap,
    })
}

/// Render only the children of the wrapper node at `root_index`, dedented one
/// level. Round-trip counterpart to [`RootMode::Document`]: the parser
/// internally wrapped a flat document, so render strips that wrapper back out.
fn render_wrapped_body(doc: &KdlDocument, root_index: usize) -> String {
    let Some(node) = doc.nodes().get(root_index) else {
        return doc.to_string();
    };
    let Some(children) = node.children() else {
        return String::new();
    };
    let rendered = children.to_string();
    // `KdlDocument::to_string` for a child block emits each top-level child
    // node on its own line. Strip one indent level so the output matches the
    // original flat-document indentation.
    let mut out = String::with_capacity(rendered.len());
    for line in rendered.lines() {
        let dedented = line.strip_prefix("    ").unwrap_or(line);
        if !dedented.is_empty() || !out.is_empty() {
            out.push_str(dedented);
            out.push('\n');
        }
    }
    let trimmed = out.trim().to_string();
    if trimmed.is_empty() {
        trimmed
    } else {
        trimmed + "\n"
    }
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
        KdlConfigError::custom(
            "KDL Document",
            "expected a single top-level node, found none",
        )
    })
}
