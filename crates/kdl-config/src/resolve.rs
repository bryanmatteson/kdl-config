use crate::{KdlConfigError, KdlDecode, Node, ParseConfig, parse_str_with_config, render_node};

#[derive(Debug, Clone, PartialEq, Eq)]
struct PathSegment {
    name: String,
    index: Option<usize>,
}

/// Locate a node by path and decode it as `T` with default parse settings.
///
/// Supported path forms:
/// - `/config/search`
/// - `/config[0]/search[0]`
pub fn decode_at<T: KdlDecode>(root: &Node, path: &str) -> Result<T, KdlConfigError> {
    decode_at_with_config(root, path, &ParseConfig::default())
}

/// Locate a node by path and decode it as `T` with custom parse settings.
pub fn decode_at_with_config<T: KdlDecode>(
    root: &Node,
    path: &str,
    config: &ParseConfig,
) -> Result<T, KdlConfigError> {
    let target = node_at(root, path)?;
    let rendered = render_node(target);
    parse_str_with_config(&rendered, config)
}

/// Locate a node by path and return a reference.
pub fn node_at<'a>(root: &'a Node, path: &str) -> Result<&'a Node, KdlConfigError> {
    let mut segments = parse_path(path)?;

    let mut current = root;
    if !current.name.is_empty() {
        if let Some(first) = segments.first() {
            if first.name == current.name {
                if let Some(index) = first.index
                    && index != 0
                {
                    return Err(KdlConfigError::custom(
                        "KDL Path",
                        format!(
                            "root segment '{}' requires index 0, found {index}",
                            first.name
                        ),
                    ));
                }
                segments.remove(0);
            }
        }
    }

    for segment in &segments {
        current = select_child(current, segment)?;
    }

    Ok(current)
}

fn parse_path(path: &str) -> Result<Vec<PathSegment>, KdlConfigError> {
    let trimmed = path.trim();
    if trimmed.is_empty() {
        return Err(KdlConfigError::custom("KDL Path", "path cannot be empty"));
    }

    let body = trimmed.strip_prefix('/').unwrap_or(trimmed);
    if body.is_empty() {
        return Err(KdlConfigError::custom(
            "KDL Path",
            "path must include at least one segment",
        ));
    }

    body.split('/').map(parse_segment).collect()
}

fn parse_segment(segment: &str) -> Result<PathSegment, KdlConfigError> {
    if segment.is_empty() {
        return Err(KdlConfigError::custom(
            "KDL Path",
            "path contains an empty segment",
        ));
    }

    if let Some(open) = segment.find('[') {
        let close = segment
            .rfind(']')
            .ok_or_else(|| KdlConfigError::custom("KDL Path", "missing closing ']' in segment"))?;
        if close <= open {
            return Err(KdlConfigError::custom(
                "KDL Path",
                "invalid index syntax in segment",
            ));
        }
        if close != segment.len() - 1 {
            return Err(KdlConfigError::custom(
                "KDL Path",
                "unexpected trailing characters after index",
            ));
        }

        let name = &segment[..open];
        let index_text = &segment[open + 1..close];
        if name.is_empty() {
            return Err(KdlConfigError::custom(
                "KDL Path",
                "segment name cannot be empty",
            ));
        }

        let index = index_text.parse::<usize>().map_err(|_| {
            KdlConfigError::custom(
                "KDL Path",
                format!("invalid index '{index_text}' in segment '{segment}'"),
            )
        })?;

        Ok(PathSegment {
            name: name.to_string(),
            index: Some(index),
        })
    } else {
        Ok(PathSegment {
            name: segment.to_string(),
            index: None,
        })
    }
}

fn select_child<'a>(current: &'a Node, segment: &PathSegment) -> Result<&'a Node, KdlConfigError> {
    let matches: Vec<&Node> = current
        .children()
        .iter()
        .filter(|child| child.name == segment.name)
        .collect();

    if let Some(index) = segment.index {
        if let Some(node) = matches.get(index) {
            return Ok(node);
        }
        return Err(KdlConfigError::custom(
            "KDL Path",
            format!(
                "node '{}[{}]' not found under {}",
                segment.name,
                index,
                display_node(current)
            ),
        ));
    } else {
        let first = matches.first().copied().ok_or_else(|| {
            KdlConfigError::custom(
                "KDL Path",
                format!(
                    "node '{}' not found under {}",
                    segment.name,
                    display_node(current)
                ),
            )
        })?;

        if matches.len() > 1 {
            return Err(KdlConfigError::custom(
                "KDL Path",
                format!(
                    "node '{}' is ambiguous under {}; use an explicit index like '{}[0]'",
                    segment.name,
                    display_node(current),
                    segment.name
                ),
            ));
        }

        Ok(first)
    }
}

fn display_node(node: &Node) -> String {
    if node.name.is_empty() {
        "<root>".to_string()
    } else {
        format!("'{}'", node.name)
    }
}

#[cfg(test)]
mod tests {
    use super::{decode_at, node_at};
    use crate::Kdl;

    #[derive(Debug, Clone, PartialEq, Eq, Kdl)]
    #[kdl(node = "search", rename_all = "kebab-case")]
    struct SearchConfig {
        #[kdl(attr)]
        mode: String,
    }

    fn root() -> crate::Node {
        crate::parse_config(
            r#"
config {
    search mode="hybrid"
}
"#,
        )
        .expect("valid test config")
    }

    #[test]
    fn decode_subtree_by_path() {
        let root = root();
        let search: SearchConfig = decode_at(&root, "/config/search").expect("decode search");
        assert_eq!(search.mode, "hybrid");
    }

    #[test]
    fn explicit_index_path_resolves() {
        let root = root();
        let node = node_at(&root, "/config[0]/search[0]").expect("node exists");
        assert_eq!(node.name, "search");
    }

    #[test]
    fn ambiguous_path_requires_index() {
        let root = crate::parse_config(
            r#"
config {
    source "a"
    source "b"
}
"#,
        )
        .expect("valid test config");

        let err = node_at(&root, "/config/source").expect_err("path should be ambiguous");
        assert!(err.to_string().contains("ambiguous"));
    }
}
