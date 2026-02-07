//! Layered configuration utilities.
//!
//! Default semantics:
//! - Later layers override earlier layers.
//! - Nodes are merged by name.
//! - Modifiers act as merge operators and are consumed:
//!   - `+node` appends a new sibling.
//!   - `!node` replaces all existing nodes with that name.
//!   - `-node` removes all existing nodes with that name.
//! - In preserve-modifier modes, modifiers are retained on nodes that are kept,
//!   but merged nodes are normalized to `Modifier::Inherit` to avoid mixed provenance.

use crate::{KdlConfigError, KdlDecode, Node, ParseConfig, parse_str_with_config, render_node};

/// Controls how layers are merged.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LayerMerge {
    /// Merge nodes with the same name and append otherwise.
    ByName,
    /// Append all nodes without merging by name.
    Append,
    /// Merge nodes with the same name and preserve modifiers in the output.
    ByNamePreserveModifiers,
    /// Append all nodes without merging by name and preserve modifiers in the output.
    AppendPreserveModifiers,
}

impl Default for LayerMerge {
    fn default() -> Self {
        LayerMerge::ByName
    }
}

/// Merge multiple parsed KDL documents into a single root node.
pub fn merge_layers(layers: &[Node]) -> Node {
    merge_layers_with(layers, LayerMerge::ByName)
}

/// Merge multiple parsed KDL documents into a single root node using the provided strategy.
pub fn merge_layers_with(layers: &[Node], mode: LayerMerge) -> Node {
    let mut root = Node::new();
    let preserve = matches!(
        mode,
        LayerMerge::ByNamePreserveModifiers | LayerMerge::AppendPreserveModifiers
    );
    let by_name = matches!(
        mode,
        LayerMerge::ByName | LayerMerge::ByNamePreserveModifiers
    );
    let policy = if preserve {
        crate::MergeModifierPolicy::Preserve
    } else {
        crate::MergeModifierPolicy::Consume
    };
    for layer in layers {
        for child in layer.children() {
            apply_layer_child(&mut root, child.clone(), by_name, policy);
        }
    }
    root
}

fn apply_layer_child(
    root: &mut Node,
    mut child: Node,
    by_name: bool,
    policy: crate::MergeModifierPolicy,
) {
    match child.modifier {
        crate::Modifier::Remove => {
            root.remove_children_named(&child.name);
        }
        crate::Modifier::Replace => {
            root.remove_children_named(&child.name);
            if matches!(policy, crate::MergeModifierPolicy::Consume) {
                child.modifier = crate::Modifier::Inherit;
            }
            root.add_child(child);
        }
        crate::Modifier::Append => {
            if matches!(policy, crate::MergeModifierPolicy::Consume) {
                child.modifier = crate::Modifier::Inherit;
            }
            root.add_child(child);
        }
        crate::Modifier::Flatten => {
            root.merge_child_by_name_with(child, policy);
        }
        crate::Modifier::Inherit => {
            if by_name {
                root.merge_child_by_name_with(child, policy);
            } else {
                if matches!(policy, crate::MergeModifierPolicy::Consume) {
                    child.modifier = crate::Modifier::Inherit;
                }
                root.add_child(child);
            }
        }
    }
}

/// Parse a layered configuration into a typed struct, using default parse config.
pub fn parse_layered<T: KdlDecode>(layers: &[Node]) -> Result<T, KdlConfigError> {
    parse_layered_with_config(layers, &ParseConfig::default())
}

/// Parse a layered configuration into a typed struct with custom parse config.
pub fn parse_layered_with_config<T: KdlDecode>(
    layers: &[Node],
    config: &ParseConfig,
) -> Result<T, KdlConfigError> {
    let merged = merge_layers(layers);
    let children = merged.children();
    match children.len() {
        0 => Err(KdlConfigError::custom(
            "KDL Document",
            "expected a single top-level node, found none",
        )),
        1 => {
            let rendered = render_node(&children[0]);
            parse_str_with_config(&rendered, config)
        }
        count => Err(KdlConfigError::custom(
            "KDL Document",
            format!("expected a single top-level node, found {count}"),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::{LayerMerge, merge_layers_with};
    use crate::{Node, Value};

    #[test]
    fn merge_by_name_merges_children() {
        let mut a = Node::new();
        a.add_child(Node::named("config").with_attr("name", Value::String("one".into())));

        let mut b = Node::new();
        b.add_child(Node::named("config").with_attr("name", Value::String("two".into())));

        let merged = merge_layers_with(&[a, b], LayerMerge::ByName);
        assert_eq!(merged.children().len(), 1);
        let config = merged.child("config").unwrap();
        let values = config.attr_values("name").unwrap();
        assert_eq!(values.len(), 2);
    }

    #[test]
    fn merge_append_keeps_all_children() {
        let mut a = Node::new();
        a.add_child(Node::named("config"));

        let mut b = Node::new();
        b.add_child(Node::named("config"));

        let merged = merge_layers_with(&[a, b], LayerMerge::Append);
        assert_eq!(merged.children().len(), 2);
    }

    #[test]
    fn modifiers_apply_in_layers() {
        let mut base = Node::new();
        base.add_child(Node::named("config").with_attr("name", Value::String("base".into())));

        let mut overlay = Node::new();
        overlay.add_child(Node::named("config").with_modifier(crate::Modifier::Remove));

        let merged = merge_layers_with(&[base, overlay], LayerMerge::ByName);
        assert!(merged.child("config").is_none());

        let mut base = Node::new();
        base.add_child(Node::named("config").with_attr("name", Value::String("base".into())));

        let mut overlay = Node::new();
        overlay.add_child(
            Node::named("config")
                .with_modifier(crate::Modifier::Replace)
                .with_attr("name", Value::String("overlay".into())),
        );

        let merged = merge_layers_with(&[base, overlay], LayerMerge::ByName);
        let config = merged.child("config").unwrap();
        let values = config.attr_values("name").unwrap();
        assert_eq!(values.len(), 1);
        assert_eq!(values[0].as_str(), Some("overlay"));
    }

    #[test]
    fn preserve_modifiers_keeps_marker() {
        let mut base = Node::new();
        base.add_child(Node::named("config"));

        let mut overlay = Node::new();
        overlay.add_child(Node::named("config").with_modifier(crate::Modifier::Append));

        let merged = merge_layers_with(&[base, overlay], LayerMerge::ByNamePreserveModifiers);
        let appended = merged.children().iter().last().unwrap();
        assert_eq!(appended.modifier, crate::Modifier::Append);

        let mut overlay = Node::new();
        overlay.add_child(Node::named("config").with_modifier(crate::Modifier::Replace));

        let merged = merge_layers_with(&[merged, overlay], LayerMerge::ByNamePreserveModifiers);
        let replaced = merged.child("config").unwrap();
        assert_eq!(replaced.modifier, crate::Modifier::Replace);
    }
}
