use std::collections::HashSet;

use kdl_config::layer::{LayerMerge, merge_layers_with};
use kdl_config::{MergeModifierPolicy, Node};
use proptest::prelude::*;

fn layer_with_names(names: &[String]) -> Node {
    let mut root = Node::new();
    for name in names {
        root.add_child(Node::named(name.clone()));
    }
    root
}

proptest! {
    #[test]
    fn append_merge_preserves_child_count(
        layers in prop::collection::vec(
            prop::collection::vec("[a-z]{1,6}", 0..6),
            0..6
        )
    ) {
        let nodes: Vec<Node> = layers.iter().map(|names| layer_with_names(names)).collect();
        let merged = merge_layers_with(&nodes, LayerMerge::Append);
        let expected: usize = layers.iter().map(|names| names.len()).sum();
        prop_assert_eq!(merged.children().len(), expected);
    }

    #[test]
    fn by_name_merge_dedupes_children(
        layers in prop::collection::vec(
            prop::collection::hash_set("[a-z]{1,6}", 0..6)
                .prop_map(|set| set.into_iter().collect::<Vec<_>>()),
            0..6
        )
    ) {
        let nodes: Vec<Node> = layers.iter().map(|names| layer_with_names(names)).collect();
        let merged = merge_layers_with(&nodes, LayerMerge::ByName);
        let expected: HashSet<String> = layers.into_iter().flatten().collect();
        let merged_names: HashSet<String> = merged.children().iter().map(|c| c.name.clone()).collect();
        prop_assert_eq!(merged_names, expected);
    }

    #[test]
    fn merge_with_different_names_appends_child(
        parent_name in "[a-z]{1,6}",
        child_name in "[a-z]{1,6}"
    ) {
        prop_assume!(parent_name != child_name);
        let mut parent = Node::named(parent_name.clone());
        let child = Node::named(child_name.clone());
        parent.merge_with(child, MergeModifierPolicy::Consume);
        prop_assert!(parent.children().iter().any(|c| c.name == child_name));
    }
}
