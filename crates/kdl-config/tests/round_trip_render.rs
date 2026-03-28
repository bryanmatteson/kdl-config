use std::collections::BTreeMap;

use kdl_config::{parse_str, parse_str_roundtrip};
use kdl_config_derive::{Kdl, KdlNode};

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct Config {
    #[kdl(attr)]
    name: String,
    #[kdl(attr)]
    count: i64,
}

#[test]
fn roundtrip_preserves_original_when_clean() {
    let src = r#"config name="demo" count=42"#;
    let mut parsed = parse_str_roundtrip::<Config>(src).unwrap();
    let rendered = parsed.to_kdl().unwrap();
    assert_eq!(rendered.trim(), src);
}

#[test]
fn roundtrip_rerenders_when_dirty() {
    let src = r#"config name="demo" count=42"#;
    let mut parsed = parse_str_roundtrip::<Config>(src).unwrap();
    parsed.value_mut().name = "changed".to_string();
    let rendered = parsed.to_kdl().unwrap();
    assert!(rendered.contains("changed"));
    assert_ne!(rendered.trim(), src);
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "item")]
struct RegistryItem {
    #[kdl(attr)]
    size: i64,
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct RegistryBTreeRoundTrip {
    #[kdl(registry, container = "item")]
    items: BTreeMap<String, RegistryItem>,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "category")]
struct CategoryEntry {
    #[kdl(attr)]
    size: i64,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct ChildrenMapBTreeRoundTrip {
    #[kdl(children_map, map_node = "category")]
    categories: BTreeMap<String, CategoryEntry>,
}

#[test]
fn roundtrip_updates_btreemap_registry_entries() {
    let src = "config {\n  item beta size=2\n  item alpha size=1\n}\n";
    let mut parsed = parse_str_roundtrip::<RegistryBTreeRoundTrip>(src).unwrap();
    parsed.value_mut().items.get_mut("beta").unwrap().size = 9;
    parsed
        .value_mut()
        .items
        .insert("gamma".to_string(), RegistryItem { size: 3 });

    let rendered = parsed.to_kdl().unwrap();
    let reparsed = parse_str::<RegistryBTreeRoundTrip>(&rendered).unwrap();
    assert_eq!(reparsed.items.get("beta").unwrap().size, 9);
    assert_eq!(reparsed.items.get("gamma").unwrap().size, 3);
}

#[test]
fn roundtrip_updates_btreemap_children_map_entries() {
    let src = "config {\n  category \"beta\" size=2\n  category \"alpha\" size=1\n}\n";
    let mut parsed = parse_str_roundtrip::<ChildrenMapBTreeRoundTrip>(src).unwrap();
    parsed.value_mut().categories.get_mut("beta").unwrap().size = 9;
    parsed
        .value_mut()
        .categories
        .insert("gamma".to_string(), CategoryEntry { size: 3 });

    let rendered = parsed.to_kdl().unwrap();
    let reparsed = parse_str::<ChildrenMapBTreeRoundTrip>(&rendered).unwrap();
    assert_eq!(reparsed.categories.get("beta").unwrap().size, 9);
    assert_eq!(reparsed.categories.get("gamma").unwrap().size, 3);
}
