use kdl_config::to_kdl;
use kdl_config_derive::KdlNode;

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config", skip_serialize_none, skip_serialize_empty_collections)]
struct SkipSerializeTagsConfig {
    #[kdl(attr)]
    name: String,
    #[kdl(attr)]
    maybe_label: Option<String>,
    #[kdl(value)]
    maybe_ports: Option<Vec<i64>>,
    #[kdl(value, no_skip_serialize)]
    maybe_values: Option<Vec<i64>>,
}

#[test]
fn skip_serialize_tags_are_accepted() {
    let cfg = SkipSerializeTagsConfig {
        name: "demo".to_string(),
        maybe_label: None,
        maybe_ports: Some(vec![]),
        maybe_values: Some(vec![]),
    };

    let rendered = to_kdl(&cfg, "config");
    assert_eq!(rendered.trim(), r#"config name="demo""#);
}
