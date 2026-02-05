use kdl_config_derive::KdlNode;
use kdl_config::{parse_str, to_kdl};

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
    let parsed = parse_str::<Config>(src).unwrap();
    let rendered = to_kdl(&parsed, "config");
    assert_eq!(rendered.trim(), src);
}

#[test]
fn roundtrip_rerenders_when_dirty() {
    let src = r#"config name="demo" count=42"#;
    let mut parsed = parse_str::<Config>(src).unwrap();
    parsed.name = "changed".to_string();
    let rendered = to_kdl(&parsed, "config");
    assert!(rendered.contains("changed"));
    assert_ne!(rendered.trim(), src);
}
