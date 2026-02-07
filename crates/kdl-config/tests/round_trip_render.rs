use kdl_config::parse_str_roundtrip;
use kdl_config_derive::KdlNode;

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
