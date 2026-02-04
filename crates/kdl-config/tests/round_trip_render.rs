use kdl_config_derive::KdlNode;
use kdl_config::parse_str_roundtrip;

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
    let parsed = parse_str_roundtrip::<Config>(src).unwrap();
    assert_eq!(parsed.to_kdl(), src);
}

#[test]
fn roundtrip_rerenders_when_dirty() {
    let src = r#"config name="demo" count=42"#;
    let mut parsed = parse_str_roundtrip::<Config>(src).unwrap();
    parsed.value_mut().name = "changed".to_string();
    let rendered = parsed.to_kdl();
    assert!(rendered.contains("changed"));
    assert_ne!(rendered, src);
}
