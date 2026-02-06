use kdl_config::parse_config;

#[test]
fn expands_typed_templates() {
    let doc = r#"
(source)template "code" {
  chunking max-size=1500
}

source "app" local "." {
  use "code"
  include "src/**"
}
"#;

    let root = parse_config(doc).expect("parse");
    assert_eq!(root.children().len(), 1);

    let source = root.child("source").expect("source node");
    let child_names: Vec<_> = source.children().iter().map(|c| c.name.as_str()).collect();
    assert_eq!(child_names.first().copied(), Some("chunking"));
    assert!(child_names.contains(&"include"));
    assert!(source.children_named("use").next().is_none());
}

#[test]
fn rejects_template_type_mismatch() {
    let doc = r#"
(source)template "code" {
  chunking max-size=1500
}

vectors "default" {
  use "code"
}
"#;

    let err = parse_config(doc).expect_err("should fail");
    let message = format!("{err}");
    assert!(message.contains("template"));
    assert!(message.contains("typed as"));
}

#[test]
fn requires_template_type_annotation() {
    let doc = r#"
template "code" {
  chunking max-size=1500
}

source "app" local "." {
  use "code"
}
"#;

    let err = parse_config(doc).expect_err("should fail");
    let message = format!("{err}");
    assert!(message.contains("type annotation"));
}
