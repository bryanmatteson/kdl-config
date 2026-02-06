use kdl_config::parse_config;
use kdl_config::{KdlNode, KdlNodeExt, parse_str_roundtrip};
use kdl::KdlDocument;

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

#[test]
fn expands_templates_inside_root_node() {
    let doc = r#"
config {
  (source)template "code" {
    include "src/**"
  }

  source "app" {
    use "code"
  }
}
"#;

    let root = parse_config(doc).expect("parse");
    let config = root.child("config").expect("config node");
    let source = config.child("source").expect("source child");
    assert!(source.children_named("include").next().is_some());
    assert!(config.children_named("template").next().is_none());
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct RoundTripConfig {
    #[kdl(child)]
    source: SourceConfig,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "source")]
struct SourceConfig {
    #[kdl(child)]
    include: IncludeConfig,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "include")]
struct IncludeConfig {
    #[kdl(positional = 0)]
    path: String,
}

#[test]
fn roundtrip_preserves_template_and_use_nodes() {
    let doc = r#"
config {
  (source)template "code" {
    include "src/**"
  }

  source "app" {
    use "code"
  }
}
"#;

    let mut rt = parse_str_roundtrip::<RoundTripConfig>(doc).expect("parse");
    rt.value_mut().source.include.path = "lib/**".to_string();
    let rendered = rt.to_kdl().expect("render");
    assert!(rendered.contains("(source)template"));
    assert!(rendered.contains("use \"code\""));
}

#[test]
fn template_aware_update_moves_change_into_template() {
    let doc = r#"
config {
  (source)template "code" {
    include "src/**"
  }

  source "app" {
    use "code"
  }
}
"#;

    let mut rt = parse_str_roundtrip::<RoundTripConfig>(doc).expect("parse");
    rt.value_mut().source.include.path = "lib/**".to_string();
    let rendered = rt.to_kdl_template_aware().expect("render");
    let parsed: KdlDocument = rendered.parse().expect("kdl");
    let config = parsed
        .nodes()
        .iter()
        .find(|node| node.base_name() == "config")
        .expect("config node");
    let config_children = config.children().map(|doc| doc.nodes()).unwrap_or(&[]);
    let template = config_children
        .iter()
        .find(|node| node.base_name() == "template")
        .expect("template node");
    let template_children = template
        .children()
        .map(|doc| doc.nodes())
        .unwrap_or(&[]);
    assert!(template_children.iter().any(|node| {
        node.base_name() == "include"
            && node.get(0).and_then(|val| val.as_string()) == Some("lib/**")
    }));

    let source = config_children
        .iter()
        .find(|node| node.base_name() == "source")
        .expect("source node");
    let source_children = source.children().map(|doc| doc.nodes()).unwrap_or(&[]);
    assert!(source_children.iter().any(|node| node.base_name() == "use"));
    assert!(source_children
        .iter()
        .all(|node| node.base_name() != "include"));
}
