use kdl_config::parse_config;
use kdl_config::{KdlNode, Value, parse_str_roundtrip};

#[test]
fn expands_typed_fragments() {
    let doc = r#"
fragment "code" source {
  chunking max-size=1500
}

source "app" local "." {
  with "code"
  include "src/**"
}
"#;

    let root = parse_config(doc).expect("parse");
    assert_eq!(root.children().len(), 1);

    let source = root.child("source").expect("source node");
    let child_names: Vec<_> = source.children().iter().map(|c| c.name.as_str()).collect();
    assert_eq!(child_names.first().copied(), Some("chunking"));
    assert!(child_names.contains(&"include"));
    assert!(source.children_named("with").next().is_none());
}

#[test]
fn typed_fragment_header_defaults_apply_without_tilde_patch() {
    let doc = r#"
fragment "code" source local {
  chunking { max-size 1500 }
}

source "app" {
  with "code"
}
"#;

    let root = parse_config(doc).expect("parse");
    let source = root.child("source").expect("source node");
    assert_eq!(source.arg(0), Some(&Value::String("app".to_string())));
    assert_eq!(source.arg(1), Some(&Value::String("local".to_string())));
    assert!(source.children_named("chunking").next().is_some());
}

#[test]
fn from_materializes_typed_fragment_at_top_level() {
    let doc = r#"
fragment "code" source local {
  include "src/**"
}

from "code" source "app" {
  include "app/**"
}
"#;

    let root = parse_config(doc).expect("parse");
    assert_eq!(root.children().len(), 1);
    let source = root.child("source").expect("materialized source");
    assert_eq!(source.arg(0), Some(&Value::String("app".to_string())));
    assert_eq!(source.arg(1), Some(&Value::String("local".to_string())));
    assert!(
        source
            .children_named("include")
            .any(|node| node.arg(0) == Some(&Value::String("app/**".to_string())))
    );
}

#[test]
fn typed_fragment_names_are_scoped_by_type() {
    let doc = r#"
fragment "code" source local {
  include "src/**"
}

fragment "code" embed onnx {
  model "mini"
}

source "app" {
  with "code"
}

embed "vectors" {
  with "code"
}
"#;

    let root = parse_config(doc).expect("parse");
    let source = root.child("source").expect("source node");
    assert_eq!(source.arg(1), Some(&Value::String("local".to_string())));
    assert!(source.children_named("include").next().is_some());

    let embed = root.child("embed").expect("embed node");
    assert_eq!(embed.arg(1), Some(&Value::String("onnx".to_string())));
    assert!(embed.children_named("model").next().is_some());
}

#[test]
fn rejects_fragment_lookup_for_wrong_type_scope() {
    let doc = r#"
fragment "code" source {
  chunking max-size=1500
}

vectors "default" {
  with "code"
}
"#;

    let err = parse_config(doc).expect_err("should fail");
    let message = format!("{err}");
    assert!(message.contains("unknown fragment"));
    assert!(message.contains("vectors"));
}

#[test]
fn rejects_fragment_without_type_argument() {
    let doc = r#"
fragment "code" {
  chunking max-size=1500
}
"#;

    let err = parse_config(doc).expect_err("should fail");
    let message = format!("{err}");
    assert!(message.contains("must declare a target type"));
}

#[test]
fn rejects_annotation_style_fragment_definition() {
    let doc = r#"
(source)fragment "code" {
  include "src/**"
}
"#;

    let err = parse_config(doc).expect_err("should fail");
    let message = format!("{err}");
    assert!(message.contains("type annotations are not supported"));
}

#[test]
fn expands_fragments_inside_nested_nodes() {
    let doc = r#"
config {
  scope {
    fragment "code" source {
      include "src/**"
    }

    source "app" {
      with "code"
    }
  }
}
"#;

    let root = parse_config(doc).expect("parse");
    let config = root.child("config").expect("config node");
    let scope = config.child("scope").expect("scope node");
    let source = scope.child("source").expect("source child");
    assert!(source.children_named("include").next().is_some());
    assert!(scope.children_named("fragment").next().is_none());
}

#[test]
fn expands_use_overrides() {
    let doc = r#"
config {
  fragment "code" source {
    include "src/**"
  }

  source "app" {
    with "code" {
      exclude "**/tests/**"
    }
  }
}
"#;

    let root = parse_config(doc).expect("parse");
    let config = root.child("config").expect("config node");
    let source = config.child("source").expect("source child");
    assert!(source.children_named("include").next().is_some());
    assert!(source.children_named("exclude").next().is_some());
}

#[test]
fn expands_use_attr_overrides() {
    let doc = r#"
config {
  fragment "code" source {
    chunking { max-size 1500 }
  }

  source "app" {
    with "code" include="src/**" chunking.max-size=2000
  }
}
"#;

    let root = parse_config(doc).expect("parse");
    let config = root.child("config").expect("config node");
    let source = config.child("source").expect("source child");
    assert!(source.children_named("include").next().is_some());

    let has_override = source.children_named("chunking").any(|chunking| {
        chunking
            .child("max-size")
            .and_then(|max_size| max_size.arg(0))
            == Some(&Value::Int(2000))
    });
    assert!(has_override);
}

#[test]
fn expands_use_flag_tokens_as_child_overrides() {
    let doc = r#"
config {
  fragment "code" source {
    include "src/**"
  }

  source "app" {
    with "code" no-enrichment
  }
}
"#;

    let root = parse_config(doc).expect("parse");
    let config = root.child("config").expect("config node");
    let source = config.child("source").expect("source child");
    assert!(source.children_named("no-enrichment").next().is_some());
}

#[test]
fn expands_nested_use_from_fragment_body() {
    let doc = r#"
fragment "b" source {
  include "src/**"
}

fragment "a" source {
  with "b"
}

source "app" local "." {
  with "a"
}
"#;

    let root = parse_config(doc).expect("parse");
    let source = root.child("source").expect("source node");
    assert!(source.children_named("include").next().is_some());
    assert!(source.children_named("with").next().is_none());
}

#[test]
fn nested_fragment_recursion_reports_recursion_chain() {
    let doc = r#"
fragment "a" source {
  with "b"
}

fragment "b" source {
  with "a"
}

source "app" local "." {
  with "a"
}
"#;

    let err = parse_config(doc).expect_err("should fail");
    let message = format!("{err}");
    assert!(message.contains("fragment recursion detected"));
    assert!(message.contains("source::a -> source::b -> source::a"));
    assert!(!message.contains("unknown fragment"));
}

#[test]
fn fragment_and_local_object_override_are_deep_merged() {
    let doc = r#"
fragment "defaults" source {
  extraction {
    mode "semantic"
    limit 20
  }
}

source "app" local "." {
  with "defaults"
  extraction {
    limit 5
    query "latest"
  }
}
"#;

    let root = parse_config(doc).expect("parse");
    let source = root.child("source").expect("source node");
    let extraction = source
        .children_named("extraction")
        .last()
        .expect("merged extraction");

    let mode = extraction
        .child("mode")
        .and_then(|node| node.arg(0))
        .cloned();
    let limit = extraction
        .child("limit")
        .and_then(|node| node.arg(0))
        .cloned();
    let query = extraction
        .child("query")
        .and_then(|node| node.arg(0))
        .cloned();

    assert_eq!(mode, Some(Value::String("semantic".to_string())));
    assert_eq!(limit, Some(Value::Int(5)));
    assert_eq!(query, Some(Value::String("latest".to_string())));
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

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct MultiRoundTripConfig {
    #[kdl(children)]
    source: Vec<MultiRoundTripSource>,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "source")]
struct MultiRoundTripSource {
    #[kdl(positional = 0)]
    name: String,
    #[kdl(child)]
    include: Option<IncludeConfig>,
}

#[test]
fn roundtrip_preserves_fragment_and_use_nodes() {
    let doc = r#"
config {
  fragment "code" source {
    include "src/**"
  }

  source "app" {
    with "code"
  }
}
"#;

    let mut rt = parse_str_roundtrip::<RoundTripConfig>(doc).expect("parse");
    rt.value_mut().source.include.path = "lib/**".to_string();
    let rendered = rt.to_kdl().expect("render");
    assert!(rendered.contains("fragment \"code\" source"));
    assert!(rendered.contains("with \"code\""));
}

#[test]
fn fragment_aware_update_moves_change_into_fragment() {
    let doc = r#"
config {
  fragment "code" source {
    include "src/**"
  }

  source "app" {
    with "code"
  }
}
"#;

    let mut rt = parse_str_roundtrip::<RoundTripConfig>(doc).expect("parse");
    rt.value_mut().source.include.path = "lib/**".to_string();
    let rendered = rt.to_kdl_fragment_aware().expect("render");
    assert!(rendered.contains("with \"code\""));
    assert!(rendered.contains("include \"lib/**\""));
}

#[test]
fn fragment_aware_update_preserves_use_overrides() {
    let doc = r#"config {
  fragment "code" source {
    include "src/**"
  }

  source "app" {
    with "code" {
      exclude "**/tests/**"
    }
  }
}
"#;

    let mut rt = parse_str_roundtrip::<RoundTripConfig>(doc).expect("parse");
    rt.value_mut().source.include.path = "lib/**".to_string();
    let rendered = rt.to_kdl_fragment_aware().expect("render");
    assert!(rendered.contains("with \"code\""));
    assert!(rendered.contains("include \"lib/**\""));
    assert!(rendered.contains("exclude \"**/tests/**\""));
}

#[test]
fn fragment_aware_update_preserves_use_attrs() {
    let doc = r#"config {
  fragment "code" source {
    chunking { max-size 1500 }
  }

  source "app" {
    with "code" include="src/**"
  }
}
"#;

    let mut rt = parse_str_roundtrip::<RoundTripConfig>(doc).expect("parse");
    rt.value_mut().source.include.path = "lib/**".to_string();
    let rendered = rt.to_kdl_fragment_aware().expect("render");
    assert!(rendered.contains("with \"code\" include=\"lib/**\""));
    assert!(!rendered.contains("with \"code\" {"));
}

#[test]
fn fragment_aware_update_emits_remove_override_for_single_use_deletion() {
    let doc = r#"config {
  fragment "code" source {
    include "src/**"
  }

  source "app-one" {
    with "code"
  }

  source "app-two" {
    with "code"
  }
}
"#;

    let mut rt = parse_str_roundtrip::<MultiRoundTripConfig>(doc).expect("parse");
    rt.value_mut().source[0].include = None;
    let rendered = rt.to_kdl_fragment_aware().expect("render");

    assert!(rendered.contains("fragment \"code\""));
    assert!(rendered.contains("include \"src/**\""));
    assert!(rendered.contains("-include"));
}
