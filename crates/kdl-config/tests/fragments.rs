use kdl::KdlDocument;
use kdl_config::parse_config;
use kdl_config::{KdlNode, KdlNodeExt, Value, parse_str_roundtrip};

#[test]
fn expands_typed_fragments() {
    let doc = r#"
(source)fragment "code" {
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
fn rejects_fragment_type_mismatch() {
    let doc = r#"
(source)fragment "code" {
  chunking max-size=1500
}

vectors "default" {
  with "code"
}
"#;

    let err = parse_config(doc).expect_err("should fail");
    let message = format!("{err}");
    assert!(message.contains("fragment"));
    assert!(message.contains("typed as"));
    assert!(message.contains("defined at line"));
}

#[test]
fn allows_fragment_without_type_annotation() {
    let doc = r#"
fragment "code" {
  chunking max-size=1500
}

source "app" local "." {
  with "code"
}
"#;

    let root = parse_config(doc).expect("parse");
    let source = root.child("source").expect("source node");
    assert!(source.children_named("chunking").next().is_some());
}

#[test]
fn untyped_fragment_must_match_parent_context() {
    let doc = r#"
fragment "code" {
  chunking max-size=1500
}

source "app" local "." {
  with "code"
}

vectors "default" {
  with "code"
}
"#;

    let err = parse_config(doc).expect_err("should fail");
    let message = format!("{err}");
    assert!(message.contains("implicitly typed"));
    assert!(message.contains("first used at line"));
}

#[test]
fn expands_fragments_inside_root_node() {
    let doc = r#"
config {
  (source)fragment "code" {
    include "src/**"
  }

  source "app" {
    with "code"
  }
}
"#;

    let root = parse_config(doc).expect("parse");
    let config = root.child("config").expect("config node");
    let source = config.child("source").expect("source child");
    assert!(source.children_named("include").next().is_some());
    assert!(config.children_named("fragment").next().is_none());
}

#[test]
fn expands_fragments_nested_deeper_than_root_children() {
    let doc = r#"
config {
  scope {
    (source)fragment "code" {
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
  (source)fragment "code" {
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
  (source)fragment "code" {
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
  (source)fragment "code" {
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
(source)fragment "b" {
  include "src/**"
}

(source)fragment "a" {
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
(source)fragment "a" {
  with "b"
}

(source)fragment "b" {
  with "a"
}

source "app" local "." {
  with "a"
}
"#;

    let err = parse_config(doc).expect_err("should fail");
    let message = format!("{err}");
    assert!(message.contains("fragment recursion detected"));
    assert!(message.contains("a -> b -> a"));
    assert!(!message.contains("unknown fragment"));
}

#[test]
fn fragment_and_local_object_override_are_deep_merged() {
    let doc = r#"
(source)fragment "defaults" {
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

#[test]
fn expands_fragment_merge_patches() {
    let doc = r#"
fragment "defaults" {
  ~source local {
    chunking { max-size 1500; overlap 200 }
  }
}

source "app" local "." {
  chunking { max-size 500 }
  with "defaults"
}
"#;

    let root = parse_config(doc).expect("parse");
    let source = root.child("source").expect("source node");
    let chunking = source.child("chunking").expect("chunking node");
    let max_size = chunking
        .child("max-size")
        .and_then(|node| node.arg(0))
        .cloned();
    let overlap = chunking
        .child("overlap")
        .and_then(|node| node.arg(0))
        .cloned();
    assert_eq!(max_size, Some(Value::Int(500)));
    assert_eq!(overlap, Some(Value::Int(200)));
}

#[test]
fn fragment_patch_discriminator_mismatch_is_ignored() {
    let doc = r#"
fragment "defaults" {
  ~source local {
    chunking { max-size 1500 }
  }
}

source "app" git "https://example.com/repo.git" {
  with "defaults"
}
"#;

    let root = parse_config(doc).expect("parse");
    let source = root.child("source").expect("source node");
    assert!(source.children_named("chunking").next().is_none());
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
  (source)fragment "code" {
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
    assert!(rendered.contains("(source)fragment"));
    assert!(rendered.contains("with \"code\""));
}

#[test]
fn fragment_aware_update_moves_change_into_fragment() {
    let doc = r#"
config {
  (source)fragment "code" {
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
    let parsed: KdlDocument = rendered.parse().expect("kdl");
    let config = parsed
        .nodes()
        .iter()
        .find(|node| node.base_name() == "config")
        .expect("config node");
    let config_children = config.children().map(|doc| doc.nodes()).unwrap_or(&[]);
    let fragment = config_children
        .iter()
        .find(|node| node.base_name() == "fragment")
        .expect("fragment node");
    let fragment_children = fragment.children().map(|doc| doc.nodes()).unwrap_or(&[]);
    assert!(fragment_children.iter().any(|node| {
        node.base_name() == "include"
            && node.get(0).and_then(|val| val.as_string()) == Some("lib/**")
    }));

    let source = config_children
        .iter()
        .find(|node| node.base_name() == "source")
        .expect("source node");
    let source_children = source.children().map(|doc| doc.nodes()).unwrap_or(&[]);
    assert!(
        source_children
            .iter()
            .any(|node| node.base_name() == "with")
    );
    assert!(
        source_children
            .iter()
            .all(|node| node.base_name() != "include")
    );
}

#[test]
fn fragment_aware_update_preserves_use_overrides() {
    let doc = r#"config {
  (source)fragment "code" {
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
    let parsed: KdlDocument = rendered.parse().expect("kdl");
    let config = parsed
        .nodes()
        .iter()
        .find(|node| node.base_name() == "config")
        .expect("config node");
    let config_children = config.children().map(|doc| doc.nodes()).unwrap_or(&[]);
    let fragment = config_children
        .iter()
        .find(|node| node.base_name() == "fragment")
        .expect("fragment node");
    let fragment_children = fragment.children().map(|doc| doc.nodes()).unwrap_or(&[]);
    assert!(fragment_children.iter().any(|node| {
        node.base_name() == "include"
            && node.get(0).and_then(|val| val.as_string()) == Some("lib/**")
    }));

    let source = config_children
        .iter()
        .find(|node| node.base_name() == "source")
        .expect("source node");
    let source_children = source.children().map(|doc| doc.nodes()).unwrap_or(&[]);
    let use_node = source_children
        .iter()
        .find(|node| node.base_name() == "with")
        .expect("use node");
    let use_children = use_node.children().map(|doc| doc.nodes()).unwrap_or(&[]);
    assert!(
        use_children
            .iter()
            .any(|node| node.base_name() == "exclude")
    );
}

#[test]
fn fragment_aware_update_preserves_use_attrs() {
    let doc = r#"config {
  (source)fragment "code" {
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
  (source)fragment "code" {
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
