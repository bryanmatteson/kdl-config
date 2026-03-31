use kdl_config::{parse_str, parse_str_roundtrip, to_kdl};
use kdl_config_derive::{Kdl, KdlNode};

#[derive(Debug, Clone, Default, PartialEq, KdlNode)]
#[kdl(node = "provider")]
struct OnnxProvider {
    #[kdl(attr)]
    model: Option<String>,
}

#[derive(Debug, Clone, Default, PartialEq, KdlNode)]
#[kdl(node = "provider")]
struct OllamaProvider {
    #[kdl(attr)]
    model: Option<String>,
    #[kdl(attr)]
    endpoint: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Kdl)]
#[kdl(node = "provider", select = attr("provider"))]
enum SemanticProvider {
    #[kdl(tag = "onnx")]
    Onnx(OnnxProvider),
    #[kdl(tag = "ollama")]
    Ollama(OllamaProvider),
}

#[derive(Debug, Clone, PartialEq, KdlNode)]
#[kdl(node = "semantic", deny_unknown)]
struct SemanticConfig {
    #[kdl(child, tag_attr = "provider", hoist_attrs = any("model"))]
    provider: SemanticProvider,
}

#[test]
fn tagged_child_parses_from_parent_discriminator_without_child_node() {
    let parsed: SemanticConfig =
        parse_str(r#"semantic provider="ollama" model="nomic-embed-code-safe""#).unwrap();

    assert_eq!(
        parsed,
        SemanticConfig {
            provider: SemanticProvider::Ollama(OllamaProvider {
                model: Some("nomic-embed-code-safe".into()),
                endpoint: None,
            }),
        }
    );
}

#[test]
fn tagged_child_parses_hoisted_and_nested_overrides_together() {
    let parsed: SemanticConfig = parse_str(
        r#"
semantic provider="ollama" model="nomic-embed-code-safe" {
    provider endpoint="http://localhost:11434"
}
"#,
    )
    .unwrap();

    assert_eq!(
        parsed,
        SemanticConfig {
            provider: SemanticProvider::Ollama(OllamaProvider {
                model: Some("nomic-embed-code-safe".into()),
                endpoint: Some("http://localhost:11434".into()),
            }),
        }
    );
}

#[test]
fn tagged_child_rejects_parent_and_child_hoist_conflict() {
    let err = parse_str::<SemanticConfig>(
        r#"
semantic provider="ollama" model="parent" {
    provider model="child"
}
"#,
    )
    .unwrap_err();

    let rendered = err.to_string();
    assert!(rendered.contains("parent attribute 'model' conflicts"));
}

#[test]
fn tagged_child_rejects_child_local_discriminator() {
    let err = parse_str::<SemanticConfig>(
        r#"
semantic {
    provider provider="ollama" endpoint="http://localhost:11434"
}
"#,
    )
    .unwrap_err();

    let rendered = err.to_string();
    assert!(rendered.contains("must not set selector attribute 'provider'"));
}

#[test]
fn tagged_child_renders_hoisted_fields_on_parent() {
    let value = SemanticConfig {
        provider: SemanticProvider::Ollama(OllamaProvider {
            model: Some("nomic-embed-code-safe".into()),
            endpoint: Some("http://localhost:11434".into()),
        }),
    };

    let rendered = to_kdl(&value, "semantic");
    assert_eq!(
        rendered.trim(),
        "semantic model=\"nomic-embed-code-safe\" provider=\"ollama\" {\n    provider endpoint=\"http://localhost:11434\"\n}"
    );
}

#[test]
fn tagged_child_omits_empty_child_after_hoisting() {
    let value = SemanticConfig {
        provider: SemanticProvider::Ollama(OllamaProvider {
            model: Some("nomic-embed-code-safe".into()),
            endpoint: None,
        }),
    };

    let rendered = to_kdl(&value, "semantic");
    assert_eq!(
        rendered.trim(),
        "semantic model=\"nomic-embed-code-safe\" provider=\"ollama\""
    );
}

#[test]
fn tagged_child_roundtrip_update_keeps_canonical_parent_attrs() {
    let src = r#"semantic model="nomic-embed-code-safe" provider="ollama" {
    provider endpoint="http://localhost:11434"
}"#;
    let mut parsed = parse_str_roundtrip::<SemanticConfig>(src).unwrap();

    match &mut parsed.value_mut().provider {
        SemanticProvider::Ollama(provider) => {
            provider.endpoint = Some("http://localhost:2244".into());
        }
        other => panic!("expected ollama provider, got {other:?}"),
    }

    let rendered = parsed.to_kdl().unwrap();
    let reparsed: SemanticConfig = parse_str(&rendered).unwrap();
    assert_eq!(reparsed, parsed.value().clone());
    assert!(rendered.contains("provider=ollama"));
    assert!(rendered.contains("model=nomic-embed-code-safe"));
    assert!(rendered.contains("provider endpoint=\"http://localhost:2244\""));
    assert!(!rendered.contains("provider model="));
}
