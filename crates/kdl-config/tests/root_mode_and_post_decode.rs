use kdl_config::{ParseConfig, RootMode, parse_str, parse_str_with_config, to_kdl, to_kdl_document};

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "config")]
struct WrappedRootConfig {
    name: String,
    count: i64,
}

#[derive(Debug, PartialEq, kdl_config::Kdl)]
#[kdl(node = "name")]
struct NameNode {
    #[kdl(value)]
    value: String,
}

#[derive(Debug, PartialEq, kdl_config::Kdl)]
#[kdl(node = "count")]
struct CountNode {
    #[kdl(value)]
    value: i64,
}

#[derive(Debug, PartialEq, kdl_config::Kdl)]
#[kdl(node = "config")]
struct FlatDocConfig {
    #[kdl(child)]
    name: NameNode,
    #[kdl(child)]
    count: CountNode,
}

#[test]
fn wrap_expected_root_mode_parses_unwrapped_documents() {
    let input = r#"
name "alpha"
count 3
"#;
    assert!(parse_str::<WrappedRootConfig>(input).is_err());

    let config = ParseConfig {
        root_mode: RootMode::WrapExpectedNode {
            name: "config".to_string(),
        },
        ..ParseConfig::default()
    };

    let parsed: WrappedRootConfig = parse_str_with_config(input, &config).unwrap();
    assert_eq!(
        parsed,
        WrappedRootConfig {
            name: "alpha".to_string(),
            count: 3,
        }
    );
}

#[test]
fn document_root_mode_parses_flat_documents() {
    let input = r#"
name "alpha"
count 3
"#;
    // Strict still rejects a flat document.
    assert!(parse_str::<WrappedRootConfig>(input).is_err());

    let config = ParseConfig {
        root_mode: RootMode::Document {
            name: "config".to_string(),
        },
        ..ParseConfig::default()
    };

    let parsed: WrappedRootConfig = parse_str_with_config(input, &config).unwrap();
    assert_eq!(
        parsed,
        WrappedRootConfig {
            name: "alpha".to_string(),
            count: 3,
        }
    );
}

#[test]
fn document_root_mode_rejects_wrapped_documents() {
    // `Document` mode always wraps. If the on-disk text already carries the
    // wrapper, the inner re-parse sees `config { config { ... } }` and the
    // inner node fails to decode into `WrappedRootConfig`'s fields. This
    // asserts the strict-vs-flat contract: a `Document` mode caller is
    // telling kdl-config the file has no outer wrapper.
    let input = r#"
config {
    name "alpha"
    count 3
}
"#;
    let config = ParseConfig {
        root_mode: RootMode::Document {
            name: "config".to_string(),
        },
        ..ParseConfig::default()
    };
    assert!(parse_str_with_config::<WrappedRootConfig>(input, &config).is_err());
}

#[test]
fn document_root_mode_round_trips_flat_documents() {
    let value = FlatDocConfig {
        name: NameNode {
            value: "alpha".to_string(),
        },
        count: CountNode { value: 3 },
    };
    let flat = to_kdl_document(&value, "config");
    // Flat output must not contain the outer wrapper.
    assert!(!flat.contains("config {"));
    // Each child is rendered as a top-level node in the flat form.
    assert!(flat.contains("value \"alpha\""));
    assert!(flat.contains("value 3"));
    // The top-level lines are the wrapper's children, dedented.
    assert!(flat.starts_with("count") || flat.starts_with("name"));

    let config = ParseConfig {
        root_mode: RootMode::Document {
            name: "config".to_string(),
        },
        ..ParseConfig::default()
    };
    let parsed: FlatDocConfig = parse_str_with_config(&flat, &config).unwrap();
    assert_eq!(parsed, value);
}

#[test]
fn to_kdl_document_strips_outer_wrapper() {
    let value = FlatDocConfig {
        name: NameNode {
            value: "alpha".to_string(),
        },
        count: CountNode { value: 3 },
    };
    let wrapped = to_kdl(&value, "config");
    let flat = to_kdl_document(&value, "config");
    // The wrapped form has an outer `config { ... }` block.
    assert!(wrapped.starts_with("config {"));
    // The flat form drops that wrapper and dedents children.
    assert!(!flat.starts_with("config {"));
    assert!(flat.starts_with("name") || flat.starts_with("count"));
    assert!(flat.contains("value \"alpha\""));
    assert!(flat.contains("value 3"));
    // Trailing newline contract.
    assert!(flat.ends_with('\n'));
}

#[test]
fn strict_root_mode_still_works() {
    // Strict requires the on-disk wrapper.
    let input = r#"
config {
    name "alpha"
    count 3
}
"#;
    let parsed: WrappedRootConfig = parse_str(input).unwrap();
    assert_eq!(
        parsed,
        WrappedRootConfig {
            name: "alpha".to_string(),
            count: 3,
        }
    );
}

fn normalize_name(value: &mut PostDecodeConfig) -> Result<(), String> {
    value.name = value.name.trim().to_string();
    if value.name.is_empty() {
        return Err("name must not be empty".to_string());
    }
    Ok(())
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "config", post_decode(func = "normalize_name"))]
struct PostDecodeConfig {
    #[kdl(attr)]
    name: String,
}

#[test]
fn post_decode_hook_can_normalize_values() {
    let parsed: PostDecodeConfig = parse_str(r#"config name="  example  ""#).unwrap();
    assert_eq!(
        parsed,
        PostDecodeConfig {
            name: "example".to_string(),
        }
    );
}

#[test]
fn post_decode_hook_can_reject_invalid_values() {
    let err = parse_str::<PostDecodeConfig>(r#"config name="   ""#).unwrap_err();
    assert!(err.to_string().contains("name must not be empty"));
}
