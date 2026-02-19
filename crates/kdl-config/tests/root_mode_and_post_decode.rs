use kdl_config::{ParseConfig, RootMode, parse_str, parse_str_with_config};

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "config")]
struct WrappedRootConfig {
    name: String,
    count: i64,
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
