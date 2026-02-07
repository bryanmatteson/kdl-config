use kdl_config::{
    ErrorKind, KdlConfigError, KdlNode, parse_config, parse_str, parse_str_roundtrip,
};

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct MinimalConfig {
    #[kdl(attr)]
    name: String,
}

fn assert_rich_parse_error(err: &KdlConfigError) {
    let rendered = err.to_string();
    assert!(
        rendered.contains("line"),
        "expected line information in error: {rendered}"
    );

    match &err.kind {
        ErrorKind::Parse(message) => {
            assert!(
                !message.contains("Failed to parse KDL document"),
                "expected detailed parser message, got generic message: {message}"
            );
            assert!(
                message.contains("detail:")
                    || message.contains("help:")
                    || message.contains("near:")
                    || message.contains("Expected")
                    || message.contains("invalid"),
                "expected parser detail in message: {message}"
            );
        }
        other => panic!("expected parse error, got {other:?}"),
    }
}

#[test]
fn parse_config_reports_kdl_diagnostics() {
    let err = parse_config("config value=1.").expect_err("should fail");
    assert_rich_parse_error(&err);
}

#[test]
fn parse_str_reports_kdl_diagnostics() {
    let err = parse_str::<MinimalConfig>("config name=\"broken").expect_err("should fail");
    assert_rich_parse_error(&err);
}

#[test]
fn parse_str_roundtrip_reports_kdl_diagnostics() {
    let err =
        parse_str_roundtrip::<MinimalConfig>("config name=\"broken").expect_err("should fail");
    assert_rich_parse_error(&err);
}
