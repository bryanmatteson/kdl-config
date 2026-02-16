use kdl_config::parse_str;

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "server")]
struct ServerConfig {
    host: String,
    #[kdl(validate(min = 1, max = 65535))]
    port: u32,
    #[kdl(validate(non_empty, max_len = 255))]
    name: String,
}

#[test]
fn valid_values_pass_validation() {
    let cfg: ServerConfig =
        parse_str("server host=\"localhost\" port=8080 name=\"my-server\"").unwrap();
    assert_eq!(cfg.port, 8080);
    assert_eq!(cfg.name, "my-server");
}

#[test]
fn port_zero_fails_min_validation() {
    let err =
        parse_str::<ServerConfig>("server host=\"localhost\" port=0 name=\"ok\"").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("less than minimum"), "got: {msg}");
}

#[test]
fn empty_name_fails_non_empty_validation() {
    let err = parse_str::<ServerConfig>("server host=\"localhost\" port=80 name=\"\"").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("must not be empty"), "got: {msg}");
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "range")]
struct RangeConfig {
    #[kdl(validate(positive))]
    start: i32,
    #[kdl(validate(positive))]
    end: i32,
}

#[test]
fn positive_validation_rejects_zero() {
    let err = parse_str::<RangeConfig>("range start=0 end=10").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("not positive"), "got: {msg}");
}

#[test]
fn positive_validation_rejects_negative() {
    let err = parse_str::<RangeConfig>("range start=-1 end=10").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("not positive"), "got: {msg}");
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "items")]
struct ItemsConfig {
    #[kdl(
        name = "entry",
        children,
        optional,
        validate(min_items = 1, max_items = 5)
    )]
    entries: Vec<Entry>,
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "entry")]
struct Entry {
    value: String,
}

#[test]
fn min_items_rejects_empty_vec() {
    let err = parse_str::<ItemsConfig>("items").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("less than minimum"), "got: {msg}");
}

#[test]
fn max_items_rejects_too_many() {
    let input = r#"items {
        entry value="a"
        entry value="b"
        entry value="c"
        entry value="d"
        entry value="e"
        entry value="f"
    }"#;
    let err = parse_str::<ItemsConfig>(input).unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("exceeds maximum"), "got: {msg}");
}

#[test]
fn valid_item_count_passes() {
    let input = r#"items {
        entry value="a"
        entry value="b"
    }"#;
    let cfg: ItemsConfig = parse_str(input).unwrap();
    assert_eq!(cfg.entries.len(), 2);
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "limits")]
struct LimitsConfig {
    #[kdl(validate(less_than = "max_val"))]
    min_val: f64,
    max_val: f64,
}

#[test]
fn cross_field_less_than_passes() {
    let cfg: LimitsConfig = parse_str("limits min_val=1.0 max_val=10.0").unwrap();
    assert_eq!(cfg.min_val, 1.0);
    assert_eq!(cfg.max_val, 10.0);
}

#[test]
fn cross_field_less_than_fails_when_equal() {
    let err = parse_str::<LimitsConfig>("limits min_val=10.0 max_val=10.0").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("must be less than"), "got: {msg}");
}

#[test]
fn cross_field_less_than_fails_when_greater() {
    let err = parse_str::<LimitsConfig>("limits min_val=20.0 max_val=10.0").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("must be less than"), "got: {msg}");
}

fn validate_even(val: &i32) -> Result<(), String> {
    if val % 2 != 0 {
        Err(format!("{} is not even", val))
    } else {
        Ok(())
    }
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "func_test")]
struct FuncValidation {
    #[kdl(validate(func = "validate_even"))]
    count: i32,
}

#[test]
fn func_validation_passes_even() {
    let cfg: FuncValidation = parse_str("func_test count=4").unwrap();
    assert_eq!(cfg.count, 4);
}

#[test]
fn func_validation_rejects_odd() {
    let err = parse_str::<FuncValidation>("func_test count=3").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("not even"), "got: {msg}");
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "optional_test")]
struct OptionalValidation {
    #[kdl(validate(min = 0, max = 100))]
    score: Option<i32>,
}

#[test]
fn optional_none_skips_validation() {
    let cfg: OptionalValidation = parse_str("optional_test").unwrap();
    assert_eq!(cfg.score, None);
}

#[test]
fn optional_some_valid_passes() {
    let cfg: OptionalValidation = parse_str("optional_test score=50").unwrap();
    assert_eq!(cfg.score, Some(50));
}

#[test]
fn optional_some_invalid_fails() {
    let err = parse_str::<OptionalValidation>("optional_test score=200").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("exceeds maximum"), "got: {msg}");
}

// --- Struct-level validation ---

fn check_server_invariants(s: &ValidatedServer) -> Result<(), String> {
    if s.host == "localhost" && s.port > 1024 {
        return Err("localhost must use a privileged port".to_string());
    }
    Ok(())
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "server", validate(func = "check_server_invariants"))]
struct ValidatedServer {
    host: String,
    port: i64,
}

#[test]
fn struct_level_validation_passes() {
    let cfg: ValidatedServer = parse_str("server host=\"example.com\" port=8080").unwrap();
    assert_eq!(cfg.host, "example.com");
}

#[test]
fn struct_level_validation_fails() {
    let err = parse_str::<ValidatedServer>("server host=\"localhost\" port=8080").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("privileged port"), "got: {msg}");
}

#[test]
fn struct_level_validation_localhost_privileged_passes() {
    let cfg: ValidatedServer = parse_str("server host=\"localhost\" port=80").unwrap();
    assert_eq!(cfg.port, 80);
}
