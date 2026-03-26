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

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "func_test_call_style")]
struct FuncValidationCallStyle {
    #[kdl(validate(func("validate_even")))]
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

#[test]
fn func_validation_call_style_passes_even() {
    let cfg: FuncValidationCallStyle = parse_str("func_test_call_style count=4").unwrap();
    assert_eq!(cfg.count, 4);
}

#[test]
fn func_validation_call_style_rejects_odd() {
    let err = parse_str::<FuncValidationCallStyle>("func_test_call_style count=3").unwrap_err();
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

// --- Cross-field with Option ---

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "opt_limits")]
struct OptionalLimits {
    #[kdl(validate(less_than = "max_val"))]
    min_val: Option<f64>,
    max_val: Option<f64>,
}

#[test]
fn cross_field_both_none_skips() {
    let cfg: OptionalLimits = parse_str("opt_limits").unwrap();
    assert_eq!(cfg.min_val, None);
    assert_eq!(cfg.max_val, None);
}

#[test]
fn cross_field_this_none_skips() {
    let cfg: OptionalLimits = parse_str("opt_limits max_val=10.0").unwrap();
    assert_eq!(cfg.min_val, None);
    assert_eq!(cfg.max_val, Some(10.0));
}

#[test]
fn cross_field_other_none_skips() {
    let cfg: OptionalLimits = parse_str("opt_limits min_val=5.0").unwrap();
    assert_eq!(cfg.min_val, Some(5.0));
    assert_eq!(cfg.max_val, None);
}

#[test]
fn cross_field_both_some_valid() {
    let cfg: OptionalLimits = parse_str("opt_limits min_val=1.0 max_val=10.0").unwrap();
    assert_eq!(cfg.min_val, Some(1.0));
}

#[test]
fn cross_field_both_some_invalid() {
    let err = parse_str::<OptionalLimits>("opt_limits min_val=20.0 max_val=10.0").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("must be less than"), "got: {msg}");
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

fn check_server_invariants_call_style(s: &ValidatedServerCallStyle) -> Result<(), String> {
    if s.host == "localhost" && s.port > 1024 {
        return Err("localhost must use a privileged port".to_string());
    }
    Ok(())
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(
    node = "server_call_style",
    validate(func("check_server_invariants_call_style"))
)]
struct ValidatedServerCallStyle {
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

#[test]
fn struct_level_validation_call_style_passes() {
    let cfg: ValidatedServerCallStyle =
        parse_str("server_call_style host=\"example.com\" port=8080").unwrap();
    assert_eq!(cfg.host, "example.com");
}

#[test]
fn struct_level_validation_call_style_fails() {
    let err =
        parse_str::<ValidatedServerCallStyle>("server_call_style host=\"localhost\" port=8080")
            .unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("privileged port"), "got: {msg}");
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "membership")]
struct MembershipConfig {
    #[kdl(attr, validate(exists_in = "allowed"))]
    item: String,
    #[kdl(attr, value, conflict = "append")]
    allowed: Vec<String>,
}

#[test]
fn exists_in_validation_passes_when_value_is_present() {
    let cfg: MembershipConfig =
        parse_str(r#"membership item="beta" allowed="alpha" allowed="beta""#).unwrap();
    assert_eq!(cfg.item, "beta");
}

#[test]
fn exists_in_validation_fails_when_value_is_missing() {
    let err =
        parse_str::<MembershipConfig>(r#"membership item="gamma" allowed="alpha" allowed="beta""#)
            .unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("must exist in"), "got: {msg}");
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "subset")]
struct SubsetValidationConfig {
    #[kdl(attr, value, conflict = "append", validate(subset_of = "allowed"))]
    selected: Vec<String>,
    #[kdl(attr, value, conflict = "append")]
    allowed: Vec<String>,
}

#[test]
fn subset_of_validation_passes_when_subset_matches() {
    let input = r#"subset selected="alpha" allowed="alpha" {
        selected "beta"
        allowed "beta"
        allowed "gamma"
    }"#;
    let cfg: SubsetValidationConfig = parse_str(input).unwrap();
    assert_eq!(cfg.selected, vec!["alpha".to_string(), "beta".to_string()]);
}

#[test]
fn subset_of_validation_fails_when_values_escape_allowed_set() {
    let input = r#"subset selected="alpha" allowed="alpha" {
        selected "delta"
        allowed "beta"
    }"#;
    let err = parse_str::<SubsetValidationConfig>(input).unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("subset"), "got: {msg}");
}

// ==========================================================================
// Error collection: multiple validation failures are reported together
// ==========================================================================

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "multi")]
struct MultiValidation {
    #[kdl(validate(min = 1, max = 100))]
    a: i32,
    #[kdl(validate(min = 1, max = 100))]
    b: i32,
}

#[test]
fn multiple_field_failures_collected() {
    let err = parse_str::<MultiValidation>("multi a=0 b=0").unwrap_err();
    let msg = err.to_string();
    // Both fields fail — error should contain both
    assert!(msg.contains("validation errors"), "expected multiple errors, got: {msg}");
    assert!(msg.contains("'a'"), "expected field 'a' mentioned, got: {msg}");
    assert!(msg.contains("'b'"), "expected field 'b' mentioned, got: {msg}");
}

#[test]
fn single_failure_no_wrapper() {
    let err = parse_str::<MultiValidation>("multi a=0 b=50").unwrap_err();
    let msg = err.to_string();
    // Only field 'a' fails — should be a direct error, not wrapped
    assert!(msg.contains("'a'"), "expected field 'a' mentioned, got: {msg}");
    assert!(
        !msg.contains("validation errors"),
        "single error should not be wrapped, got: {msg}"
    );
}

// ==========================================================================
// range() validation
// ==========================================================================

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "rng")]
struct RangeValidation {
    #[kdl(validate(range(10, 20)))]
    val: i32,
}

#[test]
fn range_passes_at_lower_bound() {
    let cfg: RangeValidation = parse_str("rng val=10").unwrap();
    assert_eq!(cfg.val, 10);
}

#[test]
fn range_passes_at_upper_bound() {
    let cfg: RangeValidation = parse_str("rng val=20").unwrap();
    assert_eq!(cfg.val, 20);
}

#[test]
fn range_passes_in_middle() {
    let cfg: RangeValidation = parse_str("rng val=15").unwrap();
    assert_eq!(cfg.val, 15);
}

#[test]
fn range_rejects_below() {
    let err = parse_str::<RangeValidation>("rng val=9").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("not in range"), "got: {msg}");
}

#[test]
fn range_rejects_above() {
    let err = parse_str::<RangeValidation>("rng val=21").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("not in range"), "got: {msg}");
}

// ==========================================================================
// multiple_of validation
// ==========================================================================

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "mul")]
struct MultipleOfValidation {
    #[kdl(validate(multiple_of = 5))]
    val: i32,
}

#[test]
fn multiple_of_passes() {
    let cfg: MultipleOfValidation = parse_str("mul val=15").unwrap();
    assert_eq!(cfg.val, 15);
}

#[test]
fn multiple_of_passes_zero() {
    let cfg: MultipleOfValidation = parse_str("mul val=0").unwrap();
    assert_eq!(cfg.val, 0);
}

#[test]
fn multiple_of_rejects() {
    let err = parse_str::<MultipleOfValidation>("mul val=7").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("not a multiple"), "got: {msg}");
}

// ==========================================================================
// negative / non_negative / non_positive
// ==========================================================================

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "neg")]
struct NegativeValidation {
    #[kdl(validate(negative))]
    val: i32,
}

#[test]
fn negative_passes() {
    let cfg: NegativeValidation = parse_str("neg val=-1").unwrap();
    assert_eq!(cfg.val, -1);
}

#[test]
fn negative_rejects_zero() {
    let err = parse_str::<NegativeValidation>("neg val=0").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("not negative"), "got: {msg}");
}

#[test]
fn negative_rejects_positive() {
    let err = parse_str::<NegativeValidation>("neg val=1").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("not negative"), "got: {msg}");
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "nn")]
struct NonNegativeValidation {
    #[kdl(validate(non_negative))]
    val: i32,
}

#[test]
fn non_negative_passes_zero() {
    let cfg: NonNegativeValidation = parse_str("nn val=0").unwrap();
    assert_eq!(cfg.val, 0);
}

#[test]
fn non_negative_passes_positive() {
    let cfg: NonNegativeValidation = parse_str("nn val=5").unwrap();
    assert_eq!(cfg.val, 5);
}

#[test]
fn non_negative_rejects_negative() {
    let err = parse_str::<NonNegativeValidation>("nn val=-1").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("not non-negative"), "got: {msg}");
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "np")]
struct NonPositiveValidation {
    #[kdl(validate(non_positive))]
    val: i32,
}

#[test]
fn non_positive_passes_zero() {
    let cfg: NonPositiveValidation = parse_str("np val=0").unwrap();
    assert_eq!(cfg.val, 0);
}

#[test]
fn non_positive_passes_negative() {
    let cfg: NonPositiveValidation = parse_str("np val=-3").unwrap();
    assert_eq!(cfg.val, -3);
}

#[test]
fn non_positive_rejects_positive() {
    let err = parse_str::<NonPositiveValidation>("np val=1").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("not non-positive"), "got: {msg}");
}

// ==========================================================================
// len() validation (combined min_len + max_len)
// ==========================================================================

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "ln")]
struct LenValidation {
    #[kdl(validate(len(2, 5)))]
    val: String,
}

#[test]
fn len_passes_at_lower_bound() {
    let cfg: LenValidation = parse_str(r#"ln val="ab""#).unwrap();
    assert_eq!(cfg.val, "ab");
}

#[test]
fn len_passes_at_upper_bound() {
    let cfg: LenValidation = parse_str(r#"ln val="abcde""#).unwrap();
    assert_eq!(cfg.val, "abcde");
}

#[test]
fn len_rejects_too_short() {
    let err = parse_str::<LenValidation>(r#"ln val="a""#).unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("not in range"), "got: {msg}");
}

#[test]
fn len_rejects_too_long() {
    let err = parse_str::<LenValidation>(r#"ln val="abcdef""#).unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("not in range"), "got: {msg}");
}

// ==========================================================================
// min_chars / max_chars / chars validation (character count, not byte length)
// ==========================================================================

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "mc")]
struct MinCharsValidation {
    #[kdl(validate(min_chars = 3))]
    val: String,
}

#[test]
fn min_chars_passes_ascii() {
    let cfg: MinCharsValidation = parse_str(r#"mc val="abc""#).unwrap();
    assert_eq!(cfg.val, "abc");
}

#[test]
fn min_chars_rejects_too_few() {
    let err = parse_str::<MinCharsValidation>(r#"mc val="ab""#).unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("character count"), "got: {msg}");
}

#[test]
fn min_chars_counts_unicode_chars_not_bytes() {
    // "héllo" is 5 chars but 6 bytes (é = 2 bytes in UTF-8)
    let cfg: MinCharsValidation = parse_str(r#"mc val="héllo""#).unwrap();
    assert_eq!(cfg.val, "héllo");
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "xc")]
struct MaxCharsValidation {
    #[kdl(validate(max_chars = 3))]
    val: String,
}

#[test]
fn max_chars_passes() {
    let cfg: MaxCharsValidation = parse_str(r#"xc val="abc""#).unwrap();
    assert_eq!(cfg.val, "abc");
}

#[test]
fn max_chars_rejects_too_many() {
    let err = parse_str::<MaxCharsValidation>(r#"xc val="abcd""#).unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("character count"), "got: {msg}");
}

#[test]
fn max_chars_counts_unicode_chars_not_bytes() {
    // "é" is 1 char but 2 bytes — should pass max_chars = 3
    let cfg: MaxCharsValidation = parse_str(r#"xc val="éàü""#).unwrap();
    assert_eq!(cfg.val, "éàü");
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "cc")]
struct CharsRangeValidation {
    #[kdl(validate(chars(2, 5)))]
    val: String,
}

#[test]
fn chars_range_passes() {
    let cfg: CharsRangeValidation = parse_str(r#"cc val="abc""#).unwrap();
    assert_eq!(cfg.val, "abc");
}

#[test]
fn chars_range_rejects_below() {
    let err = parse_str::<CharsRangeValidation>(r#"cc val="a""#).unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("character count"), "got: {msg}");
}

#[test]
fn chars_range_rejects_above() {
    let err = parse_str::<CharsRangeValidation>(r#"cc val="abcdef""#).unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("character count"), "got: {msg}");
}

// ==========================================================================
// ascii validation
// ==========================================================================

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "asc")]
struct AsciiValidation {
    #[kdl(validate(ascii))]
    val: String,
}

#[test]
fn ascii_passes() {
    let cfg: AsciiValidation = parse_str(r#"asc val="hello""#).unwrap();
    assert_eq!(cfg.val, "hello");
}

#[test]
fn ascii_rejects_unicode() {
    let err = parse_str::<AsciiValidation>(r#"asc val="héllo""#).unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("ASCII"), "got: {msg}");
}

// ==========================================================================
// alphanumeric validation
// ==========================================================================

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "alnum")]
struct AlphanumericValidation {
    #[kdl(validate(alphanumeric))]
    val: String,
}

#[test]
fn alphanumeric_passes() {
    let cfg: AlphanumericValidation = parse_str(r#"alnum val="abc123""#).unwrap();
    assert_eq!(cfg.val, "abc123");
}

#[test]
fn alphanumeric_rejects_punctuation() {
    let err = parse_str::<AlphanumericValidation>(r#"alnum val="abc-123""#).unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("alphanumeric"), "got: {msg}");
}

// ==========================================================================
// Cross-field: lte, gt, gte, eq, neq
// ==========================================================================

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "cflte")]
struct LteValidation {
    #[kdl(validate(less_than_or_equal = "b"))]
    a: f64,
    b: f64,
}

#[test]
fn lte_passes_equal() {
    let cfg: LteValidation = parse_str("cflte a=5.0 b=5.0").unwrap();
    assert_eq!(cfg.a, 5.0);
}

#[test]
fn lte_passes_less() {
    let cfg: LteValidation = parse_str("cflte a=3.0 b=5.0").unwrap();
    assert_eq!(cfg.a, 3.0);
}

#[test]
fn lte_rejects_greater() {
    let err = parse_str::<LteValidation>("cflte a=6.0 b=5.0").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("less than or equal"), "got: {msg}");
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "cfgt")]
struct GtValidation {
    #[kdl(validate(greater_than = "b"))]
    a: f64,
    b: f64,
}

#[test]
fn gt_passes() {
    let cfg: GtValidation = parse_str("cfgt a=5.0 b=3.0").unwrap();
    assert_eq!(cfg.a, 5.0);
}

#[test]
fn gt_rejects_equal() {
    let err = parse_str::<GtValidation>("cfgt a=5.0 b=5.0").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("greater than"), "got: {msg}");
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "gte")]
struct GteValidation {
    #[kdl(validate(greater_than_or_equal = "b"))]
    a: f64,
    b: f64,
}

#[test]
fn gte_passes_equal() {
    let cfg: GteValidation = parse_str("gte a=5.0 b=5.0").unwrap();
    assert_eq!(cfg.a, 5.0);
}

#[test]
fn gte_rejects_less() {
    let err = parse_str::<GteValidation>("gte a=3.0 b=5.0").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("greater than or equal"), "got: {msg}");
}

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "eqn")]
struct EqNeqValidation {
    #[kdl(validate(equal_to = "b"))]
    a: f64,
    #[kdl(validate(not_equal_to = "a"))]
    c: f64,
    b: f64,
}

#[test]
fn eq_passes() {
    let cfg: EqNeqValidation = parse_str("eqn a=5.0 b=5.0 c=3.0").unwrap();
    assert_eq!(cfg.a, 5.0);
}

#[test]
fn eq_rejects_different() {
    let err = parse_str::<EqNeqValidation>("eqn a=5.0 b=6.0 c=3.0").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("must equal"), "got: {msg}");
}

#[test]
fn neq_passes() {
    let cfg: EqNeqValidation = parse_str("eqn a=5.0 b=5.0 c=3.0").unwrap();
    assert_eq!(cfg.c, 3.0);
}

#[test]
fn neq_rejects_equal() {
    let err = parse_str::<EqNeqValidation>("eqn a=5.0 b=5.0 c=5.0").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("must not equal"), "got: {msg}");
}

// ==========================================================================
// pattern validation (runtime with regex feature)
// ==========================================================================

#[cfg(feature = "regex")]
mod pattern_tests {
    use kdl_config::parse_str;

    #[derive(Debug, PartialEq, kdl_config::KdlNode)]
    #[kdl(node = "pat")]
    struct PatternValidation {
        #[kdl(validate(pattern = "^[a-z]+$"))]
        val: String,
    }

    #[test]
    fn pattern_passes() {
        let cfg: PatternValidation = parse_str(r#"pat val="hello""#).unwrap();
        assert_eq!(cfg.val, "hello");
    }

    #[test]
    fn pattern_rejects() {
        let err = parse_str::<PatternValidation>(r#"pat val="Hello123""#).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("does not match pattern"), "got: {msg}");
    }
}

// ==========================================================================
// Boundary value tests
// ==========================================================================

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "bnd")]
struct BoundaryValidation {
    #[kdl(validate(min = 5, max = 10))]
    val: i32,
}

#[test]
fn boundary_min_passes() {
    let cfg: BoundaryValidation = parse_str("bnd val=5").unwrap();
    assert_eq!(cfg.val, 5);
}

#[test]
fn boundary_max_passes() {
    let cfg: BoundaryValidation = parse_str("bnd val=10").unwrap();
    assert_eq!(cfg.val, 10);
}

#[test]
fn boundary_below_min_fails() {
    let err = parse_str::<BoundaryValidation>("bnd val=4").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("less than minimum"), "got: {msg}");
}

#[test]
fn boundary_above_max_fails() {
    let err = parse_str::<BoundaryValidation>("bnd val=11").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("exceeds maximum"), "got: {msg}");
}

// ==========================================================================
// Improved error message format
// ==========================================================================

#[test]
fn error_message_uses_rule_display_not_debug() {
    let err = parse_str::<BoundaryValidation>("bnd val=4").unwrap_err();
    let msg = err.to_string();
    // Should contain the Display form "min(5)", not the Debug form "Min(5.0)"
    assert!(msg.contains("min(5)"), "expected rule display form, got: {msg}");
    assert!(!msg.contains("Min(5.0)"), "should not contain Debug form, got: {msg}");
}

// ==========================================================================
// multiple_of(0) produces a runtime error
// ==========================================================================

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "mz")]
struct MultipleOfZeroValidation {
    #[kdl(validate(multiple_of = 0))]
    val: i32,
}

#[test]
fn multiple_of_zero_is_runtime_error() {
    let err = parse_str::<MultipleOfZeroValidation>("mz val=10").unwrap_err();
    let msg = err.to_string();
    assert!(
        msg.contains("divisor must not be zero"),
        "expected divisor-zero error, got: {msg}"
    );
}

// ==========================================================================
// Multiple error display format
// ==========================================================================

#[test]
fn multiple_error_display_format() {
    let err = parse_str::<MultiValidation>("multi a=0 b=0").unwrap_err();
    let msg = err.to_string();
    // Should show the count
    assert!(msg.contains("2 validation errors:"), "expected count prefix, got: {msg}");
    // Should show numbered sub-errors
    assert!(msg.contains("[1]"), "expected [1] marker, got: {msg}");
    assert!(msg.contains("[2]"), "expected [2] marker, got: {msg}");
    // Sub-errors should be on separate lines
    let lines: Vec<&str> = msg.lines().collect();
    assert!(lines.len() >= 3, "expected at least 3 lines, got {} lines: {msg}", lines.len());
}

// ==========================================================================
// Combined field-level + cross-field errors collected together
// ==========================================================================

#[derive(Debug, PartialEq, kdl_config::KdlNode)]
#[kdl(node = "combo")]
struct CombinedValidation {
    #[kdl(validate(min = 10, less_than = "b"))]
    a: f64,
    #[kdl(validate(max = 50))]
    b: f64,
}

#[test]
fn field_and_cross_field_errors_combined() {
    // a=5 fails min=10 (field-level), a=5 < b=100 passes less_than, b=100 fails max=50
    // So we expect 2 errors: a's min failure + b's max failure
    let err = parse_str::<CombinedValidation>("combo a=5 b=100").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("validation errors"), "expected multiple errors, got: {msg}");
    assert!(msg.contains("'a'"), "expected field 'a' in error, got: {msg}");
    assert!(msg.contains("'b'"), "expected field 'b' in error, got: {msg}");
}

#[test]
fn field_level_and_cross_field_errors_combined() {
    // a=5 fails min=10 (field-level), a=5 < b=3 ALSO fails less_than (cross-field), b=3 passes max=50
    // So we expect 2 errors: a's min failure + a's less_than failure
    let err = parse_str::<CombinedValidation>("combo a=5 b=3").unwrap_err();
    let msg = err.to_string();
    assert!(msg.contains("validation errors"), "expected multiple errors, got: {msg}");
    assert!(msg.contains("less than minimum"), "expected min error, got: {msg}");
    assert!(msg.contains("must be less than"), "expected cross-field error, got: {msg}");
}

// ==========================================================================
// Per-field location tracking in validation errors
// ==========================================================================

#[test]
fn per_field_validation_errors_have_individual_locations() {
    // Use multi-line input so each field has a distinct column position.
    let input = r#"multi a=0 b=0"#;
    let err = parse_str::<MultiValidation>(input).unwrap_err();
    let msg = err.to_string();

    // Each sub-error should include "at line" with a position
    // The outer wrapper and both inner errors should all have location info
    assert!(msg.contains("at line"), "expected location info in error, got: {msg}");

    // Check that individual sub-errors have their own location
    if let kdl_config::ErrorKind::Multiple(ref inner) = err.kind {
        assert_eq!(inner.len(), 2);
        // Both inner errors should have location set (from per-field offset stamping)
        for (i, sub_err) in inner.iter().enumerate() {
            assert!(
                sub_err.location.is_some(),
                "sub-error [{}] should have location, got: {:?}", i + 1, sub_err
            );
        }
        // The two errors are for fields 'a' and 'b' which are at different column offsets
        let loc_a = inner[0].location.unwrap();
        let loc_b = inner[1].location.unwrap();
        // 'a' appears before 'b' in the input, so it should have a smaller column
        assert!(
            loc_a.column < loc_b.column,
            "field 'a' (col {}) should have smaller column than field 'b' (col {})",
            loc_a.column, loc_b.column
        );
    } else {
        panic!("expected Multiple error kind, got: {:?}", err.kind);
    }
}
