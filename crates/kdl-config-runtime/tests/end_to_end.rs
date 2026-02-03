use std::collections::HashMap;

use kdl_config_derive::KdlNode;
use kdl_config_runtime::{parse_config, BoolMode, DefaultPlacement, KdlParse, ParseConfig};

fn parse_named<T: KdlParse>(kdl: &str, name: &str) -> Result<T, kdl_config_runtime::KdlConfigError> {
    let root = parse_config(kdl)?;
    let node = root.child(name).expect("missing node");
    T::from_node(node, &ParseConfig::default())
}

fn parse_named_with_config<T: KdlParse>(
    kdl: &str,
    name: &str,
    config: &ParseConfig,
) -> Result<T, kdl_config_runtime::KdlConfigError> {
    let root = parse_config(kdl)?;
    let node = root.child(name).expect("missing node");
    T::from_node(node, config)
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct ScalarConfig {
    name: String,
    count: i64,
}

#[test]
fn parses_exhaustive_attr_and_value_nodes() {
    let attr = parse_named::<ScalarConfig>("config name=\"alpha\" count=3", "config").unwrap();
    assert_eq!(attr, ScalarConfig { name: "alpha".into(), count: 3 });

    let value = parse_named::<ScalarConfig>("config {\n  name \"beta\"\n  count 4\n}", "config").unwrap();
    assert_eq!(value, ScalarConfig { name: "beta".into(), count: 4 });
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct BoolConfig {
    #[kdl(attr)]
    enabled: bool,
}

#[test]
fn parses_bool_presence_and_value() {
    let enabled = parse_named::<BoolConfig>("config enabled", "config").unwrap();
    assert!(enabled.enabled);

    let disabled = parse_named::<BoolConfig>("config no-enabled", "config").unwrap();
    assert!(!disabled.enabled);

    let explicit = parse_named::<BoolConfig>("config enabled=#false", "config").unwrap();
    assert!(!explicit.enabled);
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct BoolValueOnly {
    #[kdl(attr, bool = "value-only")]
    enabled: bool,
}

#[test]
fn value_only_bools_reject_flags() {
    assert!(parse_named::<BoolValueOnly>("config enabled", "config").is_err());
    let parsed = parse_named::<BoolValueOnly>("config enabled=#true", "config").unwrap();
    assert!(parsed.enabled);
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct BoolPresenceOnly {
    #[kdl(attr, bool = "presence-only")]
    enabled: bool,
}

#[test]
fn presence_only_bools_reject_explicit_values() {
    let parsed = parse_named::<BoolPresenceOnly>("config enabled", "config").unwrap();
    assert!(parsed.enabled);

    assert!(parse_named::<BoolPresenceOnly>("config enabled=#true", "config").is_err());
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct BoolCustomFlags {
    #[kdl(attr, flag = "on", neg_flag = "off")]
    enabled: bool,
}

#[test]
fn parses_custom_flag_names() {
    let on = parse_named::<BoolCustomFlags>("config on", "config").unwrap();
    assert!(on.enabled);

    let off = parse_named::<BoolCustomFlags>("config off", "config").unwrap();
    assert!(!off.enabled);
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct BoolWithWithoutOnly {
    #[kdl(attr, flag_style = "with|without")]
    enabled: bool,
}

#[test]
fn flag_style_with_without_only() {
    let parsed = parse_named::<BoolWithWithoutOnly>("config with-enabled", "config").unwrap();
    assert!(parsed.enabled);

    assert!(parse_named::<BoolWithWithoutOnly>("config enabled", "config").is_err());
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct ConflictError {
    #[kdl(attr, value)]
    value: i64,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct ConflictFirst {
    #[kdl(attr, value, conflict = "first")]
    value: i64,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct ConflictLast {
    #[kdl(attr, value, conflict = "last")]
    value: i64,
}

#[test]
fn conflict_policies_apply_in_deterministic_order() {
    let kdl = "config value=1 { value 2 }";
    let root = parse_config(kdl).unwrap();
    let node = root.child("config").unwrap();

    assert!(ConflictError::from_node(node, &ParseConfig::default()).is_err());

    let first = ConflictFirst::from_node(node, &ParseConfig::default()).unwrap();
    assert_eq!(first.value, 1);

    let last = ConflictLast::from_node(node, &ParseConfig::default()).unwrap();
    assert_eq!(last.value, 2);
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct VecAppendConfig {
    #[kdl(attr, value, conflict = "append")]
    values: Vec<i64>,
}

#[test]
fn vec_append_conflict_concatenates() {
    let parsed = parse_named::<VecAppendConfig>("config values=1 {\n  values 2\n  values 3\n}", "config").unwrap();
    assert_eq!(parsed.values, vec![1, 2, 3]);
}

#[derive(Debug, PartialEq, KdlNode)]
struct RegistryEntry {
    #[kdl(attr)]
    size: i64,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct RegistryConfig {
    #[kdl(registry, container = "config_node")]
    nodes: HashMap<String, RegistryEntry>,
}

#[test]
fn parses_registry_keyed_by_first_arg() {
    let parsed = parse_named::<RegistryConfig>(
        "config {\n  config_node first size=1\n  config_node second size=2\n  config_node third key=\"value\" size=3\n}",
        "config",
    )
    .unwrap();

    assert_eq!(parsed.nodes.get("first").unwrap().size, 1);
    assert_eq!(parsed.nodes.get("second").unwrap().size, 2);
    assert_eq!(parsed.nodes.get("third").unwrap().size, 3);
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct RegistryLast {
    #[kdl(registry, container = "config_node", conflict = "last")]
    nodes: HashMap<String, RegistryEntry>,
}

#[test]
fn registry_conflict_last_wins() {
    let parsed = parse_named::<RegistryLast>(
        "config {\n  config_node dup size=1\n  config_node dup size=9\n}",
        "config",
    )
    .unwrap();

    assert_eq!(parsed.nodes.get("dup").unwrap().size, 9);
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config", deny_unknown)]
struct DenyUnknownConfig {
    #[kdl(attr)]
    name: String,
}

#[test]
fn deny_unknown_rejects_extra_keys() {
    assert!(parse_named::<DenyUnknownConfig>("config name=\"ok\" extra=1", "config").is_err());
    assert!(parse_named::<DenyUnknownConfig>("config {\n  name \"ok\"\n  extra {}\n}", "config").is_err());
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct PlacementOverrideConfig {
    value: i64,
}

#[test]
fn parse_config_overrides_default_placement() {
    let config = ParseConfig {
        default_placement: DefaultPlacement::Attr,
        ..ParseConfig::default()
    };

    assert!(parse_named_with_config::<PlacementOverrideConfig>("config { value 1 }", "config", &config).is_err());
    let parsed = parse_named_with_config::<PlacementOverrideConfig>("config value=1", "config", &config).unwrap();
    assert_eq!(parsed.value, 1);
}

#[test]
fn parses_signal_modifiers() {
    let root = parse_config("config {\n  +child {}\n  -child {}\n  !child {}\n}").unwrap();
    let node = root.child("config").unwrap();
    let modifiers = node
        .children()
        .iter()
        .filter(|child| child.name == "child")
        .map(|child| child.modifier)
        .collect::<Vec<_>>();

    assert!(modifiers.contains(&kdl_config_runtime::Modifier::Append));
    assert!(modifiers.contains(&kdl_config_runtime::Modifier::Remove));
    assert!(modifiers.contains(&kdl_config_runtime::Modifier::Replace));
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct RenderConfig {
    #[kdl(attr)]
    b: i64,
    #[kdl(attr)]
    a: i64,
}

#[test]
fn render_is_deterministic() {
    let config = RenderConfig { a: 1, b: 2 };
    let rendered = kdl_config_runtime::to_kdl(&config, "config");
    assert_eq!(rendered.trim(), "config a=1 b=2");
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct PresenceOnlyChild {
    #[kdl(value, bool = "presence-only")]
    enabled: bool,
}

#[test]
fn presence_only_child_nodes_allow_empty_values() {
    let parsed = parse_named::<PresenceOnlyChild>("config { enabled }", "config").unwrap();
    assert!(parsed.enabled);
    assert!(parse_named::<PresenceOnlyChild>("config { enabled #true }", "config").is_err());
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct FlagAndPositional {
    #[kdl(attr, positional = 0)]
    enabled: bool,
}

#[test]
fn positional_and_flag_candidates_conflict() {
    let result = parse_named::<FlagAndPositional>("config enabled", "config");
    assert!(result.is_err());
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct PresencePlusValueDefaults {
    #[kdl(attr, bool = "presence+value")]
    enabled: bool,
}

#[test]
fn presence_value_defaults_match_expected() {
    let parsed = parse_named::<PresencePlusValueDefaults>("config enabled", "config").unwrap();
    assert!(parsed.enabled);

    let parsed = parse_named::<PresencePlusValueDefaults>("config enabled=#true", "config").unwrap();
    assert!(parsed.enabled);

    let parsed = parse_named::<PresencePlusValueDefaults>("config no-enabled", "config").unwrap();
    assert!(!parsed.enabled);
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct AttrFlagsOnly {
    #[kdl(attr, flag)]
    enabled: bool,
}

#[test]
fn attr_flag_only_disallows_keyed_values() {
    assert!(parse_named::<AttrFlagsOnly>("config enabled=#true", "config").is_err());
    let parsed = parse_named::<AttrFlagsOnly>("config enabled", "config").unwrap();
    assert!(parsed.enabled);
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct DefaultBoolModeStruct {
    #[kdl(attr)]
    enabled: bool,
}

#[test]
fn parse_config_default_bool_mode_overrides() {
    let config = ParseConfig {
        default_bool: BoolMode::ValueOnly,
        ..ParseConfig::default()
    };

    assert!(parse_named_with_config::<DefaultBoolModeStruct>("config enabled", "config", &config).is_err());
    let parsed = parse_named_with_config::<DefaultBoolModeStruct>("config enabled=#true", "config", &config).unwrap();
    assert!(parsed.enabled);
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct RegistryDuplicateError {
    #[kdl(registry, container = "config_node")]
    nodes: HashMap<String, RegistryEntry>,
}

#[test]
fn registry_duplicate_error_is_reported() {
    let result = parse_named::<RegistryDuplicateError>(
        "config {\n  config_node dup size=1\n  config_node dup size=2\n}",
        "config",
    );
    assert!(result.is_err());
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct ExplicitKeyedDisablesFlags {
    #[kdl(attr, keyed)]
    enabled: bool,
}

#[test]
fn explicit_keyed_disables_flags() {
    assert!(parse_named::<ExplicitKeyedDisablesFlags>("config enabled", "config").is_err());
    let parsed = parse_named::<ExplicitKeyedDisablesFlags>("config enabled=#true", "config").unwrap();
    assert!(parsed.enabled);
}
