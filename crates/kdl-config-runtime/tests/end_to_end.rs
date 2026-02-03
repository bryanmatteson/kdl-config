use std::collections::HashMap;

use kdl_config_derive::{Kdl, KdlNode, KdlValue};
use kdl_config_runtime::{
    BoolMode, DefaultPlacement, KdlParse, ParseConfig, parse_config, parse_str,
};

fn parse_named<T: KdlParse>(
    kdl: &str,
    name: &str,
) -> Result<T, kdl_config_runtime::KdlConfigError> {
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
    assert_eq!(
        attr,
        ScalarConfig {
            name: "alpha".into(),
            count: 3
        }
    );

    let value =
        parse_named::<ScalarConfig>("config {\n  name \"beta\"\n  count 4\n}", "config").unwrap();
    assert_eq!(
        value,
        ScalarConfig {
            name: "beta".into(),
            count: 4
        }
    );
}

#[test]
fn parse_str_uses_single_top_level_node() {
    let parsed: ScalarConfig = parse_str("config name=\"alpha\" count=3").unwrap();
    assert_eq!(
        parsed,
        ScalarConfig {
            name: "alpha".into(),
            count: 3
        }
    );

    assert!(
        parse_str::<ScalarConfig>("config name=\"a\" count=1\nconfig name=\"b\" count=2").is_err()
    );
    assert!(parse_str::<ScalarConfig>("").is_err());
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct OutOfRangeI64 {
    #[kdl(attr)]
    value: i64,
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct LargeU64 {
    #[kdl(attr)]
    value: u64,
}

#[test]
fn rejects_out_of_range_integers() {
    assert!(parse_named::<OutOfRangeI64>("config value=9223372036854775808", "config").is_err());
    let parsed = parse_named::<LargeU64>("config value=18446744073709551615", "config").unwrap();
    assert_eq!(parsed.value, u64::MAX);
}

#[derive(Debug, PartialEq, Kdl)]
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

#[derive(Debug, PartialEq, Kdl)]
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

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config", default_bool = "value-only")]
struct DefaultValueOnlyNoPlacement {
    enabled: bool,
}

#[test]
fn default_value_only_rejects_flags_without_explicit_placement() {
    assert!(parse_named::<DefaultValueOnlyNoPlacement>("config enabled", "config").is_err());
    let parsed =
        parse_named::<DefaultValueOnlyNoPlacement>("config enabled=#true", "config").unwrap();
    assert!(parsed.enabled);
}

#[derive(Debug, PartialEq, Kdl)]
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
    assert!(parse_named::<BoolPresenceOnly>("config no-enabled", "config").is_err());
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct BoolDefaultsMissing {
    #[kdl(attr)]
    enabled: bool,
    #[kdl(attr)]
    optional: Option<bool>,
}

#[test]
fn missing_bools_default_false_and_none() {
    let parsed = parse_named::<BoolDefaultsMissing>("config", "config").unwrap();
    assert!(!parsed.enabled);
    assert!(parsed.optional.is_none());
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct BoolRequiredMissing {
    #[kdl(attr, required)]
    enabled: bool,
}

#[test]
fn required_bools_still_error_when_missing() {
    assert!(parse_named::<BoolRequiredMissing>("config", "config").is_err());
}

#[derive(Debug, PartialEq, Kdl)]
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

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct DefaultNegative {
    #[kdl(attr, default = -1)]
    value: i64,
}

#[test]
fn parses_negative_default_literals() {
    let parsed = parse_named::<DefaultNegative>("config", "config").unwrap();
    assert_eq!(parsed.value, -1);
}

#[derive(Debug, PartialEq, Kdl)]
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

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct ConflictError {
    #[kdl(attr, value)]
    value: i64,
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct ConflictFirst {
    #[kdl(attr, value, conflict = "first")]
    value: i64,
}

#[derive(Debug, PartialEq, Kdl)]
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

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct DuplicateAttrLast {
    #[kdl(attr, conflict = "last")]
    value: i64,
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct DuplicateAttrError {
    #[kdl(attr)]
    value: i64,
}

#[test]
fn duplicate_attributes_use_conflict_policy() {
    let parsed = parse_named::<DuplicateAttrLast>("config value=1 value=2", "config").unwrap();
    assert_eq!(parsed.value, 2);

    assert!(parse_named::<DuplicateAttrError>("config value=1 value=2", "config").is_err());
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct VecAppendConfig {
    #[kdl(attr, value, conflict = "append")]
    values: Vec<i64>,
}

#[test]
fn vec_append_conflict_concatenates() {
    let parsed =
        parse_named::<VecAppendConfig>("config values=1 {\n  values 2\n  values 3\n}", "config")
            .unwrap();
    assert_eq!(parsed.values, vec![1, 2, 3]);
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config_node")]
struct RegistryEntry {
    #[kdl(attr)]
    size: i64,
}

#[derive(Debug, PartialEq, Kdl)]
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

#[derive(Debug, PartialEq, Kdl)]
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

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct RegistryVecConfig {
    #[kdl(registry, container = "config_node")]
    nodes: Vec<(String, RegistryEntry)>,
}

#[test]
fn parses_registry_vec_in_document_order() {
    let parsed = parse_named::<RegistryVecConfig>(
        "config {\n  config_node first size=1\n  config_node second size=2\n  config_node third size=3\n}",
        "config",
    )
    .unwrap();

    assert_eq!(parsed.nodes[0].0, "first");
    assert_eq!(parsed.nodes[1].0, "second");
    assert_eq!(parsed.nodes[2].0, "third");
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct RegistryVecLast {
    #[kdl(registry, container = "config_node", conflict = "last")]
    nodes: Vec<(String, RegistryEntry)>,
}

#[test]
fn registry_vec_last_moves_to_last_occurrence() {
    let parsed = parse_named::<RegistryVecLast>(
        "config {\n  config_node dup size=1\n  config_node keep size=2\n  config_node dup size=9\n}",
        "config",
    )
    .unwrap();

    assert_eq!(parsed.nodes[0].0, "keep");
    assert_eq!(parsed.nodes[1].0, "dup");
    assert_eq!(parsed.nodes[1].1.size, 9);
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct RegistryVecAppend {
    #[kdl(registry, container = "config_node", conflict = "append")]
    nodes: Vec<(String, RegistryEntry)>,
}

#[test]
fn registry_vec_append_keeps_duplicates() {
    let parsed = parse_named::<RegistryVecAppend>(
        "config {\n  config_node dup size=1\n  config_node dup size=2\n}",
        "config",
    )
    .unwrap();

    assert_eq!(parsed.nodes.len(), 2);
    assert_eq!(parsed.nodes[0].1.size, 1);
    assert_eq!(parsed.nodes[1].1.size, 2);
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config", deny_unknown)]
struct DenyUnknownConfig {
    #[kdl(attr)]
    name: String,
}

#[test]
fn deny_unknown_rejects_extra_keys() {
    assert!(parse_named::<DenyUnknownConfig>("config name=\"ok\" extra=1", "config").is_err());
    assert!(
        parse_named::<DenyUnknownConfig>("config {\n  name \"ok\"\n  extra {}\n}", "config")
            .is_err()
    );
}

#[test]
fn deny_unknown_rejects_extra_args() {
    assert!(parse_named::<DenyUnknownConfig>("config name=\"ok\" extra", "config").is_err());
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config", deny_unknown)]
struct DenyUnknownDefaultPlacement {
    name: String,
}

#[test]
fn deny_unknown_allows_default_placement_fields() {
    let parsed =
        parse_named::<DenyUnknownDefaultPlacement>("config name=\"ok\"", "config").unwrap();
    assert_eq!(parsed.name, "ok");
}

#[derive(Debug, PartialEq, Kdl)]
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

    assert!(
        parse_named_with_config::<PlacementOverrideConfig>("config { value 1 }", "config", &config)
            .is_err()
    );
    let parsed =
        parse_named_with_config::<PlacementOverrideConfig>("config value=1", "config", &config)
            .unwrap();
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

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct ModifierConfig {
    #[kdl(modifier)]
    modifier: kdl_config_runtime::Modifier,
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct OptionalModifierConfig {
    #[kdl(modifier)]
    modifier: Option<kdl_config_runtime::Modifier>,
}

#[test]
fn parses_node_modifiers_into_fields() {
    let parsed = parse_named::<ModifierConfig>("+config", "config").unwrap();
    assert_eq!(parsed.modifier, kdl_config_runtime::Modifier::Append);

    let parsed = parse_named::<OptionalModifierConfig>("config", "config").unwrap();
    assert_eq!(parsed.modifier, None);

    let parsed = parse_named::<OptionalModifierConfig>("!config", "config").unwrap();
    assert_eq!(parsed.modifier, Some(kdl_config_runtime::Modifier::Replace));
}

#[test]
fn renders_node_modifiers_from_fields() {
    let config = ModifierConfig {
        modifier: kdl_config_runtime::Modifier::Remove,
    };
    let rendered = kdl_config_runtime::to_kdl(&config, "config");
    assert_eq!(rendered.trim(), "-config");

    let config = OptionalModifierConfig { modifier: None };
    let rendered = kdl_config_runtime::to_kdl(&config, "config");
    assert_eq!(rendered.trim(), "config");
}

#[derive(Debug, PartialEq, Kdl)]
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

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct VecAttrRender {
    #[kdl(attr, render = "attr")]
    values: Vec<i64>,
}

#[test]
fn renders_repeated_attrs_for_vecs() {
    let config = VecAttrRender { values: vec![1, 2] };
    let rendered = kdl_config_runtime::to_kdl(&config, "config");
    let trimmed = rendered.trim();
    assert!(trimmed.starts_with("config "));
    assert_eq!(trimmed.matches("values=").count(), 2);
    assert!(trimmed.contains("values=1"));
    assert!(trimmed.contains("values=2"));
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct RenderBoolFalse {
    #[kdl(attr)]
    enabled: bool,
}

#[test]
fn render_bool_false_emits_negative_flag() {
    let config = RenderBoolFalse { enabled: false };
    let rendered = kdl_config_runtime::to_kdl(&config, "config");
    assert_eq!(rendered.trim(), "config no-enabled");
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct OptionalPresenceOnlyRender {
    #[kdl(attr, bool = "presence-only")]
    enabled: Option<bool>,
}

#[test]
fn presence_only_optional_renders_only_positive_flag() {
    let rendered = kdl_config_runtime::to_kdl(
        &OptionalPresenceOnlyRender {
            enabled: Some(true),
        },
        "config",
    );
    assert_eq!(rendered.trim(), "config enabled");

    let rendered = kdl_config_runtime::to_kdl(
        &OptionalPresenceOnlyRender {
            enabled: Some(false),
        },
        "config",
    );
    assert_eq!(rendered.trim(), "config");

    let rendered =
        kdl_config_runtime::to_kdl(&OptionalPresenceOnlyRender { enabled: None }, "config");
    assert_eq!(rendered.trim(), "config");
}

#[derive(Debug, PartialEq, Kdl)]
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

#[derive(Debug, PartialEq, Kdl)]
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

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct PresencePlusValueDefaults {
    #[kdl(attr, bool = "presence+value")]
    enabled: bool,
}

#[derive(Debug, Clone, PartialEq, KdlValue)]
enum Mode {
    Fast,
    Safe,
}

#[derive(Debug, Clone, PartialEq, KdlValue)]
struct Port(u32);

#[derive(Debug, Clone, PartialEq, KdlValue)]
struct Flag(bool);

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct ScalarAttrConfig {
    #[kdl(attr, scalar)]
    mode: Mode,
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct ScalarValueConfig {
    #[kdl(value, scalar)]
    mode: Mode,
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct ScalarDefaultConfig {
    #[kdl(attr, scalar, default = 8080)]
    port: Port,
    #[kdl(attr, scalar, default = true)]
    enabled: Flag,
}

#[test]
fn scalar_custom_type_parses_attr() {
    let parsed = parse_named::<ScalarAttrConfig>("config mode=\"Fast\"", "config").unwrap();
    assert_eq!(parsed.mode, Mode::Fast);
}

#[test]
fn scalar_custom_type_parses_value_node() {
    let parsed = parse_named::<ScalarValueConfig>("config { mode \"Safe\" }", "config").unwrap();
    assert_eq!(parsed.mode, Mode::Safe);
}

#[test]
fn scalar_custom_type_defaults_work() {
    let parsed = parse_named::<ScalarDefaultConfig>("config", "config").unwrap();
    assert_eq!(parsed.port, Port(8080));
    assert_eq!(parsed.enabled, Flag(true));
}

#[test]
fn presence_value_defaults_match_expected() {
    let parsed = parse_named::<PresencePlusValueDefaults>("config enabled", "config").unwrap();
    assert!(parsed.enabled);

    let parsed =
        parse_named::<PresencePlusValueDefaults>("config enabled=#true", "config").unwrap();
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
#[kdl(node = "config", deny_unknown)]
struct SkipUnknownConfig {
    #[kdl(skip)]
    ignored: i64,
}

#[test]
fn skip_fields_allow_input_under_deny_unknown() {
    let parsed = parse_named::<SkipUnknownConfig>("config ignored=5", "config").unwrap();
    assert_eq!(parsed.ignored, 0);
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

    assert!(
        parse_named_with_config::<DefaultBoolModeStruct>("config enabled", "config", &config)
            .is_err()
    );
    let parsed =
        parse_named_with_config::<DefaultBoolModeStruct>("config enabled=#true", "config", &config)
            .unwrap();
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
struct RegistryKeyAttrConfig {
    #[kdl(registry, container = "item", key_attr = "id")]
    items: HashMap<String, RegistryItem>,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "item", deny_unknown)]
struct RegistryItem {
    #[kdl(attr)]
    value: i64,
}

#[test]
fn registry_key_attr_is_removed_before_parsing() {
    let parsed =
        parse_named::<RegistryKeyAttrConfig>("config {\n  item id=\"alpha\" value=1\n}", "config")
            .unwrap();
    assert_eq!(parsed.items.get("alpha").unwrap().value, 1);
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct RegistryKeyArgConfig {
    #[kdl(registry, container = "item", key_arg = 1)]
    items: HashMap<String, RegistryItemPositional>,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "item")]
struct RegistryItemPositional {
    #[kdl(attr, positional = 0)]
    value: i64,
}

#[test]
fn registry_key_arg_uses_custom_index() {
    let parsed =
        parse_named::<RegistryKeyArgConfig>("config {\n  item 10 \"beta\"\n}", "config").unwrap();
    assert_eq!(parsed.items.get("beta").unwrap().value, 10);
}

fn registry_key_from_attr(
    node: &kdl_config_runtime::Node,
) -> Result<String, kdl_config_runtime::KdlConfigError> {
    node.attr("key")
        .and_then(|value| value.as_str())
        .map(|val| val.to_string())
        .ok_or_else(|| {
            kdl_config_runtime::KdlConfigError::custom("registry key", "missing key attribute")
        })
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "item")]
struct RegistryItemKeyed {
    #[kdl(attr)]
    key: String,
    #[kdl(attr)]
    value: i64,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct RegistryKeyFnConfig {
    #[kdl(registry, container = "item", key_fn = "registry_key_from_attr")]
    items: HashMap<String, RegistryItemKeyed>,
}

#[test]
fn registry_key_fn_is_used() {
    let parsed =
        parse_named::<RegistryKeyFnConfig>("config {\n  item key=\"gamma\" value=3\n}", "config")
            .unwrap();
    assert_eq!(parsed.items.get("gamma").unwrap().value, 3);
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
    let parsed =
        parse_named::<ExplicitKeyedDisablesFlags>("config enabled=#true", "config").unwrap();
    assert!(parsed.enabled);
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "test", rename_all = "kebab-case")]
enum MixedEnum {
    Value,
    WithStructType(NewStruct),
    WithStruct {
        #[kdl(value, render = "value")]
        key: String,
    },
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "pair")]
struct Pair(i64, String);

#[test]
fn tuple_structs_parse_positional_args() {
    let parsed = parse_named::<Pair>("pair 1 \"two\"", "pair").unwrap();
    assert_eq!(parsed, Pair(1, "two".to_string()));
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "pair")]
struct PairDefaults(#[kdl(default = 2)] i64, #[kdl(optional)] Option<String>);

#[test]
fn tuple_struct_defaults_and_optional_work() {
    let parsed = parse_named::<PairDefaults>("pair", "pair").unwrap();
    assert_eq!(parsed, PairDefaults(2, None));
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "empty")]
struct Empty;

#[test]
fn unit_structs_require_no_payload() {
    let parsed = parse_named::<Empty>("empty", "empty").unwrap();
    assert_eq!(parsed, Empty);
    assert!(parse_named::<Empty>("empty value=1", "empty").is_err());
}

#[test]
fn raw_parse_preserves_identifier_quoting() {
    let root = parse_config("\"+config\" \"+attr\"=1").unwrap();
    let node = root.children().first().unwrap();
    assert_eq!(node.name, "+config");
    assert_eq!(node.name_repr(), Some("\"+config\""));
    assert_eq!(node.attr_repr("+attr"), Some("\"+attr\""));

    let rendered = kdl_config_runtime::to_kdl(node, &node.name);
    assert_eq!(rendered.trim(), "\"+config\" \"+attr\"=1");
}

#[test]
fn raw_parse_preserves_raw_string_quoting() {
    let root = parse_config(r##"#"+config"# #"+attr"#=1"##).unwrap();
    let node = root.children().first().unwrap();
    assert_eq!(node.name, "+config");
    assert_eq!(node.name_repr(), Some("#\"+config\"#"));
    assert_eq!(node.attr_repr("+attr"), Some("#\"+attr\"#"));

    let rendered = kdl_config_runtime::to_kdl(node, &node.name);
    assert_eq!(rendered.trim(), "#\"+config\"# #\"+attr\"#=1");
}

#[derive(Debug, PartialEq, KdlNode)]
struct NewStruct {
    #[kdl(attr)]
    key: Option<String>,
}

#[test]
fn parses_enum_variants_with_struct_payloads() {
    let kdl = "test value\n\
test with-struct-type {}\n\
test with-struct { key \"\" }\n";
    let root = parse_config(kdl).unwrap();
    let parsed = root
        .children_named("test")
        .map(|node| MixedEnum::from_node(node, &ParseConfig::default()).unwrap())
        .collect::<Vec<_>>();

    assert_eq!(
        parsed,
        vec![
            MixedEnum::Value,
            MixedEnum::WithStructType(NewStruct { key: None }),
            MixedEnum::WithStruct { key: "".into() },
        ]
    );
}

#[test]
fn renders_enum_variants_inline() {
    let rendered = kdl_config_runtime::to_kdl(&MixedEnum::Value, "test");
    assert_eq!(rendered.trim(), "test value");

    let rendered = kdl_config_runtime::to_kdl(&MixedEnum::WithStruct { key: "ok".into() }, "test");
    assert_eq!(rendered.trim(), "test with-struct {\n    key \"ok\"\n}");
}

#[test]
fn invalid_enum_variant_is_error() {
    assert!(parse_named::<MixedEnum>("test unknown", "test").is_err());
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "choice", rename_all = "kebab-case")]
enum TaggedEnum {
    #[kdl(tag = 1)]
    Pair(i64, String),
    #[kdl(tag = true)]
    Enabled,
}

#[test]
fn parses_enum_tuple_variants_and_non_string_tags() {
    let parsed = parse_named::<TaggedEnum>("choice 1 10 \"ok\"", "choice").unwrap();
    assert_eq!(parsed, TaggedEnum::Pair(10, "ok".into()));

    let parsed = parse_named::<TaggedEnum>("choice #true", "choice").unwrap();
    assert_eq!(parsed, TaggedEnum::Enabled);
}

#[test]
fn renders_enum_tuple_variants_and_non_string_tags() {
    let rendered = kdl_config_runtime::to_kdl(&TaggedEnum::Pair(10, "ok".into()), "choice");
    assert_eq!(rendered.trim(), "choice 1 10 \"ok\"");

    let rendered = kdl_config_runtime::to_kdl(&TaggedEnum::Enabled, "choice");
    assert_eq!(rendered.trim(), "choice #true");
}
