use std::collections::HashMap;

use kdl_config::{
    BoolMode, DefaultPlacement, KdlConfigError, KdlDecode, ParseConfig, parse_config, parse_str,
    parse_str_with_config,
};
use kdl_config::newtypes::ScalarString;
use kdl_config::{Kdl, KdlNode, KdlValue};

fn parse_named<T: KdlDecode>(kdl: &str, _name: &str) -> Result<T, KdlConfigError> {
    parse_str(kdl)
}

fn parse_named_with_config<T: KdlDecode>(
    kdl: &str,
    _name: &str,
    config: &ParseConfig,
) -> Result<T, KdlConfigError> {
    parse_str_with_config(kdl, config)
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
    assert!(parse_str::<ConflictError>(kdl).is_err());

    let first = parse_str::<ConflictFirst>(kdl).unwrap();
    assert_eq!(first.value, 1);

    let last = parse_str::<ConflictLast>(kdl).unwrap();
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

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "indexing")]
struct IndexingConfig {
    #[kdl(attr)]
    chunk_size: i64,
}

#[derive(Debug, PartialEq, KdlNode)]
struct CategoryOverrides {
    #[kdl(child)]
    indexing: IndexingConfig,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct ChildrenMapByArgConfig {
    #[kdl(children_map, map_node = "category")]
    categories: HashMap<String, CategoryOverrides>,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct ChildrenMapByNameConfig {
    #[kdl(children_map)]
    categories: HashMap<String, CategoryOverrides>,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct ChildrenMapKeyAttrConfig {
    #[kdl(children_map, map_node = "category", select(attr("name")))]
    categories: HashMap<String, CategoryOverrides>,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct ChildrenMapKeyFnConfig {
    #[kdl(
        children_map,
        map_node = "category",
        select(func("registry_key_from_attr"))
    )]
    categories: HashMap<String, CategoryOverrides>,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct ChildrenMapVecConfig {
    #[kdl(children_map, map_node = "category", conflict = "append")]
    categories: Vec<(String, CategoryOverrides)>,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct ChildrenMapScalarValueConfig {
    #[kdl(children_map, map_node = "category")]
    categories: HashMap<String, ScalarString>,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "category", deny_unknown)]
struct CategoryOverridesStrict {
    #[kdl(child)]
    indexing: IndexingConfig,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct ChildrenMapKeyAttrConsumeConfig {
    #[kdl(children_map, map_node = "category", select(attr("name"), consume))]
    categories: HashMap<String, CategoryOverridesStrict>,
}

#[test]
fn parses_children_map_keyed_by_first_arg() {
    let parsed = parse_named::<ChildrenMapByArgConfig>(
        "config {\n  category \"docs\" {\n    indexing chunk_size=3000\n  }\n  category \"code\" {\n    indexing chunk_size=2000\n  }\n}",
        "config",
    )
    .unwrap();

    assert_eq!(
        parsed.categories.get("docs").unwrap().indexing.chunk_size,
        3000
    );
    assert_eq!(
        parsed.categories.get("code").unwrap().indexing.chunk_size,
        2000
    );
}

#[test]
fn parses_children_map_keyed_by_child_name() {
    let parsed = parse_named::<ChildrenMapByNameConfig>(
        "config {\n  docs {\n    indexing chunk_size=3000\n  }\n  code {\n    indexing chunk_size=2000\n  }\n}",
        "config",
    )
    .unwrap();

    assert_eq!(
        parsed.categories.get("docs").unwrap().indexing.chunk_size,
        3000
    );
    assert_eq!(
        parsed.categories.get("code").unwrap().indexing.chunk_size,
        2000
    );
}

#[test]
fn select_consume_allows_deny_unknown_children() {
    let parsed = parse_named::<ChildrenMapKeyAttrConsumeConfig>(
        "config {\n  category name=\"docs\" {\n    indexing chunk_size=3000\n  }\n}",
        "config",
    )
    .unwrap();

    assert_eq!(
        parsed.categories.get("docs").unwrap().indexing.chunk_size,
        3000
    );
}

#[test]
fn parses_children_map_keyed_by_attribute() {
    let parsed = parse_named::<ChildrenMapKeyAttrConfig>(
        "config {\n  category name=\"docs\" {\n    indexing chunk_size=3000\n  }\n  category name=\"code\" {\n    indexing chunk_size=2000\n  }\n}",
        "config",
    )
    .unwrap();

    assert_eq!(
        parsed.categories.get("docs").unwrap().indexing.chunk_size,
        3000
    );
    assert_eq!(
        parsed.categories.get("code").unwrap().indexing.chunk_size,
        2000
    );
}

#[test]
fn parses_children_map_keyed_by_fn() {
    let parsed = parse_named::<ChildrenMapKeyFnConfig>(
        "config {\n  category key=\"docs\" {\n    indexing chunk_size=3000\n  }\n  category key=\"code\" {\n    indexing chunk_size=2000\n  }\n}",
        "config",
    )
    .unwrap();

    assert_eq!(
        parsed.categories.get("docs").unwrap().indexing.chunk_size,
        3000
    );
    assert_eq!(
        parsed.categories.get("code").unwrap().indexing.chunk_size,
        2000
    );
}

#[test]
fn parses_children_map_vec_append() {
    let parsed = parse_named::<ChildrenMapVecConfig>(
        "config {\n  category \"docs\" {\n    indexing chunk_size=3000\n  }\n  category \"docs\" {\n    indexing chunk_size=3500\n  }\n}",
        "config",
    )
    .unwrap();

    assert_eq!(parsed.categories.len(), 2);
    assert_eq!(parsed.categories[0].0, "docs".to_string());
    assert_eq!(parsed.categories[0].1.indexing.chunk_size, 3000);
    assert_eq!(parsed.categories[1].0, "docs".to_string());
    assert_eq!(parsed.categories[1].1.indexing.chunk_size, 3500);
}

#[test]
fn parses_children_map_scalar_values() {
    let parsed = parse_named::<ChildrenMapScalarValueConfig>(
        "config {\n  category \"docs\" \"doc-value\"\n  category \"code\" \"code-value\"\n}",
        "config",
    )
    .unwrap();

    assert_eq!(parsed.categories.get("docs").unwrap().value, "doc-value");
    assert_eq!(parsed.categories.get("code").unwrap().value, "code-value");
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

#[test]
fn deny_unknown_suggests_similar_attr() {
    // Provide the required `name` correctly, add a typo'd extra attr
    let err =
        parse_named::<DenyUnknownConfig>("config name=\"ok\" naem=\"oops\"", "config").unwrap_err();
    let msg = err.to_string();
    assert!(
        msg.contains("did you mean 'name'"),
        "expected suggestion in: {msg}"
    );
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config", deny_unknown)]
struct DenyUnknownWithChild {
    #[kdl(child)]
    server: DenyUnknownChildNode,
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "server")]
struct DenyUnknownChildNode {
    #[kdl(attr)]
    host: String,
}

#[test]
fn deny_unknown_suggests_similar_child() {
    let err = parse_named::<DenyUnknownWithChild>(
        "config {\n  server host=\"localhost\"\n  servr host=\"localhost\"\n}",
        "config",
    )
    .unwrap_err();
    let msg = err.to_string();
    assert!(
        msg.contains("did you mean 'server'"),
        "expected suggestion in: {msg}"
    );
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

    assert!(modifiers.contains(&kdl_config::Modifier::Append));
    assert!(modifiers.contains(&kdl_config::Modifier::Remove));
    assert!(modifiers.contains(&kdl_config::Modifier::Replace));
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct ModifierConfig {
    #[kdl(modifier)]
    modifier: kdl_config::Modifier,
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct OptionalModifierConfig {
    #[kdl(modifier)]
    modifier: Option<kdl_config::Modifier>,
}

#[test]
fn parses_node_modifiers_into_fields() {
    let parsed = parse_named::<ModifierConfig>("+config", "config").unwrap();
    assert_eq!(parsed.modifier, kdl_config::Modifier::Append);

    let parsed = parse_named::<OptionalModifierConfig>("config", "config").unwrap();
    assert_eq!(parsed.modifier, None);

    let parsed = parse_named::<OptionalModifierConfig>("!config", "config").unwrap();
    assert_eq!(parsed.modifier, Some(kdl_config::Modifier::Replace));
}

#[test]
fn renders_node_modifiers_from_fields() {
    let config = ModifierConfig {
        modifier: kdl_config::Modifier::Remove,
    };
    let rendered = kdl_config::to_kdl(&config, "config");
    assert_eq!(rendered.trim(), "-config");

    let config = OptionalModifierConfig { modifier: None };
    let rendered = kdl_config::to_kdl(&config, "config");
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
    let rendered = kdl_config::to_kdl(&config, "config");
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
    let rendered = kdl_config::to_kdl(&config, "config");
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
    let rendered = kdl_config::to_kdl(&config, "config");
    assert_eq!(rendered.trim(), "config no-enabled");
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct OptionalPresenceOnlyRender {
    #[kdl(attr, bool = "presence-only")]
    enabled: Option<bool>,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config", deny_unknown)]
struct PathConfig {
    #[kdl(path = "app.http")]
    port: u32,
}

#[test]
fn path_re_roots_relative() {
    let parsed = parse_named::<PathConfig>("config { app { http port=8080 } }", "config").unwrap();
    assert_eq!(parsed.port, 8080);
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct AbsolutePathRoot {
    #[kdl(child)]
    app: AppStub,
    #[kdl(child)]
    child: AbsolutePathChild,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "app")]
struct AppStub {
    #[kdl(child)]
    http: HttpStub,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "http")]
struct HttpStub {
    #[kdl(attr)]
    port: u32,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "child")]
struct AbsolutePathChild {
    #[kdl(path = "/app.http", attr)]
    port: u32,
}

#[test]
fn path_absolute_uses_root() {
    let parsed = parse_named::<AbsolutePathRoot>(
        "config {\n  app { http port=8080 }\n  child {}\n}",
        "config",
    )
    .unwrap();
    assert_eq!(parsed.child.port, 8080);
}

#[test]
fn presence_only_optional_renders_only_positive_flag() {
    let rendered = kdl_config::to_kdl(
        &OptionalPresenceOnlyRender {
            enabled: Some(true),
        },
        "config",
    );
    assert_eq!(rendered.trim(), "config enabled");

    let rendered = kdl_config::to_kdl(
        &OptionalPresenceOnlyRender {
            enabled: Some(false),
        },
        "config",
    );
    assert_eq!(rendered.trim(), "config");

    let rendered = kdl_config::to_kdl(&OptionalPresenceOnlyRender { enabled: None }, "config");
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
struct PositionalListConfig {
    #[kdl(attr, positional = "rest")]
    values: Vec<String>,
}

#[test]
fn positional_list_parses_all_args() {
    let parsed = parse_named::<PositionalListConfig>("config \"a\" \"b\" \"c\"", "config").unwrap();
    assert_eq!(parsed.values, vec!["a", "b", "c"]);
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config", schema)]
struct PositionalListRender {
    #[kdl(attr, positional = "rest")]
    #[kdl(render = "attr")]
    values: Vec<String>,
}

#[test]
fn positional_list_renders_as_args() {
    let rendered = kdl_config::to_kdl(
        &PositionalListRender {
            values: vec!["alpha".to_string(), "beta".to_string()],
        },
        "config",
    );
    assert_eq!(rendered.trim(), r#"config "alpha" "beta""#);
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
    #[kdl(registry, container = "item", select(attr("id"), consume))]
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
    #[kdl(registry, container = "item", select(arg(1), consume))]
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
    node: &kdl_config::KdlNode,
) -> Result<String, kdl_config::KdlConfigError> {
    use kdl_config::KdlNodeExt as _;
    node.attr("key")
        .and_then(|value| value.as_string())
        .map(|val| val.to_string())
        .ok_or_else(|| kdl_config::KdlConfigError::custom("registry key", "missing key attribute"))
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
    #[kdl(registry, container = "item", select(func("registry_key_from_attr")))]
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
struct RegistryInjectPreserveConfig {
    #[kdl(registry, container = "item", select(arg(0), inject = "selected"))]
    items: HashMap<String, RegistryInjectPreserveItem>,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "item")]
struct RegistryInjectPreserveItem {
    #[kdl(attr, positional = 0)]
    raw: String,
    #[kdl(attr)]
    selected: String,
}

#[test]
fn selector_inject_defaults_to_preserve_for_registry() {
    let parsed =
        parse_named::<RegistryInjectPreserveConfig>("config {\n  item \"alpha\"\n}", "config")
            .unwrap();

    let item = parsed.items.get("alpha").expect("registry item");
    assert_eq!(item.raw, "alpha");
    assert_eq!(item.selected, "alpha");
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

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node, rename_all = "kebab-case")]
enum MixedEnum {
    Value,
    WithStructType(NewStruct),
    WithStruct {
        #[kdl(value)]
        key: String,
    },
}

#[derive(Debug, PartialEq, Kdl)]
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

    let rendered = kdl_config::to_kdl(node, &node.name);
    assert_eq!(rendered.trim(), "\"+config\" \"+attr\"=1");
}

#[test]
fn raw_parse_preserves_raw_string_quoting() {
    let root = parse_config(r##"#"+config"# #"+attr"#=1"##).unwrap();
    let node = root.children().first().unwrap();
    assert_eq!(node.name, "+config");
    assert_eq!(node.name_repr(), Some("#\"+config\"#"));
    assert_eq!(node.attr_repr("+attr"), Some("#\"+attr\"#"));

    let rendered = kdl_config::to_kdl(node, &node.name);
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
        .map(|node| {
            let rendered = kdl_config::render_node(node);
            parse_str::<MixedEnum>(&rendered).unwrap()
        })
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
    let rendered = kdl_config::to_kdl(&MixedEnum::Value, "test");
    assert_eq!(rendered.trim(), "test value");

    let rendered = kdl_config::to_kdl(&MixedEnum::WithStruct { key: "ok".into() }, "test");
    assert_eq!(rendered.trim(), "test with-struct {\n    key \"ok\"\n}");
}

#[test]
fn invalid_enum_variant_is_error() {
    assert!(parse_named::<MixedEnum>("test unknown", "test").is_err());
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node, rename_all = "kebab-case")]
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
    let rendered = kdl_config::to_kdl(&TaggedEnum::Pair(10, "ok".into()), "choice");
    assert_eq!(rendered.trim(), "choice 1 10 \"ok\"");

    let rendered = kdl_config::to_kdl(&TaggedEnum::Enabled, "choice");
    assert_eq!(rendered.trim(), "choice #true");
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "choice", select = attr("type"), preserve)]
enum SelectAttrEnum {
    #[kdl(tag = "alpha")]
    Alpha {
        #[kdl(attr, rename = "type")]
        r#type: String,
        #[kdl(attr)]
        value: i64,
    },
    #[kdl(tag = "beta")]
    Beta,
}

#[test]
fn parses_enum_with_select_attr_preserve() {
    let parsed = parse_named::<SelectAttrEnum>("choice type=\"alpha\" value=7", "choice").unwrap();
    assert_eq!(
        parsed,
        SelectAttrEnum::Alpha {
            r#type: "alpha".into(),
            value: 7,
        }
    );
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "choice", select = any(attr("type"), arg(0)))]
enum AnySelectorEnum {
    #[kdl(tag = "alpha")]
    Alpha,
    #[kdl(tag = "beta")]
    Beta,
}

#[test]
fn parses_enum_with_any_selector() {
    let parsed = parse_named::<AnySelectorEnum>("choice type=\"alpha\"", "choice").unwrap();
    assert_eq!(parsed, AnySelectorEnum::Alpha);

    let parsed = parse_named::<AnySelectorEnum>("choice beta", "choice").unwrap();
    assert_eq!(parsed, AnySelectorEnum::Beta);
}
