use std::collections::HashMap;
use std::path::PathBuf;

use kdl_config_derive::KdlNode;
use kdl_config_runtime::{KdlConfigError, KdlParse, Modifier, parse_config};

const FIXTURES_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures");

fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(FIXTURES_DIR).join(name)
}

fn read_fixture(name: &str) -> String {
    std::fs::read_to_string(fixture_path(name)).expect("failed to read fixture")
}

fn parse_named<T: KdlParse>(kdl: &str, name: &str) -> Result<T, KdlConfigError> {
    let root = parse_config(kdl)?;
    let node = root.child(name).expect("missing node");
    T::from_node(node, &Default::default())
}

fn default_retry_limit() -> i64 {
    3
}

fn is_empty_string(value: &String) -> bool {
    value.is_empty()
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config", rename_all = "kebab-case", deny_unknown)]
struct KitchenSink {
    #[kdl(attr, positional = 0)]
    id: String,
    #[kdl(attr, positional = 1)]
    version: i64,
    #[kdl(attr, flag)]
    flag_only: bool,
    #[kdl(attr, flag = "on", neg_flag = "off")]
    custom_flag: bool,
    #[kdl(attr, bool = "presence-only")]
    presence_only: bool,
    #[kdl(attr, flag_style = "with|without")]
    styled_flag: bool,
    #[kdl(attr)]
    optional_bool: bool,
    #[kdl(attr, bool = "value-only")]
    value_only: bool,
    #[kdl(attr)]
    name: String,
    #[kdl(attr, keyed)]
    explicit_keyed: String,
    #[kdl(name = "renamed-field", attr)]
    renamed_field: String,
    #[kdl(value)]
    tags: Vec<String>,
    #[kdl(attr, value, conflict = "append")]
    ports: Vec<i64>,
    #[kdl(value, bool = "presence-only")]
    value_presence: bool,
    #[kdl(value, render = "value")]
    threshold: i64,
    #[kdl(default)]
    defaulted: i64,
    #[kdl(default = 5)]
    literal_default: i64,
    #[kdl(default_fn = "default_retry_limit")]
    retry_limit: i64,
    #[kdl(attr, default = "", skip_serializing_if = "is_empty_string")]
    empty_skipped: String,
    #[kdl(value, optional)]
    optional_items: Vec<String>,
    #[kdl(child)]
    database: DatabaseConfig,
    #[kdl(children, name = "plugin")]
    plugins: Vec<PluginConfig>,
    #[kdl(registry, container = "rule", conflict = "last")]
    rules: HashMap<String, RuleConfig>,
    #[kdl(child)]
    defaults: DefaultsConfig,
    #[kdl(child)]
    conflicts: ConflictConfig,
    #[kdl(skip)]
    skipped: String,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "database", rename_all = "kebab-case")]
struct DatabaseConfig {
    #[kdl(attr)]
    host: String,
    #[kdl(attr)]
    port: i64,
    #[kdl(attr, bool = "value-only")]
    tls: bool,
    #[kdl(child)]
    pool: PoolConfig,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "pool", rename_all = "kebab-case")]
struct PoolConfig {
    #[kdl(attr)]
    size: i64,
    #[kdl(attr)]
    timeout_ms: i64,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "plugin", rename_all = "kebab-case")]
struct PluginConfig {
    #[kdl(attr)]
    name: String,
    #[kdl(value)]
    args: Vec<String>,
    #[kdl(attr, default = 1)]
    priority: i64,
    #[kdl(attr, bool = "presence-only")]
    enabled: bool,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "rule", rename_all = "kebab-case")]
struct RuleConfig {
    #[kdl(attr)]
    kind: String,
    #[kdl(value)]
    cidrs: Vec<String>,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(
    node = "defaults",
    rename_all = "kebab-case",
    default_placement = "value",
    default_bool = "value-only",
    default_flag_style = "with|without",
    default_conflict = "last"
)]
struct DefaultsConfig {
    count: i64,
    label: String,
    enabled: bool,
    #[kdl(attr, bool = "presence-only")]
    toggled: bool,
    #[kdl(attr, value)]
    mode: String,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "conflicts", rename_all = "kebab-case")]
struct ConflictConfig {
    #[kdl(attr, value, conflict = "first")]
    level: String,
}

#[test]
fn golden_kitchen_sink_roundtrip() {
    let input = read_fixture("kitchen-sink.kdl");
    let parsed = parse_named::<KitchenSink>(&input, "config").expect("parse kitchen sink");

    let rendered = kdl_config_runtime::to_kdl(&parsed, "config");
    let expected = read_fixture("kitchen-sink.golden.kdl");
    assert_eq!(rendered.trim(), expected.trim());

    assert_eq!(parsed.ports, vec![80, 443, 8443]);
    assert_eq!(parsed.rules.get("alpha").unwrap().kind, "deny");
    assert_eq!(parsed.defaults.mode, "safe");
    assert_eq!(parsed.conflicts.level, "warn");
    assert_eq!(parsed.defaulted, 0);
    assert_eq!(parsed.literal_default, 5);
    assert_eq!(parsed.retry_limit, 3);
    assert!(parsed.value_presence);
    assert!(!parsed.custom_flag);
    assert!(!parsed.optional_bool);
    assert!(parsed.optional_items.is_empty());
    assert!(parsed.skipped.is_empty());

    let legacy = parsed
        .plugins
        .iter()
        .find(|plugin| plugin.name == "legacy")
        .unwrap();
    assert!(!legacy.enabled);
}

#[test]
fn modifiers_are_preserved_in_raw_parse() {
    let input = read_fixture("kitchen-sink.kdl");
    let root = parse_config(&input).expect("parse kitchen sink document");
    let config = root.child("config").expect("missing config node");

    let modifiers: Vec<_> = config
        .children()
        .iter()
        .filter(|child| child.name == "plugin")
        .map(|child| child.modifier)
        .collect();

    assert!(modifiers.contains(&Modifier::Append));
    assert!(modifiers.contains(&Modifier::Remove));
    assert!(modifiers.contains(&Modifier::Replace));
    assert!(modifiers.contains(&Modifier::Inherit));
}
