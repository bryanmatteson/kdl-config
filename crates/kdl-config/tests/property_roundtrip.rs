use kdl_config_derive::KdlNode;
use kdl_config::{parse_config, KdlParse, ParseConfig};
use proptest::prelude::*;

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct RoundTripConfig {
    #[kdl(attr)]
    name: String,
    #[kdl(attr)]
    count: i64,
    #[kdl(attr, bool = "value-only")]
    enabled: bool,
    #[kdl(value)]
    tags: Vec<String>,
}

fn parse_named<T: KdlParse>(kdl: &str, name: &str) -> Result<T, kdl_config::KdlConfigError> {
    let root = parse_config(kdl)?;
    let node = root.child(name).expect("missing node");
    T::from_node(node, &ParseConfig::default())
}

fn arb_string() -> impl Strategy<Value = String> {
    r"[a-zA-Z0-9 _\-]{0,16}".prop_map(|s| s)
}

prop_compose! {
    fn arb_config()(name in arb_string(), count in any::<i64>(), enabled in any::<bool>(), tags in proptest::collection::vec(arb_string(), 1..6)) -> RoundTripConfig {
        RoundTripConfig { name, count, enabled, tags }
    }
}

proptest! {
    #[test]
    fn roundtrip_render_parse(value in arb_config()) {
        let kdl = kdl_config::to_kdl(&value, "config");
        let parsed = parse_named::<RoundTripConfig>(&kdl, "config").unwrap();
        prop_assert_eq!(parsed, value);
    }
}
