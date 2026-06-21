//! Regression: a base leaf with `#[kdl(default)]`/`default_fn`/`skip_serializing_if`
//! must NOT carry those attrs into the generated partial. If it does, an absent
//! leaf decodes to `Some(<default>)` instead of `None`, clobbering lower layers.

use kdl_config::merge::PartialConfig;
use kdl_config::{Kdl, KdlMerge, KdlPartial};

fn default_n() -> u32 {
    42
}

fn is_empty_string(s: &String) -> bool {
    s.is_empty()
}

#[derive(Clone, Default, Debug, PartialEq, Kdl, KdlMerge, KdlPartial)]
struct Cfg {
    #[kdl(attr, default_fn = "default_n")]
    n: u32,
    #[kdl(attr, default = 5)]
    m: u32,
    #[kdl(attr, default = "", skip_serializing_if = "is_empty_string")]
    label: String,
    #[kdl(attr)]
    name: String,
}

#[test]
fn defaulted_leaves_stay_none_when_absent_in_a_layer() {
    // base holds non-default values; a layer that only sets `name` must leave
    // n/m/label untouched (not reset to their decode defaults).
    let base = Cfg {
        n: 7,
        m: 9,
        label: "kept".into(),
        name: "base".into(),
    };
    let p: CfgPartial = kdl_config::parse_str("cfg name=\"layer\"").unwrap();
    let out = p.apply_to(base);
    assert_eq!(out.n, 7); // default_fn leaf preserved (NOT reset to 42)
    assert_eq!(out.m, 9); // default leaf preserved (NOT reset to 5)
    assert_eq!(out.label, "kept"); // default + skip_serializing_if leaf preserved
    assert_eq!(out.name, "layer"); // the one set leaf overrides
}
