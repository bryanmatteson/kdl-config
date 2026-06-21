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

#[derive(Clone, Default, Debug, PartialEq, Kdl, KdlMerge)]
struct Item {
    #[kdl(attr)]
    x: u32,
}

// A `children` Vec is a *required* node unless `default` makes it optional.
// `default` must therefore be KEPT on the (non-Option) mirror field — stripping
// it would make every layer require the list (regression: ConfigPartial's
// `builds_raw`/`build` became `MissingRequired`).
#[derive(Clone, Default, Debug, PartialEq, Kdl, KdlMerge, KdlPartial)]
struct WithList {
    #[kdl(children, name = "item", default)]
    items: Vec<Item>,
    #[kdl(attr)]
    name: String,
}

#[test]
fn vec_default_kept_so_partial_field_stays_optional() {
    let base = WithList {
        items: vec![Item { x: 1 }],
        name: "base".into(),
    };
    // Omitting the children list MUST decode (Vec default kept => not required).
    let p: WithListPartial = kdl_config::parse_str("withlist name=\"layer\"")
        .expect("partial decodes with the children list omitted");
    let out = p.apply_to(base);
    assert_eq!(out.items, vec![Item { x: 1 }]); // absent list => empty => no-op merge
    assert_eq!(out.name, "layer");
}
