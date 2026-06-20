// A `registry` over a `Vec<(String, T)>` cannot merge by key across layers, so
// the auto-partial rejects it (use a BTreeMap, or `#[kdl(partial = "whole")]`).
use kdl_config::{Kdl, KdlMerge, KdlPartial};

#[derive(Clone, Default, Kdl, KdlMerge)]
struct Item {
    #[kdl(attr)]
    n: u32,
}

#[derive(Clone, Default, Kdl, KdlMerge, KdlPartial)]
struct Reg {
    #[kdl(registry, container = "item")]
    items: Vec<(String, Item)>,
}

fn main() {}
