use std::collections::BTreeMap;

use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Test {
    #[kdl(registry)]
    entries: BTreeMap<i64, String>,
}

fn main() {}
