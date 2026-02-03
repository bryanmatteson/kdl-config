use std::collections::HashMap;

use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Entry {
    value: i64,
}

#[derive(KdlNode)]
struct Test {
    #[kdl(registry, conflict = "append")]
    entries: HashMap<String, Entry>,
}

fn main() {}
