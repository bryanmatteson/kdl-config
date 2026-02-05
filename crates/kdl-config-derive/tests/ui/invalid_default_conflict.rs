use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
#[kdl(default_conflict = "nope")]
struct Config {
    value: i64,
}

fn main() {}
