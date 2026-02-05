use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
#[kdl(default_placement = "nope")]
struct Config {
    value: i64,
}

fn main() {}
