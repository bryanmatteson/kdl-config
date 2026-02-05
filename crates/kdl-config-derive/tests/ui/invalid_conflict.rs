use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Config {
    #[kdl(attr, conflict = "nope")]
    value: i64,
}

fn main() {}
