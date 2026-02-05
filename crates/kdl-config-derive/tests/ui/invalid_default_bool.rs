use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
#[kdl(default_bool = "nope")]
struct Config {
    enabled: bool,
}

fn main() {}
