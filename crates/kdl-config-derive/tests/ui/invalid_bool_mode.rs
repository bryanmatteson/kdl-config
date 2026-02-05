use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Config {
    #[kdl(attr, bool = "nope")]
    enabled: bool,
}

fn main() {}
