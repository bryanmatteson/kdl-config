use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Config {
    #[kdl(attr, flag_style = "nope")]
    enabled: bool,
}

fn main() {}
