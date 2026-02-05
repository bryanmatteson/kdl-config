use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
#[kdl(default_flag_style = "nope")]
struct Config {
    enabled: bool,
}

fn main() {}
