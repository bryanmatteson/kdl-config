use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
#[kdl(rename_all = "nope")]
struct Config {
    some_field: i64,
}

fn main() {}
