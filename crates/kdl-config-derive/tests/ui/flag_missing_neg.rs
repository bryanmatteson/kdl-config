use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Test {
    #[kdl(attr, flag = "on")]
    enabled: bool,
}

fn main() {}
