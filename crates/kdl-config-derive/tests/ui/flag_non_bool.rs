use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Test {
    #[kdl(attr, flag)]
    value: i64,
}

fn main() {}
