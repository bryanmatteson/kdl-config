use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Test {
    #[kdl(unknown_attr)]
    value: i64,
}

fn main() {}
