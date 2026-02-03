use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Test {
    #[kdl(default = [1, 2, 3])]
    value: i64,
}

fn main() {}
