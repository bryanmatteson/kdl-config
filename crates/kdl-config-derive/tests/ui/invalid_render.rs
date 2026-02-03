use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Test {
    #[kdl(render = "nope")]
    value: i64,
}

fn main() {}
