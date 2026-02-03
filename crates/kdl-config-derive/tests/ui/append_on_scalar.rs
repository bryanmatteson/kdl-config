use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Test {
    #[kdl(attr, conflict = "append")]
    value: i64,
}

fn main() {}
