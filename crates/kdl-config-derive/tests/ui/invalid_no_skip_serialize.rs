use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Test {
    #[kdl(no_skip_serialize = "yes")]
    value: Option<i64>,
}

fn main() {}
