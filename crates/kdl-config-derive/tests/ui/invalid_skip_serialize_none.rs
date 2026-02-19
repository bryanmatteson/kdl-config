use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
#[kdl(skip_serialize_none = "yes")]
struct Test {
    value: Option<i64>,
}

fn main() {}
