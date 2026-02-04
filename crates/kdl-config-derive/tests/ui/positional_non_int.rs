use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Test {
    #[kdl(attr, positional = "foo")]
    value: String,
}

fn main() {}
