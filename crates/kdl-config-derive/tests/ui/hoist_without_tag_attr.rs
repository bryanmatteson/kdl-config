use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
#[kdl(node = "child")]
struct Child {
    #[kdl(attr)]
    model: String,
}

#[derive(KdlNode)]
#[kdl(node = "config")]
struct Config {
    #[kdl(child, hoist_attrs = "model")]
    child: Child,
}

fn main() {}
