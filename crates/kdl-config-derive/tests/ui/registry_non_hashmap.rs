use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Test {
    #[kdl(registry)]
    entries: Vec<String>,
}

fn main() {}
