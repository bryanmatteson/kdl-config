use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Test {
    #[kdl(children)]
    item: String,
}

fn main() {}
