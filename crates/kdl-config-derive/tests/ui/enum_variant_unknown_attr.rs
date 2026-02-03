use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
enum Bad {
    #[kdl(foo)]
    Value,
}

fn main() {}
