use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Test {
    #[kdl(attr, neg_flag = "off")]
    enabled: bool,
}

fn main() {}
