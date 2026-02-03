use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Test {
    #[kdl(default, default_fn = "make")]
    value: i64,
}

fn make() -> i64 {
    1
}

fn main() {}
