use kdl_config_derive::KdlNode;

#[derive(KdlNode)]
struct Config {
    #[kdl(schema(kind = "nope"))]
    value: i64,
}

fn main() {}
