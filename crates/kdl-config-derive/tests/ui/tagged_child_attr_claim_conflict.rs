use kdl_config_derive::{Kdl, KdlNode};

#[derive(KdlNode)]
#[kdl(node = "provider")]
struct OnnxProvider;

#[derive(Kdl)]
#[kdl(node = "provider", select = attr("provider"))]
enum Provider {
    #[kdl(tag = "onnx")]
    Onnx(OnnxProvider),
}

#[derive(KdlNode)]
#[kdl(node = "config")]
struct Config {
    #[kdl(attr)]
    provider: String,

    #[kdl(child, tag_attr = "provider")]
    backend: Provider,
}

fn main() {}
