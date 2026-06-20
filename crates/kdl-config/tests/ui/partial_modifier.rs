// `modifier` fields have no layered-merge meaning; the auto-partial rejects
// them with guidance to use `#[kdl(partial = "whole")]` if intended.
use kdl_config::{Kdl, KdlPartial};

#[derive(Clone, Default, Kdl, KdlPartial)]
struct Cfg {
    #[kdl(modifier)]
    marker: kdl_config::Modifier,
    #[kdl(attr)]
    name: String,
}

fn main() {}
