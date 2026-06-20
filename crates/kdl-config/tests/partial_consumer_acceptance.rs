//! Consumer-acceptance gate (kdl-config plan, Verification Step 2): a
//! koda-representative shape — 3-level nesting, a keyed map of structs whose
//! value contains an enum, a `#[kdl(skip)]` derived field — derives cleanly
//! with **zero hand-written partials and zero manual enum `DeepMerge` impls**.

use kdl_config::merge::PartialConfig;
use kdl_config::{Kdl, KdlMerge, KdlPartial, KdlValue};
use std::collections::BTreeMap;

#[derive(Clone, Default, Debug, PartialEq, KdlValue, KdlMerge)]
enum Kind {
    #[default]
    Source,
    Sink,
}

// Map value: derives KdlMerge directly — the enum field merges via the
// generated enum `DeepMerge` (Change 1), no hand-written impl.
#[derive(Clone, Default, Debug, PartialEq, Kdl, KdlMerge)]
struct Plugin {
    #[kdl(attr, scalar)]
    kind: Kind,
    #[kdl(attr)]
    path: String,
}

#[derive(Clone, Default, Debug, PartialEq, Kdl, KdlMerge, KdlPartial)]
struct Rerank {
    #[kdl(attr)]
    lambda: u32,
}

#[derive(Clone, Default, Debug, PartialEq, Kdl, KdlMerge, KdlPartial)]
struct Search {
    #[kdl(attr)]
    mode: String,
    #[kdl(child)]
    rerank: Rerank,
}

#[derive(Clone, Default, Debug, PartialEq, Kdl, KdlMerge, KdlPartial)]
#[kdl(node = "top")]
struct Top {
    #[kdl(child)]
    search: Search,
    #[kdl(children_map, map_node = "plugin")]
    plugins: BTreeMap<String, Plugin>,
    #[kdl(skip)]
    derived: u32,
}

fn base() -> Top {
    Top {
        search: Search {
            mode: "hybrid".into(),
            rerank: Rerank { lambda: 5 },
        },
        plugins: BTreeMap::from([(
            "a".to_string(),
            Plugin {
                kind: Kind::Source,
                path: "/a".into(),
            },
        )]),
        derived: 7,
    }
}

#[test]
fn three_level_nest_decodes_and_subfield_non_clobber() {
    // A project layer that only deep-sets search.rerank.lambda.
    let p: TopPartial =
        kdl_config::parse_str("top { search { rerank lambda=9 } }").expect("decode partial");
    let out = p.apply_to(base());
    assert_eq!(out.search.mode, "hybrid"); // sibling preserved across 3 levels
    assert_eq!(out.search.rerank.lambda, 9); // deep override
    assert_eq!(out.plugins["a"].path, "/a"); // untouched
    assert_eq!(out.derived, 7); // skip preserved
}

#[test]
fn keyed_map_of_structs_with_enum_merges_by_key() {
    // A layer overriding plugin "a" and adding "b" (constructed: this is the
    // map-merge + enum-merge law, independent of the enum's decode spelling).
    let layer = TopPartial {
        search: None,
        plugins: BTreeMap::from([
            (
                "a".to_string(),
                Plugin {
                    kind: Kind::Sink,
                    path: "/a-new".into(),
                },
            ),
            (
                "b".to_string(),
                Plugin {
                    kind: Kind::Source,
                    path: "/b".into(),
                },
            ),
        ]),
    };
    let out = layer.apply_to(base());
    assert_eq!(out.plugins["a"].kind, Kind::Sink); // enum value merged (replace)
    assert_eq!(out.plugins["a"].path, "/a-new");
    assert_eq!(out.plugins["b"].path, "/b"); // new key added
    assert_eq!(out.search.mode, "hybrid"); // search untouched (None)
}
