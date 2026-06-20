use kdl_config::merge::DeepMerge;
use kdl_config::KdlMerge;

#[derive(Clone, PartialEq, Debug, KdlMerge)]
enum Mode {
    Fast,
    Slow,
}

#[derive(Clone, PartialEq, Debug, KdlMerge)]
struct Holder {
    #[kdl(merge = "deep")]
    mode: Mode,
    n: u32,
}

#[test]
fn enum_replaces_on_merge() {
    assert_eq!(Mode::Fast.deep_merge(Mode::Slow), Mode::Slow);
    let merged = Holder {
        mode: Mode::Fast,
        n: 1,
    }
    .deep_merge(Holder {
        mode: Mode::Slow,
        n: 2,
    });
    assert_eq!(
        merged,
        Holder {
            mode: Mode::Slow,
            n: 2
        }
    );
}

// Enum carrying a custom merge function.
fn keep_first(a: Pick, _b: Pick) -> Pick {
    a
}

#[derive(Clone, PartialEq, Debug, KdlMerge)]
#[kdl(merge(func = "keep_first"))]
enum Pick {
    A,
    B,
}

#[test]
fn enum_custom_func_is_honored() {
    assert_eq!(Pick::A.deep_merge(Pick::B), Pick::A);
}
