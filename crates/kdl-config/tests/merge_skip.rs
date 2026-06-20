use kdl_config::merge::DeepMerge;
use kdl_config::KdlMerge;

// Intentionally does NOT implement DeepMerge: a `#[kdl(skip)]` field must not
// require it, because skip fields are kept (self) on merge.
#[derive(Clone, Debug, PartialEq)]
struct Derived(u32);

#[derive(Clone, Debug, PartialEq, KdlMerge)]
struct C {
    keep_from_other: String,
    #[kdl(skip)]
    derived: Derived,
}

#[test]
fn skip_field_is_kept_from_self() {
    let base = C {
        keep_from_other: "base".into(),
        derived: Derived(1),
    };
    let overlay = C {
        keep_from_other: "overlay".into(),
        derived: Derived(2),
    };
    let merged = base.deep_merge(overlay);
    assert_eq!(merged.keep_from_other, "overlay"); // normal field: other wins
    assert_eq!(merged.derived, Derived(1)); // skip field: self kept
}
