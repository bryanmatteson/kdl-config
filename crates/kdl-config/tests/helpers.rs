use kdl_config::{ErrorKind, FlagStyle, KdlDocument, KdlNode, Placement, helpers};

fn node_with_flags(flags: &[&str]) -> KdlNode {
    let args = flags.join(" ");
    let doc: KdlDocument = format!("config {args}").parse().expect("valid kdl");
    doc.nodes().first().expect("missing node").clone()
}

#[test]
fn conflicting_flags_are_errors() {
    let node = node_with_flags(&["enabled", "no-enabled"]);
    let err = helpers::find_flag_with_style(
        &node,
        "enabled",
        FlagStyle::ValueNo,
        None,
        None,
        "Test",
        "enabled",
    )
    .unwrap_err();

    match err.kind {
        ErrorKind::ConflictingFlags { pos, neg } => {
            assert_eq!(pos, "enabled");
            assert_eq!(neg, "no-enabled");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn both_flag_style_is_ambiguous_when_both_sets_present() {
    let node = node_with_flags(&["enabled", "with-enabled"]);
    let err = helpers::find_flag_with_style(
        &node,
        "enabled",
        FlagStyle::Both,
        None,
        None,
        "Test",
        "enabled",
    )
    .unwrap_err();

    match err.kind {
        ErrorKind::AmbiguousPlacement { .. } => {}
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn custom_flags_require_both_overrides() {
    let node = node_with_flags(&["on"]);
    let err = helpers::find_flag_with_style(
        &node,
        "enabled",
        FlagStyle::ValueNo,
        Some("on".to_string()),
        None,
        "Test",
        "enabled",
    )
    .unwrap_err();

    match err.kind {
        ErrorKind::Custom(msg) => assert!(msg.contains("flag override requires both")),
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn resolve_scalar_first_last_append_behavior() {
    let mut current = None;
    helpers::resolve_scalar(
        kdl_config::ConflictPolicy::First,
        &mut current,
        1,
        "Test",
        "field",
        "field",
        Placement::AttrKeyed,
    )
    .unwrap();
    helpers::resolve_scalar(
        kdl_config::ConflictPolicy::First,
        &mut current,
        2,
        "Test",
        "field",
        "field",
        Placement::AttrKeyed,
    )
    .unwrap();
    assert_eq!(current, Some(1));

    helpers::resolve_scalar(
        kdl_config::ConflictPolicy::Last,
        &mut current,
        3,
        "Test",
        "field",
        "field",
        Placement::AttrKeyed,
    )
    .unwrap();
    assert_eq!(current, Some(3));

    let err = helpers::resolve_scalar(
        kdl_config::ConflictPolicy::Append,
        &mut current,
        4,
        "Test",
        "field",
        "field",
        Placement::AttrKeyed,
    )
    .unwrap_err();

    match err.kind {
        ErrorKind::AmbiguousPlacement { .. } => {}
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn resolve_vec_append_behavior() {
    let mut current = None;
    helpers::resolve_vec(
        kdl_config::ConflictPolicy::Append,
        &mut current,
        vec![1, 2],
        "Test",
        "field",
        "field",
        Placement::Value,
    )
    .unwrap();
    helpers::resolve_vec(
        kdl_config::ConflictPolicy::Append,
        &mut current,
        vec![3],
        "Test",
        "field",
        "field",
        Placement::Value,
    )
    .unwrap();

    assert_eq!(current, Some(vec![1, 2, 3]));
}
