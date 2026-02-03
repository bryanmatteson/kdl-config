use kdl_config::derive::{KdlChoice, KdlNode, KdlSchema};
use kdl_config::runtime::parse_str;

#[derive(Debug, PartialEq, KdlNode, KdlSchema)]
struct PrefixFilter {
    #[kdl(attr, positional = 0)]
    pattern: String,
}

#[derive(Debug, PartialEq, KdlNode, KdlSchema)]
struct RegexFilter {
    #[kdl(attr, positional = 0)]
    pattern: String,
}

#[derive(Debug, PartialEq, KdlChoice)]
enum Filter {
    #[kdl(name = "prefix")]
    Prefix(PrefixFilter),
    #[kdl(name = "regex")]
    Regex(RegexFilter),
    Skip,
}

#[test]
fn test_choice_direct_match() {
    let kdl = r#"prefix "/src/""#;
    let filter: Filter = parse_str(kdl).unwrap();
    assert_eq!(
        filter,
        Filter::Prefix(PrefixFilter {
            pattern: "/src/".into()
        })
    );
}

#[test]
fn test_choice_render() {
    let filter = Filter::Regex(RegexFilter {
        pattern: ".*\\.rs".into(),
    });
    let rendered = kdl_config::runtime::to_kdl(&filter, "filter");
    assert!(rendered.contains("regex"));
}
