use kdl_config::parse_str;
use kdl_config_derive::{KdlChoice, KdlNode, KdlSchema};

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
    let rendered = kdl_config::to_kdl(&filter, "filter");
    assert!(rendered.contains("regex"));
}
