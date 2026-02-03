use kdl_config::derive::{Kdl, KdlNode};
use kdl_config::runtime::parse_str;

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "config")]
struct Config {
    #[kdl(attr)]
    level: String,
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(choice)]
enum Filter {
    #[kdl(name = "prefix")]
    Prefix(Prefix),
    #[kdl(name = "regex")]
    Regex(Regex),
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "prefix")]
struct Prefix {
    #[kdl(attr, positional = 0)]
    pattern: String,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "regex")]
struct Regex {
    #[kdl(attr, positional = 0)]
    pattern: String,
}

#[test]
fn kdl_derive_node_defaults_to_node_mode() {
    let config: Config = parse_str(r#"config level="info""#).unwrap();
    assert_eq!(config.level, "info".to_string());
}

#[test]
fn kdl_derive_choice_parses_by_node_name() {
    let filter: Filter = parse_str(r#"prefix "/src/""#).unwrap();
    assert_eq!(
        filter,
        Filter::Prefix(Prefix {
            pattern: "/src/".to_string()
        })
    );
}

#[derive(Kdl)]
#[allow(dead_code)]
#[kdl(schema)]
union OnlySchema {
    value: u32,
    other: f32,
}

#[test]
fn kdl_derive_union_schema_only_compiles() {
    let _ = <OnlySchema as kdl_config::runtime::schema::KdlSchema>::schema_ref();
}
