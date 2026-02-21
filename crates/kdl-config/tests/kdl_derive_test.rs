use kdl_config::parse_str;
use kdl_config_derive::{Kdl, KdlNode};

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
    let _ = <OnlySchema as kdl_config::schema::KdlSchema>::schema_ref();
}

fn ensure_non_empty(value: &String) -> Result<(), String> {
    if value.trim().is_empty() {
        Err("value must not be empty".to_string())
    } else {
        Ok(())
    }
}

fn validate_eq(cfg: &ValidatedEqStyle) -> Result<(), String> {
    if cfg.level == "forbidden" {
        Err("forbidden level".to_string())
    } else {
        Ok(())
    }
}

fn validate_call(cfg: &ValidatedCallStyle) -> Result<(), String> {
    if cfg.level == "forbidden" {
        Err("forbidden level".to_string())
    } else {
        Ok(())
    }
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "validated-eq", validate(func = "validate_eq"))]
struct ValidatedEqStyle {
    #[kdl(attr, validate(func = "ensure_non_empty"))]
    level: String,
}

#[derive(Debug, PartialEq, Kdl)]
#[kdl(node = "validated-call", validate(func("validate_call")))]
struct ValidatedCallStyle {
    #[kdl(attr, validate(func("ensure_non_empty")))]
    level: String,
}

#[test]
fn kdl_derive_node_accepts_validate_eq_style() {
    let cfg: ValidatedEqStyle = parse_str(r#"validated-eq level="info""#).unwrap();
    assert_eq!(cfg.level, "info");
}

#[test]
fn kdl_derive_node_accepts_validate_call_style() {
    let cfg: ValidatedCallStyle = parse_str(r#"validated-call level="info""#).unwrap();
    assert_eq!(cfg.level, "info");
}
