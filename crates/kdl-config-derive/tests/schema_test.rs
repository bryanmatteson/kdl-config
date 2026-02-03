use kdl_config_derive::KdlSchema;
use kdl_config_runtime::schema::{KdlSchema, SchemaRef, SchemaType};

#[derive(KdlSchema)]
#[allow(unused)]
#[kdl(node = "config")]
struct Config {
    #[kdl(attr)]
    name: String,

    #[kdl(value)]
    count: i64,

    #[kdl(child)]
    setting: Setting,
}

#[allow(dead_code)]
#[derive(KdlSchema)]
struct Setting {
    #[kdl(attr)]
    enabled: bool,
}

#[allow(dead_code)]
#[derive(KdlSchema)]
#[kdl(node = "choice")]
enum Choice {
    Alpha,
    Beta(i64, String),
    Gamma {
        #[kdl(attr)]
        enabled: bool,
        #[kdl(value)]
        count: i64,
    },
}

#[test]
fn test_schema_generation() {
    let mut registry = kdl_config_runtime::schema::SchemaRegistry::default();
    Config::register_definitions(&mut registry);

    let config_schema = registry
        .definitions
        .get("Config")
        .expect("Config schema not found");
    assert_eq!(config_schema.name, Some("config".to_string()));

    // Check props
    assert!(config_schema.props.contains_key("name"));
    assert_eq!(
        config_schema.props.get("name").unwrap().ty,
        SchemaType::String
    );

    // Value placements render as child nodes
    assert_eq!(config_schema.values.len(), 0);

    // Check children
    let children = config_schema
        .children
        .as_ref()
        .expect("Should have children");
    assert_eq!(children.nodes.len(), 2);

    let mut saw_setting = false;
    let mut saw_count = false;

    for child in &children.nodes {
        match child {
            SchemaRef::Inline(node) => {
                if node.name.as_deref() == Some("setting") {
                    assert_eq!(node.ref_type, Some("Setting".to_string()));
                    saw_setting = true;
                } else if node.name.as_deref() == Some("count") {
                    assert_eq!(node.values.len(), 1);
                    assert_eq!(node.values[0].ty, SchemaType::Integer);
                    saw_count = true;
                }
            }
            _ => panic!("Expected Inline child node wrapper"),
        }
    }

    assert!(saw_setting);
    assert!(saw_count);

    // Verify Setting is in registry
    assert!(registry.definitions.contains_key("Setting"));
    let setting_schema = registry.definitions.get("Setting").unwrap();
    assert!(setting_schema.props.contains_key("enabled"));
}

#[test]
fn test_enum_schema_generation() {
    let mut registry = kdl_config_runtime::schema::SchemaRegistry::default();
    Choice::register_definitions(&mut registry);

    let choice_schema = registry
        .definitions
        .get("Choice")
        .expect("Choice schema not found");
    assert_eq!(choice_schema.name, Some("choice".to_string()));
    assert_eq!(choice_schema.values.len(), 1);
    assert_eq!(choice_schema.values[0].ty, SchemaType::String);
    assert!(choice_schema.props.is_empty());
    assert!(choice_schema.children.is_none());
}
