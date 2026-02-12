use kdl_config::schema::{KdlSchema, SchemaLiteral, SchemaRef, SchemaType};
use kdl_config_derive::{KdlChoice, KdlNode, KdlSchema, KdlValue};

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
#[derive(KdlNode)]
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

#[derive(KdlChoice)]
#[allow(dead_code)]
enum ChoiceNode {
    #[kdl(name = "alpha")]
    Alpha(Setting),
    #[kdl(name = "beta")]
    Beta,
}

#[derive(Debug, Clone, PartialEq, KdlValue)]
enum ScalarMode {
    Fast,
    Safe,
}

#[derive(KdlSchema)]
#[allow(dead_code)]
#[kdl(node = "tuple")]
struct TupleSchemaOverride(#[kdl(schema(kind = "string"))] i64);

#[derive(KdlSchema)]
#[allow(dead_code)]
#[kdl(node = "config")]
struct ScalarSchemaConfig {
    #[kdl(attr, scalar)]
    mode: ScalarMode,
}

#[derive(KdlSchema)]
#[allow(dead_code)]
#[kdl(node = "config")]
struct PathSchemaConfig {
    #[kdl(path = "app.http", attr)]
    port: i64,
}

#[derive(KdlSchema)]
#[allow(dead_code)]
#[kdl(
    node = "config",
    schema(name = "config_schema", description = "Config schema", deny_unknown)
)]
struct OverrideConfig {
    #[kdl(attr)]
    #[kdl(schema(name = "title", description = "Title", optional))]
    name: String,

    #[kdl(positional = 0)]
    #[kdl(schema(kind = "integer", description = "Primary count"))]
    count: i64,

    #[kdl(child)]
    #[kdl(schema(name = "child", description = "Child node"))]
    setting: Setting,

    #[kdl(attr)]
    #[kdl(schema(skip))]
    ignored: String,
}

#[derive(Clone, Copy, KdlSchema)]
#[allow(dead_code)]
#[kdl(node = "leaf")]
struct Leaf {
    #[kdl(positional = 0)]
    value: i64,
}

#[derive(KdlSchema)]
#[allow(dead_code)]
union UnionChoice {
    #[kdl(schema(name = "leaf-node", description = "Leaf choice"))]
    leaf: Leaf,
    #[kdl(schema(name = "count", kind = "integer"))]
    count: i64,
}

#[test]
fn test_schema_generation() {
    let mut registry = kdl_config::schema::SchemaRegistry::default();
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
fn schema_path_inserts_nested_nodes() {
    let mut registry = kdl_config::schema::SchemaRegistry::default();
    PathSchemaConfig::register_definitions(&mut registry);

    let config_schema = registry
        .definitions
        .get("PathSchemaConfig")
        .expect("PathSchemaConfig schema not found");
    let children = config_schema
        .children
        .as_ref()
        .expect("PathSchemaConfig should have children");

    let app_node = children
        .nodes
        .iter()
        .find_map(|child| match child {
            SchemaRef::Inline(node) if node.name.as_deref() == Some("app") => Some(node),
            _ => None,
        })
        .expect("expected app node");

    let app_children = app_node
        .children
        .as_ref()
        .expect("app node should have children");
    let http_node = app_children
        .nodes
        .iter()
        .find_map(|child| match child {
            SchemaRef::Inline(node) if node.name.as_deref() == Some("http") => Some(node),
            _ => None,
        })
        .expect("expected http node");

    assert!(http_node.props.contains_key("port"));
    assert_eq!(http_node.props.get("port").unwrap().ty, SchemaType::Integer);
}

#[test]
fn test_scalar_schema_generation() {
    let mut registry = kdl_config::schema::SchemaRegistry::default();
    ScalarSchemaConfig::register_definitions(&mut registry);

    let schema = registry
        .definitions
        .get("ScalarSchemaConfig")
        .expect("ScalarSchemaConfig schema not found");
    let prop = schema.props.get("mode").expect("missing mode prop");
    assert_eq!(prop.ty, SchemaType::String);
}

#[test]
fn test_tuple_schema_override() {
    let mut registry = kdl_config::schema::SchemaRegistry::default();
    TupleSchemaOverride::register_definitions(&mut registry);

    let schema = registry
        .definitions
        .get("TupleSchemaOverride")
        .expect("TupleSchemaOverride schema not found");
    assert_eq!(schema.values.len(), 1);
    assert_eq!(schema.values[0].ty, SchemaType::String);
}

#[test]
fn test_enum_schema_generation() {
    let mut registry = kdl_config::schema::SchemaRegistry::default();
    Choice::register_definitions(&mut registry);

    let choice_schema = registry
        .definitions
        .get("Choice")
        .expect("Choice schema not found");
    assert_eq!(choice_schema.name, Some("choice".to_string()));
    assert_eq!(choice_schema.values.len(), 1);
    assert_eq!(choice_schema.values[0].ty, SchemaType::String);
    let enum_values = choice_schema.values[0]
        .enum_values
        .as_ref()
        .expect("missing enum values");
    assert_eq!(enum_values.len(), 3);
    assert!(enum_values.contains(&SchemaLiteral::String("Alpha".to_string())));
    assert!(enum_values.contains(&SchemaLiteral::String("Beta".to_string())));
    assert!(enum_values.contains(&SchemaLiteral::String("Gamma".to_string())));
    assert!(choice_schema.props.is_empty());
    assert!(choice_schema.children.is_none());
    let variants = choice_schema
        .variants
        .as_ref()
        .expect("Choice variants missing");
    assert_eq!(variants.len(), 3);

    let mut saw_tuple = false;
    let mut saw_struct = false;
    for variant in variants {
        if let SchemaRef::Inline(node) = variant {
            let tag_value = node
                .values
                .first()
                .and_then(|value| value.enum_values.as_ref())
                .expect("missing variant tag enum");
            assert_eq!(tag_value.len(), 1);
            if node.values.len() == 3 {
                assert_eq!(node.values[0].ty, SchemaType::String);
                assert_eq!(node.values[1].ty, SchemaType::Integer);
                assert_eq!(node.values[2].ty, SchemaType::String);
                saw_tuple = true;
            }
            if node.props.contains_key("enabled") {
                assert!(node.children.is_some());
                saw_struct = true;
            }
        }
    }

    assert!(saw_tuple);
    assert!(saw_struct);
}

#[test]
fn test_choice_schema_generation() {
    let schema = <ChoiceNode as KdlSchema>::schema_ref();
    match schema {
        SchemaRef::Choice(choices) => {
            assert_eq!(choices.len(), 2);
            let mut names = Vec::new();
            for choice in choices {
                match choice {
                    SchemaRef::Inline(node) => {
                        names.push(node.name.unwrap_or_default());
                    }
                    other => panic!("Unexpected schema ref: {other:?}"),
                }
            }
            names.sort();
            assert_eq!(names, vec!["alpha".to_string(), "beta".to_string()]);
        }
        other => panic!("Expected choice schema, got {other:?}"),
    }
}

#[test]
fn test_schema_overrides() {
    let mut registry = kdl_config::schema::SchemaRegistry::default();
    OverrideConfig::register_definitions(&mut registry);

    let config_schema = registry
        .definitions
        .get("OverrideConfig")
        .expect("OverrideConfig schema not found");

    assert_eq!(config_schema.name.as_deref(), Some("config_schema"));
    assert_eq!(config_schema.description.as_deref(), Some("Config schema"));
    assert_eq!(config_schema.deny_unknown, Some(true));

    let title_prop = config_schema
        .props
        .get("title")
        .expect("missing title prop");
    assert_eq!(title_prop.required, false);
    assert_eq!(title_prop.description.as_deref(), Some("Title"));

    assert!(!config_schema.props.contains_key("ignored"));

    assert_eq!(config_schema.values.len(), 1);
    assert_eq!(config_schema.values[0].ty, SchemaType::Integer);
    assert_eq!(
        config_schema.values[0].description.as_deref(),
        Some("Primary count")
    );

    let children = config_schema
        .children
        .as_ref()
        .expect("expected children for OverrideConfig");
    let mut child_names = Vec::new();
    for child in &children.nodes {
        if let SchemaRef::Inline(node) = child {
            child_names.push(node.name.clone().unwrap_or_default());
            if node.name.as_deref() == Some("child") {
                assert_eq!(node.description.as_deref(), Some("Child node"));
            }
        }
    }
    child_names.sort();
    assert_eq!(child_names, vec!["child".to_string()]);
}

#[test]
fn test_union_schema_generation() {
    let schema = <UnionChoice as KdlSchema>::schema_ref();
    match schema {
        SchemaRef::Choice(choices) => {
            let mut names = Vec::new();
            for choice in choices {
                match choice {
                    SchemaRef::Inline(node) => {
                        if let Some(name) = node.name.clone() {
                            names.push(name);
                        }
                        if node.name.as_deref() == Some("count") {
                            assert_eq!(node.values.len(), 1);
                            assert_eq!(node.values[0].ty, SchemaType::Integer);
                        }
                        if node.name.as_deref() == Some("leaf-node") {
                            assert_eq!(node.description.as_deref(), Some("Leaf choice"));
                        }
                    }
                    other => panic!("Unexpected schema ref: {other:?}"),
                }
            }
            names.sort();
            assert_eq!(names, vec!["count".to_string(), "leaf-node".to_string()]);
        }
        other => panic!("Expected choice schema, got {other:?}"),
    }
}
