use kdl_config::schema::{
    KdlNodeSchema, SchemaRef, SchemaRegistry, SchemaType, fragment_node_schema,
};

#[test]
fn fragment_schema_includes_type_arg_and_children() {
    let mut registry = SchemaRegistry::default();
    registry.add(
        "Source",
        KdlNodeSchema {
            name: Some("source".to_string()),
            ..Default::default()
        },
    );

    let schema = fragment_node_schema(&registry);
    assert_eq!(schema.name.as_deref(), Some("fragment"));
    assert_eq!(schema.values.len(), 2);
    assert_eq!(schema.values[0].ty, SchemaType::String);
    assert_eq!(schema.values[1].ty, SchemaType::String);
    assert!(schema.type_annotation.is_none());

    let children = schema.children.as_ref().expect("children");
    assert_eq!(children.nodes.len(), 1);
    assert!(children.nodes.iter().any(|node| {
        match node {
            SchemaRef::Choice(choices) => choices
                .iter()
                .any(|choice| matches!(choice, SchemaRef::Ref(name) if name == "Source")),
            _ => false,
        }
    }));
}
