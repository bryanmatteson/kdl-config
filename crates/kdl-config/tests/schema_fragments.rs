use kdl_config::schema::{
    KdlNodeSchema, SchemaRef, SchemaRegistry, SchemaType, fragment_node_schema,
};

#[test]
fn fragment_schema_includes_type_annotation_and_children() {
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
    assert_eq!(schema.values.len(), 1);
    assert_eq!(schema.values[0].ty, SchemaType::String);

    let type_annotation = schema.type_annotation.as_ref().expect("type annotation");
    assert!(!type_annotation.required);
    match &type_annotation.allowed {
        SchemaRef::Choice(choices) => {
            assert!(
                choices
                    .iter()
                    .any(|choice| matches!(choice, SchemaRef::Ref(name) if name == "Source"))
            );
        }
        _ => panic!("expected choice for type annotation"),
    }

    let children = schema.children.as_ref().expect("children");
    assert_eq!(children.nodes.len(), 2);
    assert!(children.nodes.iter().any(|node| {
        match node {
            SchemaRef::Choice(choices) => choices
                .iter()
                .any(|choice| matches!(choice, SchemaRef::Ref(name) if name == "Source")),
            _ => false,
        }
    }));
    assert!(children.nodes.iter().any(|node| match node {
        SchemaRef::Choice(choices) => choices.iter().any(|choice| {
            matches!(
                choice,
                SchemaRef::Inline(schema) if schema.name.as_deref() == Some("~source")
            )
        }),
        _ => false,
    }));
}
