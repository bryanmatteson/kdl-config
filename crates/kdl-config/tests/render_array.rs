use kdl_config::{Node, Value, render_node};

#[test]
fn render_array_expands_args_and_attrs() {
    let mut node = Node::named("config");
    node.add_arg(Value::Array(vec![Value::Int(1), Value::Int(2)]));
    node.set_attr(
        "key",
        Value::Array(vec![Value::String("a".into()), Value::String("b".into())]),
    );

    let rendered = render_node(&node);
    assert!(rendered.contains("1 2"));
    assert!(rendered.contains("key=\"a\" key=\"b\""));
}
