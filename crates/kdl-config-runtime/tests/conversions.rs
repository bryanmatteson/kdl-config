use kdl_config_runtime::{convert_value, convert_value_checked, ErrorKind, Placement, Value};

#[test]
fn type_mismatch_reports_error() {
    let err = convert_value::<i64>(
        &Value::String("nope".to_string()),
        "Test",
        "field",
        "field",
        Placement::AttrKeyed,
    )
    .unwrap_err();

    match err.kind {
        ErrorKind::TypeMismatch { expected, actual } => {
            assert_eq!(expected, "i64");
            assert_eq!(actual, "string");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn out_of_range_is_reported_for_int_coercions() {
    let err = convert_value_checked::<u32>(
        &Value::Int(-1),
        "Test",
        "field",
        "field",
        Placement::AttrKeyed,
    )
    .unwrap_err();

    match err.kind {
        ErrorKind::OutOfRange { value, target_type } => {
            assert_eq!(value, "-1");
            assert_eq!(target_type, "u32");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn array_conversion_accepts_scalar_and_array() {
    let scalar = convert_value::<Vec<i64>>(
        &Value::Int(7),
        "Test",
        "field",
        "field",
        Placement::AttrKeyed,
    )
    .unwrap();
    assert_eq!(scalar, vec![7]);

    let array = convert_value::<Vec<i64>>(
        &Value::Array(vec![Value::Int(1), Value::Int(2)]),
        "Test",
        "field",
        "field",
        Placement::AttrKeyed,
    )
    .unwrap();
    assert_eq!(array, vec![1, 2]);
}
