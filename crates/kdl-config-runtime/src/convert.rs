use crate::error::{KdlConfigError, Placement};
use crate::Value;

pub trait FromKdlValue: Sized {
    const TYPE_NAME: &'static str;
    fn from_value(value: &Value) -> Option<Self>;
}

impl FromKdlValue for String {
    const TYPE_NAME: &'static str = "string";

    fn from_value(value: &Value) -> Option<Self> {
        match value {
            Value::String(s) => Some(s.clone()),
            _ => None,
        }
    }
}

impl FromKdlValue for bool {
    const TYPE_NAME: &'static str = "bool";

    fn from_value(value: &Value) -> Option<Self> {
        match value {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

impl FromKdlValue for i64 {
    const TYPE_NAME: &'static str = "i64";

    fn from_value(value: &Value) -> Option<Self> {
        match value {
            Value::Int(n) => Some(*n),
            _ => None,
        }
    }
}

impl FromKdlValue for u64 {
    const TYPE_NAME: &'static str = "u64";

    fn from_value(value: &Value) -> Option<Self> {
        match value {
            Value::Int(n) if *n >= 0 => Some(*n as u64),
            _ => None,
        }
    }
}

impl FromKdlValue for i32 {
    const TYPE_NAME: &'static str = "i32";

    fn from_value(value: &Value) -> Option<Self> {
        match value {
            Value::Int(n) if *n >= i32::MIN as i64 && *n <= i32::MAX as i64 => Some(*n as i32),
            _ => None,
        }
    }
}

impl FromKdlValue for u32 {
    const TYPE_NAME: &'static str = "u32";

    fn from_value(value: &Value) -> Option<Self> {
        match value {
            Value::Int(n) if *n >= 0 && *n <= u32::MAX as i64 => Some(*n as u32),
            _ => None,
        }
    }
}

impl FromKdlValue for usize {
    const TYPE_NAME: &'static str = "usize";

    fn from_value(value: &Value) -> Option<Self> {
        match value {
            Value::Int(n) if *n >= 0 => {
                let n = *n as u64;
                if n <= usize::MAX as u64 {
                    Some(n as usize)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl FromKdlValue for f64 {
    const TYPE_NAME: &'static str = "f64";

    fn from_value(value: &Value) -> Option<Self> {
        match value {
            Value::Float(f) => Some(*f),
            Value::Int(n) => Some(*n as f64),
            _ => None,
        }
    }
}

impl FromKdlValue for f32 {
    const TYPE_NAME: &'static str = "f32";

    fn from_value(value: &Value) -> Option<Self> {
        match value {
            Value::Float(f) => Some(*f as f32),
            Value::Int(n) => Some(*n as f32),
            _ => None,
        }
    }
}

impl<T: FromKdlValue> FromKdlValue for Vec<T> {
    const TYPE_NAME: &'static str = "array";

    fn from_value(value: &Value) -> Option<Self> {
        match value {
            Value::Array(arr) => {
                let mut result = Vec::with_capacity(arr.len());
                for item in arr {
                    result.push(T::from_value(item)?);
                }
                Some(result)
            }
            val => Some(vec![T::from_value(val)?]),
        }
    }
}

impl<T: FromKdlValue> FromKdlValue for Option<T> {
    const TYPE_NAME: &'static str = "option";

    fn from_value(value: &Value) -> Option<Self> {
        match value {
            Value::Null => Some(None),
            _ => T::from_value(value).map(Some),
        }
    }
}

pub fn convert_value<T: FromKdlValue>(
    value: &Value,
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
    placement: Placement,
) -> Result<T, KdlConfigError> {
    T::from_value(value).ok_or_else(|| {
        KdlConfigError::type_mismatch(
            struct_name,
            field_name,
            kdl_key,
            placement,
            T::TYPE_NAME,
            value.type_name(),
        )
    })
}

pub fn convert_value_checked<T: FromKdlValue>(
    value: &Value,
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
    placement: Placement,
) -> Result<T, KdlConfigError> {
    if let Value::Int(n) = value {
        if T::from_value(value).is_none() {
            return Err(KdlConfigError::out_of_range(
                struct_name,
                field_name,
                kdl_key,
                placement,
                n.to_string(),
                T::TYPE_NAME,
            ));
        }
    }

    convert_value(value, struct_name, field_name, kdl_key, placement)
}
