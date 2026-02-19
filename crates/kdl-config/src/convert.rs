use crate::error::{KdlConfigError, Placement};
use kdl::KdlValue;
use std::path::PathBuf;

pub trait FromKdlValue: Sized {
    const TYPE_NAME: &'static str;
    fn from_value(value: &KdlValue) -> Option<Self>;
}

fn kdl_value_type_name(value: &KdlValue) -> &'static str {
    match value {
        KdlValue::Null => "null",
        KdlValue::Bool(_) => "bool",
        KdlValue::Integer(_) => "int",
        KdlValue::Float(_) => "float",
        KdlValue::String(_) => "string",
    }
}

/// Context for converting a KDL value into a typed field.
#[derive(Debug, Clone, Copy)]
pub struct ConvertContext<'a> {
    pub struct_name: &'a str,
    pub field_name: &'a str,
    pub kdl_key: &'a str,
    pub placement: Placement,
}

impl<'a> ConvertContext<'a> {
    pub fn new(struct_name: &'a str, field_name: &'a str) -> Self {
        Self {
            struct_name,
            field_name,
            kdl_key: field_name,
            placement: Placement::Unknown,
        }
    }

    pub fn key(mut self, kdl_key: &'a str) -> Self {
        self.kdl_key = kdl_key;
        self
    }

    pub fn placement(mut self, placement: Placement) -> Self {
        self.placement = placement;
        self
    }

    pub fn attr(self) -> Self {
        self.placement(Placement::AttrKeyed)
    }

    pub fn positional(self) -> Self {
        self.placement(Placement::AttrPositional)
    }

    pub fn flag(self) -> Self {
        self.placement(Placement::AttrFlag)
    }

    pub fn value(self) -> Self {
        self.placement(Placement::Value)
    }

    pub fn child(self) -> Self {
        self.placement(Placement::Child)
    }

    pub fn children(self) -> Self {
        self.placement(Placement::Children)
    }

    pub fn registry(self) -> Self {
        self.placement(Placement::Registry)
    }
}

impl FromKdlValue for String {
    const TYPE_NAME: &'static str = "string";

    fn from_value(value: &KdlValue) -> Option<Self> {
        match value {
            KdlValue::String(s) => Some(s.clone()),
            _ => None,
        }
    }
}

impl FromKdlValue for PathBuf {
    const TYPE_NAME: &'static str = "path";

    fn from_value(value: &KdlValue) -> Option<Self> {
        match value {
            KdlValue::String(s) => Some(PathBuf::from(s)),
            _ => None,
        }
    }
}

impl FromKdlValue for bool {
    const TYPE_NAME: &'static str = "bool";

    fn from_value(value: &KdlValue) -> Option<Self> {
        match value {
            KdlValue::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

impl FromKdlValue for i64 {
    const TYPE_NAME: &'static str = "i64";

    fn from_value(value: &KdlValue) -> Option<Self> {
        match value {
            KdlValue::Integer(n) if *n >= i64::MIN as i128 && *n <= i64::MAX as i128 => {
                Some(*n as i64)
            }
            _ => None,
        }
    }
}

impl FromKdlValue for i128 {
    const TYPE_NAME: &'static str = "i128";

    fn from_value(value: &KdlValue) -> Option<Self> {
        match value {
            KdlValue::Integer(n) => Some(*n),
            _ => None,
        }
    }
}

impl FromKdlValue for u64 {
    const TYPE_NAME: &'static str = "u64";

    fn from_value(value: &KdlValue) -> Option<Self> {
        match value {
            KdlValue::Integer(n) if *n >= 0 && *n <= u64::MAX as i128 => Some(*n as u64),
            _ => None,
        }
    }
}

impl FromKdlValue for i32 {
    const TYPE_NAME: &'static str = "i32";

    fn from_value(value: &KdlValue) -> Option<Self> {
        match value {
            KdlValue::Integer(n) if *n >= i32::MIN as i128 && *n <= i32::MAX as i128 => {
                Some(*n as i32)
            }
            _ => None,
        }
    }
}

impl FromKdlValue for u32 {
    const TYPE_NAME: &'static str = "u32";

    fn from_value(value: &KdlValue) -> Option<Self> {
        match value {
            KdlValue::Integer(n) if *n >= 0 && *n <= u32::MAX as i128 => Some(*n as u32),
            _ => None,
        }
    }
}

impl FromKdlValue for usize {
    const TYPE_NAME: &'static str = "usize";

    fn from_value(value: &KdlValue) -> Option<Self> {
        match value {
            KdlValue::Integer(n) if *n >= 0 => {
                if *n <= usize::MAX as i128 {
                    Some(*n as usize)
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

    fn from_value(value: &KdlValue) -> Option<Self> {
        match value {
            KdlValue::Float(f) => Some(*f),
            KdlValue::Integer(n) => Some(*n as f64),
            _ => None,
        }
    }
}

impl FromKdlValue for f32 {
    const TYPE_NAME: &'static str = "f32";

    fn from_value(value: &KdlValue) -> Option<Self> {
        match value {
            KdlValue::Float(f) => Some(*f as f32),
            KdlValue::Integer(n) => Some(*n as f32),
            _ => None,
        }
    }
}

impl<T: FromKdlValue> FromKdlValue for Vec<T> {
    const TYPE_NAME: &'static str = "array";

    fn from_value(value: &KdlValue) -> Option<Self> {
        Some(vec![T::from_value(value)?])
    }
}

impl<T: FromKdlValue> FromKdlValue for Option<T> {
    const TYPE_NAME: &'static str = "option";

    fn from_value(value: &KdlValue) -> Option<Self> {
        match value {
            KdlValue::Null => Some(None),
            _ => T::from_value(value).map(Some),
        }
    }
}

pub fn convert_value<T: FromKdlValue>(
    value: &KdlValue,
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
            kdl_value_type_name(value),
        )
    })
}

pub fn convert_value_checked<T: FromKdlValue>(
    value: &KdlValue,
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
    placement: Placement,
) -> Result<T, KdlConfigError> {
    if let KdlValue::Integer(n) = value {
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

pub fn convert_value_ctx<T: FromKdlValue>(
    value: &KdlValue,
    ctx: ConvertContext<'_>,
) -> Result<T, KdlConfigError> {
    convert_value(
        value,
        ctx.struct_name,
        ctx.field_name,
        ctx.kdl_key,
        ctx.placement,
    )
}

pub fn convert_value_checked_ctx<T: FromKdlValue>(
    value: &KdlValue,
    ctx: ConvertContext<'_>,
) -> Result<T, KdlConfigError> {
    convert_value_checked(
        value,
        ctx.struct_name,
        ctx.field_name,
        ctx.kdl_key,
        ctx.placement,
    )
}

/// Convenience methods on `Value` for context-aware conversion.
pub trait ValueConvertExt {
    fn convert_with<T: FromKdlValue>(&self, ctx: ConvertContext<'_>) -> Result<T, KdlConfigError>;
    fn convert_with_checked<T: FromKdlValue>(
        &self,
        ctx: ConvertContext<'_>,
    ) -> Result<T, KdlConfigError>;
}

impl ValueConvertExt for KdlValue {
    fn convert_with<T: FromKdlValue>(&self, ctx: ConvertContext<'_>) -> Result<T, KdlConfigError> {
        convert_value_ctx(self, ctx)
    }

    fn convert_with_checked<T: FromKdlValue>(
        &self,
        ctx: ConvertContext<'_>,
    ) -> Result<T, KdlConfigError> {
        convert_value_checked_ctx(self, ctx)
    }
}
