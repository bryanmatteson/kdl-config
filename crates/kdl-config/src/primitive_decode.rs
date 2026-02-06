use std::path::PathBuf;

use kdl::KdlValue;

use crate::{
    convert_value_checked, DecodeContext, FromKdlValue, KdlConfigError, KdlDecode, KdlNode,
    KdlNodeExt, Placement,
};

fn decode_value_node<T: FromKdlValue>(
    node: &KdlNode,
    struct_name: &'static str,
) -> Result<T, KdlConfigError> {
    let null_value = KdlValue::Null;
    let value = node
        .arg(0)
        .or_else(|| node.iter_children().next().and_then(|child| child.arg(0)))
        .unwrap_or(&null_value);
    let key = node.name_str();
    convert_value_checked::<T>(value, struct_name, key, key, Placement::Value)
}

macro_rules! impl_kdl_decode_value {
    ($ty:ty) => {
        impl KdlDecode for $ty {
            fn decode(node: &KdlNode, _ctx: &DecodeContext) -> Result<Self, KdlConfigError> {
                decode_value_node::<$ty>(node, stringify!($ty))
            }
        }
    };
}

impl_kdl_decode_value!(String);
impl_kdl_decode_value!(PathBuf);
impl_kdl_decode_value!(bool);
impl_kdl_decode_value!(i64);
impl_kdl_decode_value!(i128);
impl_kdl_decode_value!(i32);
impl_kdl_decode_value!(u64);
impl_kdl_decode_value!(u32);
impl_kdl_decode_value!(usize);
impl_kdl_decode_value!(f64);
impl_kdl_decode_value!(f32);
