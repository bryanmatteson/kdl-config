use std::fmt;
use std::num::NonZeroU32;
use std::str::FromStr;

use crate::schema::{KdlNodeSchema, KdlSchema, SchemaRef, SchemaRegistry, SchemaType, SchemaValue};
use crate::{
    DecodeContext, FromKdlValue, KdlConfigError, KdlDecode, KdlNode, KdlNodeExt, KdlRender,
    KdlUpdate, NodeRenderer, Placement, UpdateContext, Value, convert_value_checked,
};
use kdl::{KdlEntry, KdlValue};

/// Generic KDL-renderable scalar wrapper for map value nodes and other
/// node-valued scalar shapes.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Scalar<T> {
    pub value: T,
}

pub type ScalarString = Scalar<String>;
pub type ScalarBool = Scalar<bool>;
pub type ScalarI64 = Scalar<i64>;
pub type ScalarI128 = Scalar<i128>;
pub type ScalarI32 = Scalar<i32>;
pub type ScalarU64 = Scalar<u64>;
pub type ScalarU32 = Scalar<u32>;
pub type ScalarUsize = Scalar<usize>;
pub type ScalarF64 = Scalar<f64>;
pub type ScalarF32 = Scalar<f32>;
pub type ScalarPathBuf = Scalar<std::path::PathBuf>;

impl<T> Scalar<T> {
    pub fn new(value: T) -> Self {
        Self { value }
    }

    pub fn into_inner(self) -> T {
        self.value
    }
}

impl ScalarString {
    pub fn as_str(&self) -> &str {
        self.value.as_str()
    }
}

impl<T> From<T> for Scalar<T> {
    fn from(value: T) -> Self {
        Self { value }
    }
}

impl From<&str> for ScalarString {
    fn from(value: &str) -> Self {
        Self {
            value: value.to_string(),
        }
    }
}

impl<T> std::ops::Deref for Scalar<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> AsRef<T> for Scalar<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl AsRef<str> for ScalarString {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl<T: fmt::Display> fmt::Display for Scalar<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: FromKdlValue> FromKdlValue for Scalar<T> {
    const TYPE_NAME: &'static str = T::TYPE_NAME;

    fn from_value(value: &KdlValue) -> Option<Self> {
        T::from_value(value).map(Self::from)
    }
}

impl<T: Into<Value>> From<Scalar<T>> for Value {
    fn from(value: Scalar<T>) -> Self {
        value.value.into()
    }
}

impl<T: KdlSchema> KdlSchema for Scalar<T> {
    fn schema_ref() -> SchemaRef {
        T::schema_ref()
    }

    fn register_definitions(registry: &mut SchemaRegistry) {
        T::register_definitions(registry);
    }
}

impl<T: FromKdlValue> KdlDecode for Scalar<T> {
    fn decode(node: &KdlNode, _ctx: &DecodeContext) -> Result<Self, KdlConfigError> {
        let struct_name = std::any::type_name::<Self>();
        let inline_value = node.args().last().copied();
        let value = node
            .iter_children()
            .find(|child| child.base_name() == "value")
            .and_then(|child| child.arg(0))
            .or_else(|| {
                // If the node carries both map key and scalar payload as positional
                // args, the scalar payload is conventionally the last arg.
                inline_value
            })
            .or_else(|| node.iter_children().next().and_then(|child| child.arg(0)))
            .ok_or_else(|| {
                KdlConfigError::missing_required(struct_name, "value", "value", Placement::Value)
            })?;

        let parsed =
            convert_value_checked::<T>(value, struct_name, "value", "value", Placement::Value)?;
        Ok(Self { value: parsed })
    }
}

impl<T: Clone + Into<Value>> KdlRender for Scalar<T> {
    fn render<W: std::fmt::Write>(
        &self,
        w: &mut W,
        name: &str,
        _indent: usize,
    ) -> std::fmt::Result {
        let mut renderer = NodeRenderer::new(name, crate::Modifier::Inherit);
        renderer.positional(0, &self.value.clone().into());
        write!(w, "{}", renderer.render())
    }
}

impl<T: Clone + Into<Value>> KdlUpdate for Scalar<T> {
    fn update(&self, node: &mut KdlNode, _ctx: &UpdateContext) -> Result<(), KdlConfigError> {
        let value = crate::value_to_kdl(&self.value.clone().into());
        node.entries_mut().clear();
        node.entries_mut().push(KdlEntry::new(value));
        if let Some(children) = node.children_mut() {
            children.nodes_mut().clear();
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Duration(std::time::Duration);

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct DurationParseError {
    input: String,
    reason: &'static str,
}

impl DurationParseError {
    fn new(input: impl Into<String>, reason: &'static str) -> Self {
        Self {
            input: input.into(),
            reason,
        }
    }
}

impl fmt::Display for DurationParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "invalid duration '{}' ({})", self.input, self.reason)
    }
}

impl std::error::Error for DurationParseError {}

impl Duration {
    pub fn from_millis(millis: u64) -> Self {
        Self(std::time::Duration::from_millis(millis))
    }

    pub fn from_secs(secs: u64) -> Self {
        Self(std::time::Duration::from_secs(secs))
    }

    pub fn from_minutes(minutes: u64) -> Option<Self> {
        minutes
            .checked_mul(60)
            .map(|secs| Self(std::time::Duration::from_secs(secs)))
    }

    pub fn from_hours(hours: u64) -> Option<Self> {
        hours
            .checked_mul(60 * 60)
            .map(|secs| Self(std::time::Duration::from_secs(secs)))
    }

    pub fn from_days(days: u64) -> Option<Self> {
        days.checked_mul(24 * 60 * 60)
            .map(|secs| Self(std::time::Duration::from_secs(secs)))
    }

    pub fn as_std(&self) -> std::time::Duration {
        self.0
    }

    pub fn as_millis(&self) -> u128 {
        self.0.as_millis()
    }

    pub fn parse(input: &str) -> Result<Self, DurationParseError> {
        let raw = input.trim();
        if raw.is_empty() {
            return Err(DurationParseError::new(input, "empty value"));
        }

        let (num, unit) = if let Some(stripped) = raw.strip_suffix("ms") {
            (stripped, DurationUnit::Millis)
        } else if let Some(stripped) = raw.strip_suffix('s') {
            (stripped, DurationUnit::Seconds)
        } else if let Some(stripped) = raw.strip_suffix('m') {
            (stripped, DurationUnit::Minutes)
        } else if let Some(stripped) = raw.strip_suffix('h') {
            (stripped, DurationUnit::Hours)
        } else if let Some(stripped) = raw.strip_suffix('d') {
            (stripped, DurationUnit::Days)
        } else {
            (raw, DurationUnit::Millis)
        };

        let value: u64 = num
            .trim()
            .parse()
            .map_err(|_| DurationParseError::new(input, "invalid number"))?;

        let duration = match unit {
            DurationUnit::Millis => std::time::Duration::from_millis(value),
            DurationUnit::Seconds => std::time::Duration::from_secs(value),
            DurationUnit::Minutes => value
                .checked_mul(60)
                .map(std::time::Duration::from_secs)
                .ok_or_else(|| DurationParseError::new(input, "overflow"))?,
            DurationUnit::Hours => value
                .checked_mul(60 * 60)
                .map(std::time::Duration::from_secs)
                .ok_or_else(|| DurationParseError::new(input, "overflow"))?,
            DurationUnit::Days => value
                .checked_mul(24 * 60 * 60)
                .map(std::time::Duration::from_secs)
                .ok_or_else(|| DurationParseError::new(input, "overflow"))?,
        };

        Ok(Self(duration))
    }
}

impl From<std::time::Duration> for Duration {
    fn from(duration: std::time::Duration) -> Self {
        Self(duration)
    }
}

impl From<Duration> for std::time::Duration {
    fn from(duration: Duration) -> Self {
        duration.0
    }
}

impl FromStr for Duration {
    type Err = DurationParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s)
    }
}

impl fmt::Display for Duration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let total_ms = self.0.as_millis();

        const MS_PER_SEC: u128 = 1_000;
        const MS_PER_MIN: u128 = 60 * MS_PER_SEC;
        const MS_PER_HOUR: u128 = 60 * MS_PER_MIN;
        const MS_PER_DAY: u128 = 24 * MS_PER_HOUR;

        if total_ms % MS_PER_DAY == 0 {
            write!(f, "{}d", total_ms / MS_PER_DAY)
        } else if total_ms % MS_PER_HOUR == 0 {
            write!(f, "{}h", total_ms / MS_PER_HOUR)
        } else if total_ms % MS_PER_MIN == 0 {
            write!(f, "{}m", total_ms / MS_PER_MIN)
        } else if total_ms % MS_PER_SEC == 0 {
            write!(f, "{}s", total_ms / MS_PER_SEC)
        } else {
            write!(f, "{}ms", total_ms)
        }
    }
}

impl FromKdlValue for Duration {
    const TYPE_NAME: &'static str = "duration";

    fn from_value(value: &KdlValue) -> Option<Self> {
        match value {
            KdlValue::String(s) => s.parse().ok(),
            KdlValue::Integer(n) => (*n)
                .try_into()
                .ok()
                .map(|millis: u64| Self(std::time::Duration::from_millis(millis))),
            KdlValue::Float(f) => duration_from_millis_f64(*f).map(Self),
            _ => None,
        }
    }
}

impl From<Duration> for Value {
    fn from(value: Duration) -> Self {
        Value::String(value.to_string())
    }
}

impl KdlSchema for Duration {
    fn schema_ref() -> SchemaRef {
        SchemaRef::Inline(KdlNodeSchema {
            values: vec![SchemaValue {
                ty: SchemaType::AnyOf(vec![
                    SchemaType::Integer,
                    SchemaType::Float,
                    SchemaType::String,
                ]),
                required: true,
                description: Some(
                    "milliseconds (integer/float) or duration string (e.g. \"5m\")".to_string(),
                ),
                enum_values: None,
                validations: vec![],
            }],
            ..Default::default()
        })
    }

    fn register_definitions(registry: &mut SchemaRegistry) {
        let _ = registry;
    }
}

impl KdlDecode for Duration {
    fn decode(node: &KdlNode, _ctx: &DecodeContext) -> Result<Self, KdlConfigError> {
        let null_value = KdlValue::Null;
        let value = node
            .arg(0)
            .or_else(|| node.iter_children().next().and_then(|child| child.arg(0)))
            .unwrap_or(&null_value);
        convert_value_checked::<Duration>(
            value,
            "Duration",
            "duration",
            "duration",
            Placement::Value,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Weight(f64);

#[derive(Debug, Clone, Copy, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct WeightError {
    value: f64,
}

impl WeightError {
    fn new(value: f64) -> Self {
        Self { value }
    }
}

impl fmt::Display for WeightError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "weight must be between 0.0 and 1.0 inclusive (got {})",
            self.value
        )
    }
}

impl std::error::Error for WeightError {}

impl Weight {
    pub fn new(value: f64) -> Result<Self, WeightError> {
        if value.is_finite() && (0.0..=1.0).contains(&value) {
            Ok(Self(value))
        } else {
            Err(WeightError::new(value))
        }
    }

    pub fn get(self) -> f64 {
        self.0
    }

    pub fn parse(s: &str) -> Result<Self, WeightError> {
        let raw = s.parse::<f64>().map_err(|_| WeightError::new(f64::NAN))?;
        Self::new(raw)
    }

    pub fn as_f32(self) -> f32 {
        self.0 as f32
    }
}

impl TryFrom<f64> for Weight {
    type Error = WeightError;

    fn try_from(value: f64) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<Weight> for f64 {
    fn from(value: Weight) -> Self {
        value.0
    }
}

impl FromKdlValue for Weight {
    const TYPE_NAME: &'static str = "weight";

    fn from_value(value: &KdlValue) -> Option<Self> {
        let raw = match value {
            KdlValue::Float(f) => *f,
            KdlValue::Integer(n) => *n as f64,
            _ => return None,
        };

        Weight::new(raw).ok()
    }
}

impl From<Weight> for Value {
    fn from(value: Weight) -> Self {
        Value::Float(value.0)
    }
}

impl KdlSchema for Weight {
    fn schema_ref() -> SchemaRef {
        <f64 as KdlSchema>::schema_ref()
    }

    fn register_definitions(registry: &mut SchemaRegistry) {
        <f64 as KdlSchema>::register_definitions(registry);
    }
}

impl KdlDecode for Weight {
    fn decode(node: &KdlNode, _ctx: &DecodeContext) -> Result<Self, KdlConfigError> {
        let null_value = KdlValue::Null;
        let value = node
            .arg(0)
            .or_else(|| node.iter_children().next().and_then(|child| child.arg(0)))
            .unwrap_or(&null_value);
        convert_value_checked::<Weight>(value, "Weight", "weight", "weight", Placement::Value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DurationUnit {
    Millis,
    Seconds,
    Minutes,
    Hours,
    Days,
}

fn duration_from_millis_f64(millis: f64) -> Option<std::time::Duration> {
    if !millis.is_finite() || millis < 0.0 {
        return None;
    }
    let truncated = millis.trunc();
    if truncated > u64::MAX as f64 {
        return None;
    }
    Some(std::time::Duration::from_millis(truncated as u64))
}

// ============================================================================
// PositiveCount (NonZeroU32 wrapper)
// ============================================================================

/// A positive count guaranteed to be at least 1.
///
/// Uses `NonZeroU32` internally for niche optimization (Option<PositiveCount>
/// has the same size as PositiveCount).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct PositiveCount(NonZeroU32);

impl PositiveCount {
    /// The minimum value (1).
    pub const ONE: Self = Self(NonZeroU32::new(1).unwrap());

    /// Create a new positive count, returning `None` if zero.
    #[inline]
    pub const fn new(value: u32) -> Option<Self> {
        match NonZeroU32::new(value) {
            Some(nz) => Some(Self(nz)),
            None => None,
        }
    }

    /// Create a positive count without validation.
    ///
    /// # Safety
    /// The value must be non-zero.
    #[inline]
    pub const unsafe fn new_unchecked(value: u32) -> Self {
        // SAFETY: Caller guarantees value is non-zero
        Self(unsafe { NonZeroU32::new_unchecked(value) })
    }

    /// Get the inner u32 value.
    #[inline]
    pub const fn get(self) -> u32 {
        self.0.get()
    }

    /// Convert to usize.
    #[inline]
    pub const fn as_usize(self) -> usize {
        self.0.get() as usize
    }

    /// Saturating add.
    #[inline]
    pub fn saturating_add(self, rhs: u32) -> Self {
        Self(self.0.saturating_add(rhs))
    }

    /// Checked multiply.
    #[inline]
    pub fn checked_mul(self, rhs: u32) -> Option<Self> {
        self.0.checked_mul(NonZeroU32::new(rhs)?).map(Self)
    }
}

impl Default for PositiveCount {
    #[inline]
    fn default() -> Self {
        Self::ONE
    }
}

impl fmt::Display for PositiveCount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl TryFrom<u32> for PositiveCount {
    type Error = &'static str;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        Self::new(value).ok_or("count must be positive (non-zero)")
    }
}

impl TryFrom<usize> for PositiveCount {
    type Error = &'static str;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        if value == 0 {
            Err("count must be positive (non-zero)")
        } else if value > u32::MAX as usize {
            Err("count too large for u32")
        } else {
            Ok(Self(NonZeroU32::new(value as u32).unwrap()))
        }
    }
}

impl KdlDecode for PositiveCount {
    fn decode(node: &KdlNode, _ctx: &DecodeContext) -> Result<Self, KdlConfigError> {
        let value = node.arg(0).ok_or_else(|| {
            KdlConfigError::missing_required(
                "PositiveCount",
                "value",
                "value",
                Placement::AttrPositional,
            )
        })?;
        <Self as FromKdlValue>::from_value(value)
            .ok_or_else(|| KdlConfigError::custom("PositiveCount", "count must be positive"))
    }
}

impl KdlRender for PositiveCount {
    fn render<W: std::fmt::Write>(
        &self,
        w: &mut W,
        name: &str,
        _indent: usize,
    ) -> std::fmt::Result {
        let mut renderer = NodeRenderer::new(name, crate::Modifier::Inherit);
        renderer.positional(0, &Value::Int(self.get() as i128));
        write!(w, "{}", renderer.render())
    }
}

impl FromKdlValue for PositiveCount {
    const TYPE_NAME: &'static str = "integer";
    fn from_value(value: &KdlValue) -> Option<Self> {
        u32::from_value(value).and_then(Self::new)
    }
}

impl From<PositiveCount> for Value {
    fn from(v: PositiveCount) -> Self {
        Value::Int(v.get() as i128)
    }
}

#[cfg(test)]
mod tests {
    use super::{Duration, PositiveCount, ScalarI64, ScalarString, Weight};
    use crate::FromKdlValue;
    use crate::KdlValue;

    #[test]
    fn duration_parses_units() {
        let d = Duration::parse("100ms").unwrap();
        assert_eq!(d.as_millis(), 100);

        let d = Duration::parse("20s").unwrap();
        assert_eq!(d.as_millis(), 20_000);

        let d = Duration::parse("5m").unwrap();
        assert_eq!(d.as_millis(), 300_000);

        let d = Duration::parse("24h").unwrap();
        assert_eq!(d.as_millis(), 86_400_000);

        let d = Duration::parse("1d").unwrap();
        assert_eq!(d.as_millis(), 86_400_000);

        let d = Duration::parse("500").unwrap();
        assert_eq!(d.as_millis(), 500);
    }

    #[test]
    fn duration_rejects_invalid() {
        assert!(Duration::parse("").is_err());
        assert!(Duration::parse("ms").is_err());
    }

    #[test]
    fn duration_from_kdl_value() {
        let val = KdlValue::String("5m".to_string());
        let parsed = Duration::from_value(&val).unwrap();
        assert_eq!(parsed.as_millis(), 300_000);

        let val = KdlValue::Integer(250);
        let parsed = Duration::from_value(&val).unwrap();
        assert_eq!(parsed.as_millis(), 250);

        let val = KdlValue::Float(2.5);
        let parsed = Duration::from_value(&val).unwrap();
        assert_eq!(parsed.as_millis(), 2);
    }

    #[test]
    fn weight_bounds() {
        assert!(Weight::new(0.0).is_ok());
        assert!(Weight::new(1.0).is_ok());
        assert!(Weight::new(-0.1).is_err());
        assert!(Weight::new(1.1).is_err());
    }

    #[test]
    fn positive_count_from_kdl_value() {
        let val = KdlValue::Integer(1);
        let parsed = PositiveCount::from_value(&val).unwrap();
        assert_eq!(parsed.get(), 1);

        let val = KdlValue::Integer(0);
        assert!(PositiveCount::from_value(&val).is_none());
    }

    #[test]
    fn scalar_decode_from_inline_value() {
        let parsed: ScalarString = crate::parse_str(r#"item "alpha""#).unwrap();
        assert_eq!(parsed.value, "alpha");

        let parsed: ScalarI64 = crate::parse_str("item 42").unwrap();
        assert_eq!(parsed.value, 42);

        let parsed: ScalarString = crate::parse_str(r#"item "key" "payload""#).unwrap();
        assert_eq!(parsed.value, "payload");
    }

    #[test]
    fn scalar_decode_from_value_child() {
        let parsed: ScalarString = crate::parse_str(
            r#"
item {
  value "beta"
}
"#,
        )
        .unwrap();
        assert_eq!(parsed.value, "beta");
    }

    #[test]
    fn scalar_render_as_single_positional_value() {
        let rendered = crate::to_kdl(&ScalarString::from("hello"), "item");
        assert_eq!(rendered.trim(), r#"item "hello""#);
    }
}
