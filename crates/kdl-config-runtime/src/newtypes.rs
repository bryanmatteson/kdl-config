use std::fmt;
use std::str::FromStr;

use crate::schema::{KdlNodeSchema, KdlSchema, SchemaRef, SchemaRegistry, SchemaType, SchemaValue};
use crate::{FromKdlValue, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Duration(std::time::Duration);

#[derive(Debug, Clone, PartialEq, Eq)]
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
        days
            .checked_mul(24 * 60 * 60)
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

    fn from_value(value: &Value) -> Option<Self> {
        match value {
            Value::String(s) => s.parse().ok(),
            Value::Int(n) => (*n)
                .try_into()
                .ok()
                .map(|millis: u64| Self(std::time::Duration::from_millis(millis))),
            Value::Float(f) => duration_from_millis_f64(*f).map(Self),
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
                ty: SchemaType::Integer,
                required: true,
                description: Some("milliseconds".to_string()),
                enum_values: None,
            }],
            ..Default::default()
        })
    }

    fn register_definitions(registry: &mut SchemaRegistry) {
        let _ = registry;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Weight(f64);

#[derive(Debug, Clone, Copy, PartialEq)]
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

    fn from_value(value: &Value) -> Option<Self> {
        let raw = match value {
            Value::Float(f) => *f,
            Value::Int(n) => *n as f64,
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

#[cfg(test)]
mod tests {
    use super::{Duration, Weight};
    use crate::FromKdlValue;
    use crate::Value;

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
        let val = Value::String("5m".to_string());
        let parsed = Duration::from_value(&val).unwrap();
        assert_eq!(parsed.as_millis(), 300_000);

        let val = Value::Int(250);
        let parsed = Duration::from_value(&val).unwrap();
        assert_eq!(parsed.as_millis(), 250);

        let val = Value::Float(2.5);
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
}
