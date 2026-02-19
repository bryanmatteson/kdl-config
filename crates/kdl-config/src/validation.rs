use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use crate::error::{KdlConfigError, Placement};
use crate::schema::Validation;

// ---------------------------------------------------------------------------
// KdlValidate — run a single Validation rule against a value
// ---------------------------------------------------------------------------

pub trait KdlValidate {
    fn kdl_validate(&self, validation: &Validation) -> Result<(), String>;
}

// --- Numeric implementations (cast to f64, delegate to validate_number) ---

macro_rules! impl_kdl_validate_numeric {
    ($($ty:ty),*) => {
        $(
            impl KdlValidate for $ty {
                #[inline]
                fn kdl_validate(&self, validation: &Validation) -> Result<(), String> {
                    validation.validate_number(*self as f64)
                }
            }
        )*
    };
}

impl_kdl_validate_numeric!(i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, isize, usize);

// --- String implementations ---

impl KdlValidate for String {
    #[inline]
    fn kdl_validate(&self, validation: &Validation) -> Result<(), String> {
        validation.validate_str(self.as_str())
    }
}

impl KdlValidate for &str {
    #[inline]
    fn kdl_validate(&self, validation: &Validation) -> Result<(), String> {
        validation.validate_str(self)
    }
}

impl KdlValidate for std::path::PathBuf {
    #[inline]
    fn kdl_validate(&self, validation: &Validation) -> Result<(), String> {
        validation.validate_str(self.to_string_lossy().as_ref())
    }
}

// --- Bool (no-op) ---

impl KdlValidate for bool {
    #[inline]
    fn kdl_validate(&self, _validation: &Validation) -> Result<(), String> {
        Ok(())
    }
}

// --- Option<T> (skip if None) ---

impl<T: KdlValidate> KdlValidate for Option<T> {
    #[inline]
    fn kdl_validate(&self, validation: &Validation) -> Result<(), String> {
        match self {
            Some(inner) => inner.kdl_validate(validation),
            None => Ok(()),
        }
    }
}

// ---------------------------------------------------------------------------
// KdlValidateCount — collection length for MinItems / MaxItems
// ---------------------------------------------------------------------------

pub trait KdlValidateCount {
    fn count(&self) -> usize;
}

impl<T> KdlValidateCount for Vec<T> {
    #[inline]
    fn count(&self) -> usize {
        self.len()
    }
}

impl<T> KdlValidateCount for Option<Vec<T>> {
    #[inline]
    fn count(&self) -> usize {
        match self {
            Some(v) => v.len(),
            None => 0,
        }
    }
}

impl<K, V> KdlValidateCount for HashMap<K, V> {
    #[inline]
    fn count(&self) -> usize {
        self.len()
    }
}

// Note: Vec<(K, V)> is already covered by the Vec<T> impl above.

// ---------------------------------------------------------------------------
// AsF64 — numeric conversion for cross-field validation codegen
// ---------------------------------------------------------------------------

pub trait AsF64 {
    fn as_f64(&self) -> f64;
}

macro_rules! impl_as_f64 {
    ($($ty:ty),*) => {
        $(
            impl AsF64 for $ty {
                #[inline]
                fn as_f64(&self) -> f64 {
                    *self as f64
                }
            }
        )*
    };
}

impl_as_f64!(i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, isize, usize);

// ---------------------------------------------------------------------------
// Relational cross-field helpers (exists_in / subset_of)
// ---------------------------------------------------------------------------

pub trait KdlContains<T> {
    fn kdl_contains(&self, value: &T) -> bool;
}

impl<T: PartialEq> KdlContains<T> for Vec<T> {
    #[inline]
    fn kdl_contains(&self, value: &T) -> bool {
        self.iter().any(|candidate| candidate == value)
    }
}

impl<T: Eq + Hash> KdlContains<T> for HashSet<T> {
    #[inline]
    fn kdl_contains(&self, value: &T) -> bool {
        self.contains(value)
    }
}

impl<K: Eq + Hash, V> KdlContains<K> for HashMap<K, V> {
    #[inline]
    fn kdl_contains(&self, value: &K) -> bool {
        self.contains_key(value)
    }
}

pub trait KdlSubsetOf<Rhs = Self> {
    fn kdl_subset_of(&self, other: &Rhs) -> bool;
}

impl<T: PartialEq> KdlSubsetOf<Vec<T>> for Vec<T> {
    #[inline]
    fn kdl_subset_of(&self, other: &Vec<T>) -> bool {
        self.iter()
            .all(|item| other.iter().any(|candidate| candidate == item))
    }
}

impl<T: Eq + Hash> KdlSubsetOf<HashSet<T>> for HashSet<T> {
    #[inline]
    fn kdl_subset_of(&self, other: &HashSet<T>) -> bool {
        self.is_subset(other)
    }
}

impl<K: Eq + Hash, V> KdlSubsetOf<HashMap<K, V>> for Vec<K> {
    #[inline]
    fn kdl_subset_of(&self, other: &HashMap<K, V>) -> bool {
        self.iter().all(|item| other.contains_key(item))
    }
}

impl<K: Eq + Hash, V1, V2> KdlSubsetOf<HashMap<K, V2>> for HashMap<K, V1> {
    #[inline]
    fn kdl_subset_of(&self, other: &HashMap<K, V2>) -> bool {
        self.keys().all(|key| other.contains_key(key))
    }
}

pub fn run_exists_in_validation<T, C: KdlContains<T>>(
    value: &T,
    collection: &C,
    other_field: &str,
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
) -> Result<(), KdlConfigError> {
    if !collection.kdl_contains(value) {
        return Err(KdlConfigError::invalid_value(
            struct_name,
            field_name,
            kdl_key,
            Placement::Unknown,
            "(value)",
            format!("value must exist in '{}'", other_field),
        ));
    }
    Ok(())
}

pub fn run_subset_of_validation<T, U: KdlSubsetOf<T>>(
    value: &U,
    other: &T,
    other_field: &str,
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
) -> Result<(), KdlConfigError> {
    if !value.kdl_subset_of(other) {
        return Err(KdlConfigError::invalid_value(
            struct_name,
            field_name,
            kdl_key,
            Placement::Unknown,
            "(collection)",
            format!("value must be a subset of '{}'", other_field),
        ));
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Helper: run per-field (non-cross-field, non-Func) validations
// ---------------------------------------------------------------------------

pub fn run_field_validations<T: KdlValidate>(
    value: &T,
    validations: &[Validation],
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
) -> Result<(), KdlConfigError> {
    for v in validations {
        // Skip cross-field validators — handled separately.
        if v.cross_field_ref().is_some() {
            continue;
        }
        // Skip Func validators — handled via run_func_validation.
        if matches!(v, Validation::Func(_)) {
            continue;
        }
        if let Err(msg) = value.kdl_validate(v) {
            return Err(KdlConfigError::invalid_value(
                struct_name,
                field_name,
                kdl_key,
                Placement::Unknown,
                format!("{:?}", v),
                msg,
            ));
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Helper: run collection-count validations (MinItems / MaxItems)
// ---------------------------------------------------------------------------

pub fn run_count_validations<T: KdlValidateCount>(
    value: &T,
    validations: &[Validation],
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
) -> Result<(), KdlConfigError> {
    let count = value.count();
    for v in validations {
        if let Err(msg) = v.validate_count(count) {
            return Err(KdlConfigError::invalid_value(
                struct_name,
                field_name,
                kdl_key,
                Placement::Unknown,
                format!("{} items", count),
                msg,
            ));
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Helper: run a Func validation
// ---------------------------------------------------------------------------

pub fn run_func_validation<T, F: Fn(&T) -> Result<(), String>>(
    value: &T,
    func: F,
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
) -> Result<(), KdlConfigError> {
    if let Err(msg) = func(value) {
        return Err(KdlConfigError::invalid_value(
            struct_name,
            field_name,
            kdl_key,
            Placement::Unknown,
            "(custom)",
            msg,
        ));
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Helper: run a cross-field validation
// ---------------------------------------------------------------------------

pub fn run_cross_field_validation(
    value: f64,
    other_value: f64,
    validation: &Validation,
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
) -> Result<(), KdlConfigError> {
    let other_field = validation
        .cross_field_ref()
        .expect("run_cross_field_validation called with non-cross-field validation");

    if let Err(msg) = validation.validate_cross_field(value, other_field, other_value) {
        return Err(KdlConfigError::invalid_value(
            struct_name,
            field_name,
            kdl_key,
            Placement::Unknown,
            format!("{}", value),
            msg,
        ));
    }
    Ok(())
}
