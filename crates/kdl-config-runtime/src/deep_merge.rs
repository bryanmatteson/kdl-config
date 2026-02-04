use std::path::PathBuf;

use crate::newtypes::{Duration, Weight};

/// Trait for deep-merging configuration values.
///
/// The semantics are: `self.deep_merge(other)` applies `other` as an overlay,
/// where `Some` values in `other` override `self`, and `None` preserves `self`.
///
/// For collections, the merge behavior depends on the type:
/// - `Vec`: Replace entirely (no element-wise merge)
/// - `Option<T>`: `other` wins if `Some`
/// - Scalar types: other replaces self
pub trait DeepMerge {
    /// Merge `other` into `self`, with `other` taking precedence for set values.
    ///
    /// This consumes both values and returns the merged result.
    fn deep_merge(self, other: Self) -> Self;
}

// ============================================================================
// Primitive implementations
// ============================================================================

impl DeepMerge for bool {
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        other
    }
}

impl DeepMerge for u32 {
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        other
    }
}

impl DeepMerge for u64 {
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        other
    }
}

impl DeepMerge for i32 {
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        other
    }
}

impl DeepMerge for i64 {
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        other
    }
}

impl DeepMerge for usize {
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        other
    }
}

impl DeepMerge for f32 {
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        other
    }
}

impl DeepMerge for f64 {
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        other
    }
}

impl DeepMerge for String {
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        other
    }
}

impl DeepMerge for PathBuf {
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        other
    }
}

// ============================================================================
// Newtype implementations
// ============================================================================

impl DeepMerge for Weight {
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        other
    }
}

impl DeepMerge for Duration {
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        other
    }
}

// ============================================================================
// Option implementation
// ============================================================================

impl<T: DeepMerge> DeepMerge for Option<T> {
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        match (self, other) {
            (Some(a), Some(b)) => Some(a.deep_merge(b)),
            (Some(a), None) => Some(a),
            (None, Some(b)) => Some(b),
            (None, None) => None,
        }
    }
}

// ============================================================================
// Collection implementations
// ============================================================================

impl<T> DeepMerge for Vec<T> {
    /// Vectors are replaced entirely (no element-wise merge).
    ///
    /// If the other vector is empty, we keep self (allowing partial override).
    #[inline]
    fn deep_merge(self, other: Self) -> Self {
        if other.is_empty() { self } else { other }
    }
}

#[cfg(test)]
mod tests {
    use super::DeepMerge;

    #[test]
    fn scalar_merge() {
        assert_eq!(42u32.deep_merge(100), 100);
        assert_eq!("hello".to_string().deep_merge("world".to_string()), "world");
    }

    #[test]
    fn option_merge() {
        assert_eq!(Some(1u32).deep_merge(Some(2u32)), Some(2));
        assert_eq!(Some(1u32).deep_merge(None), Some(1));
        assert_eq!(None::<u32>.deep_merge(Some(2)), Some(2));
        assert_eq!(None::<u32>.deep_merge(None), None);
    }

    #[test]
    fn vec_merge() {
        let base = vec![1, 2, 3];
        let overlay = vec![4, 5];
        assert_eq!(base.clone().deep_merge(overlay), vec![4, 5]);

        let empty: Vec<i32> = vec![];
        assert_eq!(base.deep_merge(empty), vec![1, 2, 3]);
    }
}
