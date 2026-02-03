use std::ops::{Deref, DerefMut};

use crate::{parse_config, to_kdl, KdlConfigError, KdlParse, KdlRender, ParseConfig};

#[derive(Debug, Clone)]
pub struct RoundTrip<T> {
    value: T,
    original: String,
    canonical: String,
    node_name: String,
    dirty: bool,
}

impl<T> RoundTrip<T> {
    pub fn value(&self) -> &T {
        &self.value
    }

    pub fn value_mut(&mut self) -> RoundTripMut<'_, T> {
        RoundTripMut { inner: self }
    }

    pub fn into_inner(self) -> T {
        self.value
    }

    pub fn original(&self) -> &str {
        &self.original
    }

    pub fn canonical(&self) -> &str {
        &self.canonical
    }

    pub fn node_name(&self) -> &str {
        &self.node_name
    }

    pub fn is_dirty(&self) -> bool {
        self.dirty
    }

    pub fn mark_dirty(&mut self) {
        self.dirty = true;
    }

    pub fn set_value(&mut self, value: T) {
        self.value = value;
        self.dirty = true;
    }
}

impl<T: KdlRender> RoundTrip<T> {
    pub fn to_kdl(&self) -> String {
        let rendered = to_kdl(&self.value, &self.node_name);
        if self.dirty {
            return rendered;
        }
        if rendered == self.canonical {
            self.original.clone()
        } else {
            rendered
        }
    }
}

pub struct RoundTripMut<'a, T> {
    inner: &'a mut RoundTrip<T>,
}

impl<'a, T> Deref for RoundTripMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner.value
    }
}

impl<'a, T> DerefMut for RoundTripMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner.value
    }
}

impl<'a, T> Drop for RoundTripMut<'a, T> {
    fn drop(&mut self) {
        self.inner.dirty = true;
    }
}

pub fn parse_str_roundtrip<T: KdlParse + KdlRender>(
    contents: &str,
) -> Result<RoundTrip<T>, KdlConfigError> {
    parse_str_with_config_roundtrip(contents, &ParseConfig::default())
}

pub fn parse_str_with_config_roundtrip<T: KdlParse + KdlRender>(
    contents: &str,
    config: &ParseConfig,
) -> Result<RoundTrip<T>, KdlConfigError> {
    let root = parse_config(contents)?;
    let children = root.children();
    match children.len() {
        0 => Err(KdlConfigError::custom(
            "KDL Document",
            "expected a single top-level node, found none",
        )),
        1 => {
            let node = &children[0];
            let value = T::from_node(node, config)?;
            let canonical = to_kdl(&value, &node.name);
            Ok(RoundTrip {
                value,
                original: contents.to_string(),
                canonical,
                node_name: node.name.clone(),
                dirty: false,
            })
        }
        count => Err(KdlConfigError::custom(
            "KDL Document",
            format!("expected a single top-level node, found {count}"),
        )),
    }
}
