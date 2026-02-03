use std::collections::HashSet;

use crate::config::FlagStyle;
use crate::config::ConflictPolicy;
use crate::error::{KdlConfigError, Placement};
use crate::types::{Node, Value};

#[derive(Debug, Default)]
pub struct UsedKeys {
    attrs: HashSet<String>,
    children: HashSet<String>,
}

impl UsedKeys {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn mark_attr(&mut self, key: &str) {
        self.attrs.insert(key.to_string());
    }

    pub fn mark_child(&mut self, name: &str) {
        self.children.insert(name.to_string());
    }

    pub fn check_unknowns(&self, node: &Node, struct_name: &str) -> Result<(), KdlConfigError> {
        for key in node.attrs().keys() {
            if !self.attrs.contains(key) {
                return Err(KdlConfigError::unknown_attribute(struct_name, key));
            }
        }

        for child in node.children() {
            if !self.children.contains(&child.name) {
                return Err(KdlConfigError::unknown_child(struct_name, &child.name));
            }
        }

        Ok(())
    }
}

pub fn flag_names_for_key(key: &str, style: FlagStyle) -> (String, String) {
    match style {
        FlagStyle::ValueNo => (key.to_string(), format!("no-{key}")),
        FlagStyle::WithWithout => (format!("with-{key}"), format!("without-{key}")),
        FlagStyle::Both => (key.to_string(), format!("no-{key}")),
    }
}

pub fn find_flag(
    node: &Node,
    pos: &str,
    neg: &str,
    struct_name: &str,
    field_name: &str,
) -> Result<Option<bool>, KdlConfigError> {
    let mut found_pos = false;
    let mut found_neg = false;

    for arg in node.args() {
        if let Value::String(s) = arg {
            if s == pos {
                found_pos = true;
            } else if s == neg {
                found_neg = true;
            }
        }
    }

    match (found_pos, found_neg) {
        (true, true) => Err(KdlConfigError::conflicting_flags(struct_name, field_name, pos, neg)),
        (true, false) => Ok(Some(true)),
        (false, true) => Ok(Some(false)),
        (false, false) => Ok(None),
    }
}

pub fn find_flag_with_style(
    node: &Node,
    key: &str,
    style: FlagStyle,
    custom_pos: Option<String>,
    custom_neg: Option<String>,
    struct_name: &str,
    field_name: &str,
) -> Result<Option<bool>, KdlConfigError> {
    if custom_pos.is_some() || custom_neg.is_some() {
        let pos = custom_pos.ok_or_else(|| {
            KdlConfigError::custom(struct_name, "flag override requires both flag and neg_flag")
        })?;
        let neg = custom_neg.ok_or_else(|| {
            KdlConfigError::custom(struct_name, "flag override requires both flag and neg_flag")
        })?;
        return find_flag(node, &pos, &neg, struct_name, field_name);
    }

    let (pos1, neg1) = match style {
        FlagStyle::WithWithout => (format!("with-{}", key), format!("without-{}", key)),
        _ => (key.to_string(), format!("no-{}", key)),
    };

    if style != FlagStyle::Both {
        return find_flag(node, &pos1, &neg1, struct_name, field_name);
    }

    let res1 = find_flag(node, &pos1, &neg1, struct_name, field_name)?;
    let (pos2, neg2) = (format!("with-{}", key), format!("without-{}", key));
    let res2 = find_flag(node, &pos2, &neg2, struct_name, field_name)?;

    match (res1, res2) {
        (Some(_), Some(_)) => Err(KdlConfigError::ambiguous(
            struct_name,
            field_name,
            key,
            Placement::AttrFlag,
            "multiple flag candidates",
        )),
        (Some(v), None) => Ok(Some(v)),
        (None, Some(v)) => Ok(Some(v)),
        (None, None) => Ok(None),
    }
}

pub fn expect_arg<'a>(
    node: &'a Node,
    index: usize,
    struct_name: &str,
    field_name: &str,
) -> Result<&'a Value, KdlConfigError> {
    node.arg(index).ok_or_else(|| {
        KdlConfigError::missing_required(
            struct_name,
            field_name,
            format!("arg[{index}]"),
            Placement::AttrPositional,
        )
    })
}

pub fn expect_attr<'a>(
    node: &'a Node,
    key: &str,
    struct_name: &str,
    field_name: &str,
) -> Result<&'a Value, KdlConfigError> {
    node.attr(key).ok_or_else(|| {
        KdlConfigError::missing_required(struct_name, field_name, key, Placement::AttrKeyed)
    })
}

pub fn resolve_scalar<T>(
    policy: ConflictPolicy,
    current: &mut Option<T>,
    candidate: T,
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
    placement: Placement,
) -> Result<(), KdlConfigError> {
    match policy {
        ConflictPolicy::Error => {
            if current.is_some() {
                return Err(KdlConfigError::ambiguous(
                    struct_name,
                    field_name,
                    kdl_key,
                    placement,
                    "multiple candidates",
                ));
            }
            *current = Some(candidate);
        }
        ConflictPolicy::First => {
            if current.is_none() {
                *current = Some(candidate);
            }
        }
        ConflictPolicy::Last => {
            *current = Some(candidate);
        }
        ConflictPolicy::Append => {
            return Err(KdlConfigError::ambiguous(
                struct_name,
                field_name,
                kdl_key,
                placement,
                "append conflict policy is only valid for Vec fields",
            ));
        }
    }
    Ok(())
}

pub fn resolve_vec<T>(
    policy: ConflictPolicy,
    current: &mut Option<Vec<T>>,
    mut candidate: Vec<T>,
    struct_name: &str,
    field_name: &str,
    kdl_key: &str,
    placement: Placement,
) -> Result<(), KdlConfigError> {
    match policy {
        ConflictPolicy::Error => {
            if current.is_some() {
                return Err(KdlConfigError::ambiguous(
                    struct_name,
                    field_name,
                    kdl_key,
                    placement,
                    "multiple candidates",
                ));
            }
            *current = Some(candidate);
        }
        ConflictPolicy::First => {
            if current.is_none() {
                *current = Some(candidate);
            }
        }
        ConflictPolicy::Last => {
            *current = Some(candidate);
        }
        ConflictPolicy::Append => {
            if let Some(existing) = current.as_mut() {
                existing.append(&mut candidate);
            } else {
                *current = Some(candidate);
            }
        }
    }
    Ok(())
}
