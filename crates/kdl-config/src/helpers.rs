use std::collections::HashSet;

use crate::config::{ConflictPolicy, FlagStyle, ParseConfig};
use crate::error::{ErrorKind, KdlConfigError, Placement};
use crate::types::{Node, Value};
use crate::KdlParse;

#[derive(Debug, Default)]
pub struct UsedKeys {
    attrs: HashSet<String>,
    children: HashSet<String>,
    args: HashSet<usize>,
    flags: HashSet<String>,
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

    pub fn mark_arg(&mut self, index: usize) {
        self.args.insert(index);
    }

    pub fn mark_flag(&mut self, token: &str) {
        self.flags.insert(token.to_string());
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

        for (index, arg) in node.args().iter().enumerate() {
            if self.args.contains(&index) {
                continue;
            }
            if let Value::String(token) = arg {
                if self.flags.contains(token) {
                    continue;
                }
            }
            return Err(KdlConfigError::unknown_argument(struct_name, index));
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
    find_flag_marked(node, pos, neg, struct_name, field_name, None)
}

fn find_flag_marked(
    node: &Node,
    pos: &str,
    neg: &str,
    struct_name: &str,
    field_name: &str,
    mut used_keys: Option<&mut UsedKeys>,
) -> Result<Option<bool>, KdlConfigError> {
    let mut found_pos = false;
    let mut found_neg = false;

    for arg in node.args() {
        if let Value::String(s) = arg {
            if s == pos {
                found_pos = true;
                if let Some(keys) = used_keys.as_deref_mut() {
                    keys.mark_flag(s);
                }
            } else if s == neg {
                found_neg = true;
                if let Some(keys) = used_keys.as_deref_mut() {
                    keys.mark_flag(s);
                }
            }
        }
    }

    match (found_pos, found_neg) {
        (true, true) => Err(KdlConfigError::conflicting_flags(
            struct_name,
            field_name,
            pos,
            neg,
        )),
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
    find_flag_with_style_marked(
        node,
        key,
        style,
        custom_pos,
        custom_neg,
        struct_name,
        field_name,
        None,
    )
}

pub fn find_flag_with_style_marked(
    node: &Node,
    key: &str,
    style: FlagStyle,
    custom_pos: Option<String>,
    custom_neg: Option<String>,
    struct_name: &str,
    field_name: &str,
    mut used_keys: Option<&mut UsedKeys>,
) -> Result<Option<bool>, KdlConfigError> {
    if custom_pos.is_some() || custom_neg.is_some() {
        let pos = custom_pos.ok_or_else(|| {
            KdlConfigError::custom(struct_name, "flag override requires both flag and neg_flag")
        })?;
        let neg = custom_neg.ok_or_else(|| {
            KdlConfigError::custom(struct_name, "flag override requires both flag and neg_flag")
        })?;
        return find_flag_marked(node, &pos, &neg, struct_name, field_name, used_keys);
    }

    let (pos1, neg1) = match style {
        FlagStyle::WithWithout => (format!("with-{}", key), format!("without-{}", key)),
        _ => (key.to_string(), format!("no-{}", key)),
    };

    if style != FlagStyle::Both {
        let (other_pos, other_neg) = match style {
            FlagStyle::WithWithout => (key.to_string(), format!("no-{}", key)),
            FlagStyle::ValueNo => (format!("with-{}", key), format!("without-{}", key)),
            FlagStyle::Both => unreachable!(),
        };

        for arg in node.args() {
            if let Value::String(s) = arg {
                if s == &other_pos || s == &other_neg {
                    return Err(KdlConfigError::incompatible_placement(
                        struct_name,
                        field_name,
                        "flag token is not valid for the configured flag style",
                    ));
                }
            }
        }

        return find_flag_marked(node, &pos1, &neg1, struct_name, field_name, used_keys);
    }

    let (pos2, neg2) = (format!("with-{}", key), format!("without-{}", key));
    let mut found_pos1 = false;
    let mut found_neg1 = false;
    let mut found_pos2 = false;
    let mut found_neg2 = false;

    for arg in node.args() {
        if let Value::String(s) = arg {
            if s == &pos1 {
                found_pos1 = true;
                if let Some(keys) = used_keys.as_deref_mut() {
                    keys.mark_flag(s);
                }
            } else if s == &neg1 {
                found_neg1 = true;
                if let Some(keys) = used_keys.as_deref_mut() {
                    keys.mark_flag(s);
                }
            } else if s == &pos2 {
                found_pos2 = true;
                if let Some(keys) = used_keys.as_deref_mut() {
                    keys.mark_flag(s);
                }
            } else if s == &neg2 {
                found_neg2 = true;
                if let Some(keys) = used_keys.as_deref_mut() {
                    keys.mark_flag(s);
                }
            }
        }
    }

    let res1 = match (found_pos1, found_neg1) {
        (true, true) => {
            return Err(KdlConfigError::conflicting_flags(
                struct_name,
                field_name,
                &pos1,
                &neg1,
            ));
        }
        (true, false) => Some(true),
        (false, true) => Some(false),
        (false, false) => None,
    };

    let res2 = match (found_pos2, found_neg2) {
        (true, true) => {
            return Err(KdlConfigError::conflicting_flags(
                struct_name,
                field_name,
                &pos2,
                &neg2,
            ));
        }
        (true, false) => Some(true),
        (false, true) => Some(false),
        (false, false) => None,
    };

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

pub fn parse_flatten<T: KdlParse>(node: &Node, config: &ParseConfig) -> Result<T, KdlConfigError> {
    match T::from_node(node, config) {
        Ok(val) => Ok(val),
        Err(err) => match &err.kind {
            ErrorKind::NodeNameMismatch { expected, .. } => {
                let mut clone = node.clone();
                clone.name = expected.clone();
                T::from_node(&clone, config)
            }
            _ => Err(err),
        },
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
