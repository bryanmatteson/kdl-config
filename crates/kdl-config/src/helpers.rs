use std::collections::HashSet;

use crate::config::{ConflictPolicy, FlagStyle};
use crate::error::{ErrorKind, KdlConfigError, Placement};
use crate::node_ext::KdlNodeExt;
use crate::KdlDecode;
use kdl::{KdlNode, KdlValue};

#[derive(Debug, Default)]
pub struct Claims {
    attrs: HashSet<String>,
    args: Vec<bool>,
    children: Vec<bool>,
}

impl Claims {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn claim_attr(&mut self, key: &str) {
        self.attrs.insert(key.to_string());
    }

    pub fn claim_arg(&mut self, index: usize) {
        if index >= self.args.len() {
            self.args.resize(index + 1, false);
        }
        self.args[index] = true;
    }

    pub fn claim_child(&mut self, index: usize) {
        if index >= self.children.len() {
            self.children.resize(index + 1, false);
        }
        self.children[index] = true;
    }

    pub fn check_unknowns(&self, node: &KdlNode, struct_name: &str) -> Result<(), KdlConfigError> {
        let mut arg_index = 0usize;
        for entry in node.entries() {
            if let Some(name) = entry.name() {
                let key = name.value();
                if !self.attrs.contains(key) {
                    return Err(KdlConfigError::unknown_attribute(struct_name, key));
                }
            } else {
                if self.args.get(arg_index).copied().unwrap_or(false) {
                    arg_index += 1;
                    continue;
                }
                return Err(KdlConfigError::unknown_argument(struct_name, arg_index));
            }
        }

        for (index, _child) in node.iter_children().enumerate() {
            if !self.children.get(index).copied().unwrap_or(false) {
                return Err(KdlConfigError::unknown_child(struct_name, _child.name().value()));
            }
        }

        Ok(())
    }
}

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

    pub fn check_unknowns(&self, node: &KdlNode, struct_name: &str) -> Result<(), KdlConfigError> {
        let mut arg_index = 0usize;
        for entry in node.entries() {
            if let Some(name) = entry.name() {
                let key = name.value();
                if !self.attrs.contains(key) {
                    return Err(KdlConfigError::unknown_attribute(struct_name, key));
                }
            } else {
                if !self.args.contains(&arg_index) {
                    if let KdlValue::String(token) = entry.value() {
                        if self.flags.contains(token) {
                            arg_index += 1;
                            continue;
                        }
                    }
                    return Err(KdlConfigError::unknown_argument(struct_name, arg_index));
                }
                arg_index += 1;
            }
        }

        for child in node.iter_children() {
            let name = child.name().value();
            if !self.children.contains(name) {
                return Err(KdlConfigError::unknown_child(struct_name, name));
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
    node: &KdlNode,
    pos: &str,
    neg: &str,
    struct_name: &str,
    field_name: &str,
) -> Result<Option<bool>, KdlConfigError> {
    find_flag_marked(node, pos, neg, struct_name, field_name, None)
}

fn find_flag_marked(
    node: &KdlNode,
    pos: &str,
    neg: &str,
    struct_name: &str,
    field_name: &str,
    mut used_keys: Option<&mut UsedKeys>,
) -> Result<Option<bool>, KdlConfigError> {
    let mut found_pos = false;
    let mut found_neg = false;

    for arg in node.args() {
        if let KdlValue::String(s) = arg {
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
    node: &KdlNode,
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

/// Scan flag tokens and return the resolved bool plus arg indices (if any).
pub fn scan_flag_with_style(
    node: &KdlNode,
    key: &str,
    style: FlagStyle,
    custom_pos: Option<String>,
    custom_neg: Option<String>,
    struct_name: &str,
    field_name: &str,
) -> Result<Option<(bool, Vec<usize>)>, KdlConfigError> {
    if custom_pos.is_some() || custom_neg.is_some() {
        let pos = custom_pos.ok_or_else(|| {
            KdlConfigError::custom(struct_name, "flag override requires both flag and neg_flag")
        })?;
        let neg = custom_neg.ok_or_else(|| {
            KdlConfigError::custom(struct_name, "flag override requires both flag and neg_flag")
        })?;
        return scan_flag_tokens(node, &pos, &neg, None, None, struct_name, field_name);
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

        return scan_flag_tokens(
            node,
            &pos1,
            &neg1,
            Some((&other_pos, &other_neg)),
            None,
            struct_name,
            field_name,
        );
    }

    let (pos2, neg2) = (format!("with-{}", key), format!("without-{}", key));
    scan_flag_tokens(
        node,
        &pos1,
        &neg1,
        None,
        Some((&pos2, &neg2)),
        struct_name,
        field_name,
    )
}

fn scan_flag_tokens(
    node: &KdlNode,
    pos1: &str,
    neg1: &str,
    other_style: Option<(&str, &str)>,
    alt_style: Option<(&str, &str)>,
    struct_name: &str,
    field_name: &str,
) -> Result<Option<(bool, Vec<usize>)>, KdlConfigError> {
    let mut pos_indices: Vec<usize> = Vec::new();
    let mut neg_indices: Vec<usize> = Vec::new();
    let mut arg_index = 0usize;

    for entry in node.entries() {
        if entry.name().is_some() {
            continue;
        }
        let value = entry.value();
        if let KdlValue::String(s) = value {
            if let Some((other_pos, other_neg)) = other_style {
                if s == other_pos || s == other_neg {
                    return Err(KdlConfigError::incompatible_placement(
                        struct_name,
                        field_name,
                        "flag token is not valid for the configured flag style",
                    ));
                }
            }

            if s == pos1 {
                pos_indices.push(arg_index);
            } else if s == neg1 {
                neg_indices.push(arg_index);
            } else if let Some((alt_pos, alt_neg)) = alt_style {
                if s == alt_pos {
                    pos_indices.push(arg_index);
                } else if s == alt_neg {
                    neg_indices.push(arg_index);
                }
            }
        }
        arg_index += 1;
    }

    match (pos_indices.is_empty(), neg_indices.is_empty()) {
        (true, true) => Ok(None),
        (false, false) => Err(KdlConfigError::conflicting_flags(
            struct_name,
            field_name,
            pos1,
            neg1,
        )),
        (false, true) => Ok(Some((true, pos_indices))),
        (true, false) => Ok(Some((false, neg_indices))),
    }
}

pub fn find_flag_with_style_marked(
    node: &KdlNode,
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
            if let KdlValue::String(s) = arg {
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
        if let KdlValue::String(s) = arg {
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

pub fn parse_flatten<T: KdlDecode>(node: &KdlNode, ctx: &crate::DecodeContext) -> Result<T, KdlConfigError> {
    match T::decode(node, ctx) {
        Ok(val) => Ok(val),
        Err(err) => match &err.kind {
            ErrorKind::NodeNameMismatch { expected, .. } => {
                let mut clone = node.clone();
                clone.set_name(expected.clone());
                T::decode(&clone, ctx)
            }
            _ => Err(err),
        },
    }
}

pub fn expect_arg<'a>(
    node: &'a KdlNode,
    index: usize,
    struct_name: &str,
    field_name: &str,
) -> Result<&'a KdlValue, KdlConfigError> {
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
    node: &'a KdlNode,
    key: &str,
    struct_name: &str,
    field_name: &str,
) -> Result<&'a KdlValue, KdlConfigError> {
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
