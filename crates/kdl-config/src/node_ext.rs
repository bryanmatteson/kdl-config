use std::collections::HashMap;

use kdl::{KdlEntry, KdlNode, KdlValue};

use crate::types::Modifier;

pub trait KdlNodeExt {
    fn name_str(&self) -> &str;
    fn base_name(&self) -> &str;
    fn modifier(&self) -> Modifier;
    fn args(&self) -> Vec<&KdlValue>;
    fn arg(&self, index: usize) -> Option<&KdlValue>;
    fn attrs(&self) -> HashMap<String, Vec<&KdlValue>>;
    fn attr_values(&self, key: &str) -> Option<Vec<&KdlValue>>;
    fn attr(&self, key: &str) -> Option<&KdlValue>;
    fn children(&self) -> Vec<&KdlNode>;
    fn children_named<'a>(&'a self, name: &'a str) -> impl Iterator<Item = &'a KdlNode> + 'a;
    fn without_attr(&self, key: &str) -> KdlNode;
    fn without_arg(&self, index: usize) -> KdlNode;
    fn without_first_arg(&self) -> KdlNode {
        self.without_arg(0)
    }
}

impl KdlNodeExt for KdlNode {
    fn name_str(&self) -> &str {
        self.base_name()
    }

    fn base_name(&self) -> &str {
        let ident = self.name();
        let raw = ident.value();
        let repr = ident.repr();
        let is_quoted = repr
            .map(|repr| {
                repr.starts_with('"')
                    || repr.starts_with('#')
                    || repr.starts_with('r')
                    || repr.is_empty()
            })
            .unwrap_or(false);
        if is_quoted {
            return raw;
        }
        match raw.as_bytes().first().copied() {
            Some(b'+') | Some(b'-') | Some(b'!') | Some(b'~') => &raw[1..],
            _ => raw,
        }
    }

    fn modifier(&self) -> Modifier {
        let ident = self.name();
        let raw = ident.value();
        let repr = ident.repr();
        let is_quoted = repr
            .map(|repr| {
                repr.starts_with('"')
                    || repr.starts_with('#')
                    || repr.starts_with('r')
                    || repr.is_empty()
            })
            .unwrap_or(false);
        if is_quoted {
            return Modifier::Inherit;
        }
        match raw.as_bytes().first().copied() {
            Some(b'+') => Modifier::Append,
            Some(b'-') => Modifier::Remove,
            Some(b'!') => Modifier::Replace,
            Some(b'~') => Modifier::Flatten,
            _ => Modifier::Inherit,
        }
    }

    fn args(&self) -> Vec<&KdlValue> {
        self.entries()
            .iter()
            .filter(|entry| entry.name().is_none())
            .map(|entry| entry.value())
            .collect()
    }

    fn arg(&self, index: usize) -> Option<&KdlValue> {
        let mut pos = 0usize;
        for entry in self.entries() {
            if entry.name().is_none() {
                if pos == index {
                    return Some(entry.value());
                }
                pos += 1;
            }
        }
        None
    }

    fn attrs(&self) -> HashMap<String, Vec<&KdlValue>> {
        let mut map: HashMap<String, Vec<&KdlValue>> = HashMap::new();
        for entry in self.entries() {
            if let Some(name) = entry.name() {
                map.entry(name.value().to_string())
                    .or_default()
                    .push(entry.value());
            }
        }
        map
    }

    fn attr_values(&self, key: &str) -> Option<Vec<&KdlValue>> {
        let mut values = Vec::new();
        for entry in self.entries() {
            if let Some(name) = entry.name() {
                if name.value() == key {
                    values.push(entry.value());
                }
            }
        }
        if values.is_empty() {
            None
        } else {
            Some(values)
        }
    }

    fn attr(&self, key: &str) -> Option<&KdlValue> {
        for entry in self.entries() {
            if let Some(name) = entry.name() {
                if name.value() == key {
                    return Some(entry.value());
                }
            }
        }
        None
    }

    fn children(&self) -> Vec<&KdlNode> {
        self.iter_children().collect()
    }

    fn children_named<'a>(&'a self, name: &'a str) -> impl Iterator<Item = &'a KdlNode> + 'a {
        self.iter_children()
            .filter(move |child| child.base_name() == name)
    }

    fn without_attr(&self, key: &str) -> KdlNode {
        let mut clone = self.clone();
        clone
            .entries_mut()
            .retain(|entry| entry.name().map(|name| name.value() != key).unwrap_or(true));
        clone
    }

    fn without_arg(&self, index: usize) -> KdlNode {
        let mut clone = self.clone();
        let mut pos = 0usize;
        let mut remove_idx: Option<usize> = None;
        for (idx, entry) in clone.entries().iter().enumerate() {
            if entry.name().is_none() {
                if pos == index {
                    remove_idx = Some(idx);
                    break;
                }
                pos += 1;
            }
        }
        if let Some(idx) = remove_idx {
            clone.entries_mut().remove(idx);
        }
        clone
    }
}

pub fn arg_entry_index(node: &KdlNode, index: usize) -> Option<usize> {
    let mut pos = 0usize;
    for (idx, entry) in node.entries().iter().enumerate() {
        if entry.name().is_none() {
            if pos == index {
                return Some(idx);
            }
            pos += 1;
        }
    }
    None
}

pub fn attr_entry_indices(node: &KdlNode, key: &str) -> Vec<usize> {
    let mut indices = Vec::new();
    for (idx, entry) in node.entries().iter().enumerate() {
        if let Some(name) = entry.name() {
            if name.value() == key {
                indices.push(idx);
            }
        }
    }
    indices
}

pub fn remove_entry_indices(node: &mut KdlNode, indices: &[usize]) {
    let mut sorted = indices.to_vec();
    sorted.sort_unstable_by(|a, b| b.cmp(a));
    for idx in sorted {
        if idx < node.entries().len() {
            node.entries_mut().remove(idx);
        }
    }
}

pub fn remove_attr_entries(node: &mut KdlNode, key: &str) {
    let indices = attr_entry_indices(node, key);
    remove_entry_indices(node, &indices);
}

pub fn update_or_insert_attr(node: &mut KdlNode, key: &str, value: KdlValue) {
    if let Some(idx) = attr_entry_indices(node, key).into_iter().next() {
        update_entry_value(&mut node.entries_mut()[idx], value);
    } else {
        let mut entry = KdlEntry::new(value);
        entry.set_name(Some(key.to_string()));
        node.entries_mut().push(entry);
    }
}

pub fn update_or_insert_positional(node: &mut KdlNode, pos: usize, value: KdlValue) {
    if let Some(idx) = arg_entry_index(node, pos) {
        update_entry_value(&mut node.entries_mut()[idx], value);
        return;
    }

    let mut last_positional = None;
    for (idx, entry) in node.entries().iter().enumerate() {
        if entry.name().is_none() {
            last_positional = Some(idx);
        }
    }
    let insert_at = match last_positional {
        Some(last_idx) => last_idx + 1,
        None => 0,
    };
    node.entries_mut().insert(insert_at, KdlEntry::new(value));
}

pub fn remove_positional_entry(node: &mut KdlNode, pos: usize) {
    if let Some(idx) = arg_entry_index(node, pos) {
        node.entries_mut().remove(idx);
    }
}

pub fn update_entry_value(entry: &mut KdlEntry, value: KdlValue) {
    entry.set_value(value.clone());
    if let Some(fmt) = entry.format_mut() {
        fmt.value_repr = value.to_string();
    }
}
