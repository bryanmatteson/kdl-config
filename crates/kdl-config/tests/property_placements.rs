use kdl_config::parse_str;
use kdl_config_derive::KdlNode;
use proptest::prelude::*;

fn parse_named<T: kdl_config::KdlDecode>(
    kdl: &str,
    _name: &str,
) -> Result<T, kdl_config::KdlConfigError> {
    parse_str(kdl)
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct PlacementFirst {
    #[kdl(attr, positional = 0, value, conflict = "first")]
    value: i64,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct PlacementLast {
    #[kdl(attr, positional = 0, value, conflict = "last")]
    value: i64,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct PlacementError {
    #[kdl(attr, positional = 0, value, conflict = "error")]
    value: i64,
}

fn build_kdl(attr: Option<i64>, positional: Option<i64>, child: Option<i64>) -> String {
    let mut header = String::from("config");
    if let Some(pos) = positional {
        header.push(' ');
        header.push_str(&pos.to_string());
    }
    if let Some(attr_val) = attr {
        header.push(' ');
        header.push_str(&format!("value={}", attr_val));
    }

    if let Some(child_val) = child {
        format!("{} {{\n  value {}\n}}", header, child_val)
    } else {
        header
    }
}

proptest! {
    #[test]
    fn placement_first_follows_order(
        attr in proptest::option::of(any::<i64>()),
        positional in proptest::option::of(any::<i64>()),
        child in proptest::option::of(any::<i64>()),
    ) {
        prop_assume!(attr.is_some() || positional.is_some() || child.is_some());
        let kdl = build_kdl(attr, positional, child);
        let parsed = parse_named::<PlacementFirst>(&kdl, "config").unwrap();

        let expected = if let Some(val) = attr {
            val
        } else if let Some(val) = positional {
            val
        } else {
            child.unwrap()
        };

        prop_assert_eq!(parsed.value, expected);
    }

    #[test]
    fn placement_last_follows_order(
        attr in proptest::option::of(any::<i64>()),
        positional in proptest::option::of(any::<i64>()),
        child in proptest::option::of(any::<i64>()),
    ) {
        prop_assume!(attr.is_some() || positional.is_some() || child.is_some());
        let kdl = build_kdl(attr, positional, child);
        let parsed = parse_named::<PlacementLast>(&kdl, "config").unwrap();

        let expected = if let Some(val) = child {
            val
        } else if let Some(val) = positional {
            val
        } else {
            attr.unwrap()
        };

        prop_assert_eq!(parsed.value, expected);
    }

    #[test]
    fn placement_error_detects_conflicts(
        attr in proptest::option::of(any::<i64>()),
        positional in proptest::option::of(any::<i64>()),
        child in proptest::option::of(any::<i64>()),
    ) {
        prop_assume!(attr.is_some() || positional.is_some() || child.is_some());
        let kdl = build_kdl(attr, positional, child);
        let candidate_count = attr.is_some() as u8 + positional.is_some() as u8 + child.is_some() as u8;
        let result = parse_named::<PlacementError>(&kdl, "config");
        if candidate_count > 1 {
            prop_assert!(result.is_err());
        } else {
            prop_assert!(result.is_ok());
        }
    }
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct BoolConflictFirst {
    #[kdl(attr, keyed, flag, conflict = "first")]
    enabled: bool,
}

#[derive(Debug, PartialEq, KdlNode)]
#[kdl(node = "config")]
struct BoolConflictLast {
    #[kdl(attr, keyed, flag, conflict = "last")]
    enabled: bool,
}

fn build_bool_kdl(explicit: Option<bool>, flag: Option<bool>) -> String {
    let mut header = String::from("config");
    if let Some(flag_val) = flag {
        header.push(' ');
        if flag_val {
            header.push_str("enabled");
        } else {
            header.push_str("no-enabled");
        }
    }
    if let Some(explicit_val) = explicit {
        header.push(' ');
        header.push_str(&format!(
            "enabled=#{}",
            if explicit_val { "true" } else { "false" }
        ));
    }
    header
}

proptest! {
    #[test]
    fn bool_conflict_first_prefers_explicit(
        explicit in proptest::option::of(any::<bool>()),
        flag in proptest::option::of(any::<bool>()),
    ) {
        prop_assume!(explicit.is_some() || flag.is_some());
        let kdl = build_bool_kdl(explicit, flag);
        let parsed = parse_named::<BoolConflictFirst>(&kdl, "config").unwrap();
        let expected = if let Some(explicit_val) = explicit {
            explicit_val
        } else {
            flag.unwrap()
        };
        prop_assert_eq!(parsed.enabled, expected);
    }

    #[test]
    fn bool_conflict_last_prefers_flag(
        explicit in proptest::option::of(any::<bool>()),
        flag in proptest::option::of(any::<bool>()),
    ) {
        prop_assume!(explicit.is_some() || flag.is_some());
        let kdl = build_bool_kdl(explicit, flag);
        let parsed = parse_named::<BoolConflictLast>(&kdl, "config").unwrap();
        let expected = if let Some(flag_val) = flag {
            flag_val
        } else {
            explicit.unwrap()
        };
        prop_assert_eq!(parsed.enabled, expected);
    }
}
