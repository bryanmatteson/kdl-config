use kdl_config::merge::{DeepMerge, PartialConfig};
use kdl_config::{KdlMerge, KdlPartial};

#[derive(Debug, Clone, PartialEq, KdlMerge)]
struct SearchSettings {
    timeout_ms: Option<u64>,
    tags: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, KdlMerge)]
struct MergeConfig {
    #[kdl(merge = "keep")]
    keep: Option<String>,
    #[kdl(merge = "replace")]
    replace: Option<String>,
    nested: SearchSettings,
    #[kdl(merge = "append")]
    pools: Vec<String>,
}

#[test]
fn kdl_merge_respects_field_policies() {
    let base = MergeConfig {
        keep: Some("base".to_string()),
        replace: Some("base".to_string()),
        nested: SearchSettings {
            timeout_ms: Some(100),
            tags: vec!["alpha".to_string()],
        },
        pools: vec!["one".to_string()],
    };
    let overlay = MergeConfig {
        keep: Some("overlay".to_string()),
        replace: Some("overlay".to_string()),
        nested: SearchSettings {
            timeout_ms: None,
            tags: vec!["beta".to_string()],
        },
        pools: vec!["two".to_string()],
    };

    let merged = base.deep_merge(overlay);
    assert_eq!(merged.keep, Some("base".to_string()));
    assert_eq!(merged.replace, Some("overlay".to_string()));
    assert_eq!(merged.nested.timeout_ms, Some(100));
    assert_eq!(merged.nested.tags, vec!["beta".to_string()]);
    assert_eq!(merged.pools, vec!["one".to_string(), "two".to_string()]);
}

fn prefer_longer(left: Option<String>, right: Option<String>) -> Option<String> {
    match (left, right) {
        (Some(a), Some(b)) => {
            if b.len() >= a.len() {
                Some(b)
            } else {
                Some(a)
            }
        }
        (None, Some(b)) => Some(b),
        (Some(a), None) => Some(a),
        (None, None) => None,
    }
}

#[derive(Debug, Clone, PartialEq, KdlMerge)]
struct CustomMergePolicy {
    #[kdl(merge(func = "prefer_longer"))]
    label: Option<String>,
}

#[test]
fn kdl_merge_supports_custom_merge_functions() {
    let base = CustomMergePolicy {
        label: Some("short".to_string()),
    };
    let overlay = CustomMergePolicy {
        label: Some("significantly-longer".to_string()),
    };

    let merged = base.deep_merge(overlay);
    assert_eq!(merged.label, Some("significantly-longer".to_string()));
}

#[derive(Debug, Clone, PartialEq, KdlMerge)]
struct AppConfig {
    name: String,
    retries: u32,
    search: SearchSettings,
}

#[derive(Debug, Clone, PartialEq, KdlPartial)]
#[kdl(partial_for = "AppConfig")]
struct AppConfigPartial {
    name: Option<String>,
    retries: Option<u32>,
    search: Option<SearchSettings>,
}

#[test]
fn kdl_partial_applies_option_overlay_to_base_type() {
    let base = AppConfig {
        name: "stag".to_string(),
        retries: 1,
        search: SearchSettings {
            timeout_ms: Some(200),
            tags: vec!["stable".to_string()],
        },
    };
    let overlay = AppConfigPartial {
        name: None,
        retries: Some(5),
        search: Some(SearchSettings {
            timeout_ms: None,
            tags: vec!["fast".to_string()],
        }),
    };

    let merged = overlay.apply_to(base);
    assert_eq!(merged.name, "stag".to_string());
    assert_eq!(merged.retries, 5);
    assert_eq!(merged.search.timeout_ms, Some(200));
    assert_eq!(merged.search.tags, vec!["fast".to_string()]);
}
