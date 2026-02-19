#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefaultPlacement {
    Exhaustive,
    Attr,
    Value,
    Child,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoolMode {
    PresenceAndValue,
    ValueOnly,
    PresenceOnly,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlagStyle {
    Both,
    ValueNo,
    WithWithout,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConflictPolicy {
    Error,
    First,
    Last,
    Append,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RootMode {
    Strict,
    WrapExpectedNode { name: String },
}

impl Default for RootMode {
    fn default() -> Self {
        Self::Strict
    }
}

#[derive(Debug, Clone)]
pub struct ParseConfig {
    pub default_placement: DefaultPlacement,
    pub default_bool: BoolMode,
    pub default_flag_style: FlagStyle,
    pub default_conflict: ConflictPolicy,
    pub deny_unknown: bool,
    pub root_mode: RootMode,
}

impl Default for ParseConfig {
    fn default() -> Self {
        Self {
            default_placement: DefaultPlacement::Exhaustive,
            default_bool: BoolMode::PresenceAndValue,
            default_flag_style: FlagStyle::Both,
            default_conflict: ConflictPolicy::Error,
            deny_unknown: false,
            root_mode: RootMode::Strict,
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct StructOverrides {
    pub default_placement: Option<DefaultPlacement>,
    pub default_bool: Option<BoolMode>,
    pub default_flag_style: Option<FlagStyle>,
    pub default_conflict: Option<ConflictPolicy>,
    pub deny_unknown: Option<bool>,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct FieldOverrides {
    pub bool_mode: Option<BoolMode>,
    pub flag_style: Option<FlagStyle>,
    pub conflict: Option<ConflictPolicy>,
}

#[derive(Debug, Clone, Copy)]
pub struct EffectiveConfig {
    pub default_placement: DefaultPlacement,
    pub bool_mode: BoolMode,
    pub flag_style: FlagStyle,
    pub conflict: ConflictPolicy,
    pub deny_unknown: bool,
}

pub fn resolve_struct(config: &ParseConfig, overrides: StructOverrides) -> EffectiveConfig {
    EffectiveConfig {
        default_placement: overrides
            .default_placement
            .unwrap_or(config.default_placement),
        bool_mode: overrides.default_bool.unwrap_or(config.default_bool),
        flag_style: overrides
            .default_flag_style
            .unwrap_or(config.default_flag_style),
        conflict: overrides
            .default_conflict
            .unwrap_or(config.default_conflict),
        deny_unknown: overrides.deny_unknown.unwrap_or(config.deny_unknown),
    }
}

pub fn resolve_field(base: &EffectiveConfig, overrides: FieldOverrides) -> EffectiveConfig {
    EffectiveConfig {
        default_placement: base.default_placement,
        bool_mode: overrides.bool_mode.unwrap_or(base.bool_mode),
        flag_style: overrides.flag_style.unwrap_or(base.flag_style),
        conflict: overrides.conflict.unwrap_or(base.conflict),
        deny_unknown: base.deny_unknown,
    }
}
