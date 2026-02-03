use std::fmt;

#[derive(Debug, Clone)]
pub struct KdlConfigError {
    pub struct_name: String,
    pub field_name: Option<String>,
    pub kdl_key: Option<String>,
    pub placement: Placement,
    pub required: bool,
    pub kind: ErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Placement {
    AttrKeyed,
    AttrPositional,
    AttrFlag,
    Value,
    Child,
    Children,
    Registry,
    Unknown,
}

impl fmt::Display for Placement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Placement::AttrKeyed => write!(f, "keyed attribute"),
            Placement::AttrPositional => write!(f, "positional argument"),
            Placement::AttrFlag => write!(f, "flag token"),
            Placement::Value => write!(f, "value node"),
            Placement::Child => write!(f, "child node"),
            Placement::Children => write!(f, "children nodes"),
            Placement::Registry => write!(f, "registry"),
            Placement::Unknown => write!(f, "unknown"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    MissingRequired,
    TooManyValues { count: usize },
    TypeMismatch { expected: &'static str, actual: String },
    OutOfRange { value: String, target_type: &'static str },
    ConflictingFlags { pos: String, neg: String },
    AmbiguousPlacement { details: String },
    NodeNameMismatch { expected: String, actual: String },
    InvalidVariant { value: String, valid_variants: Vec<String> },
    NoMatchingChoice { discriminator: String, valid_choices: Vec<String> },
    UnknownAttribute { key: String },
    UnknownChild { name: String },
    InvalidRegistryKey { reason: String },
    UnknownArgument { index: usize },
    IncompatiblePlacement { reason: String },
    Custom(String),
    Parse(String),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::MissingRequired => write!(f, "required value is missing"),
            ErrorKind::TooManyValues { count } => {
                write!(f, "expected at most 1 value, found {count}")
            }
            ErrorKind::TypeMismatch { expected, actual } => {
                write!(f, "type mismatch: expected {expected}, found {actual}")
            }
            ErrorKind::OutOfRange { value, target_type } => {
                write!(f, "value '{value}' is out of range for {target_type}")
            }
            ErrorKind::ConflictingFlags { pos, neg } => {
                write!(f, "conflicting flags: both '{pos}' and '{neg}' are present")
            }
            ErrorKind::AmbiguousPlacement { details } => write!(f, "ambiguous value: {details}"),
            ErrorKind::NodeNameMismatch { expected, actual } => {
                write!(f, "expected node '{expected}', found '{actual}'")
            }
            ErrorKind::InvalidVariant { value, valid_variants } => {
                write!(f, "invalid variant '{value}', expected one of: {}", valid_variants.join(", "))
            }
            ErrorKind::NoMatchingChoice { discriminator, valid_choices } => {
                write!(f, "no matching choice for '{discriminator}', expected one of: {}", valid_choices.join(", "))
            }
            ErrorKind::UnknownAttribute { key } => write!(f, "unknown attribute '{key}'"),
            ErrorKind::UnknownChild { name } => write!(f, "unknown child '{name}'"),
            ErrorKind::InvalidRegistryKey { reason } => write!(f, "invalid registry key: {reason}"),
            ErrorKind::UnknownArgument { index } => write!(f, "unknown argument at position {index}"),
            ErrorKind::IncompatiblePlacement { reason } => write!(f, "incompatible placement: {reason}"),
            ErrorKind::Custom(msg) => write!(f, "{msg}"),
            ErrorKind::Parse(msg) => write!(f, "parse error: {msg}"),
        }
    }
}

impl fmt::Display for KdlConfigError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error parsing {}", self.struct_name)?;

        if let Some(ref field) = self.field_name {
            write!(f, " field '{field}'")?;
        }

        if let Some(ref key) = self.kdl_key {
            if self.field_name.as_deref() != Some(key.as_str()) {
                write!(f, " (kdl key: '{key}')")?;
            }
        }

        if self.placement != Placement::Unknown {
            write!(f, " as {}", self.placement)?;
        }

        write!(f, ": {}", self.kind)?;

        if self.required && matches!(self.kind, ErrorKind::MissingRequired) {
            write!(f, " (this field is required)")?;
        }

        Ok(())
    }
}

impl std::error::Error for KdlConfigError {}

impl KdlConfigError {
    pub fn missing_required(struct_name: impl Into<String>, field_name: impl Into<String>, kdl_key: impl Into<String>, placement: Placement) -> Self {
        Self {
            struct_name: struct_name.into(),
            field_name: Some(field_name.into()),
            kdl_key: Some(kdl_key.into()),
            placement,
            required: true,
            kind: ErrorKind::MissingRequired,
        }
    }

    pub fn too_many(struct_name: impl Into<String>, field_name: impl Into<String>, kdl_key: impl Into<String>, placement: Placement, count: usize) -> Self {
        Self {
            struct_name: struct_name.into(),
            field_name: Some(field_name.into()),
            kdl_key: Some(kdl_key.into()),
            placement,
            required: false,
            kind: ErrorKind::TooManyValues { count },
        }
    }

    pub fn type_mismatch(struct_name: impl Into<String>, field_name: impl Into<String>, kdl_key: impl Into<String>, placement: Placement, expected: &'static str, actual: impl Into<String>) -> Self {
        Self {
            struct_name: struct_name.into(),
            field_name: Some(field_name.into()),
            kdl_key: Some(kdl_key.into()),
            placement,
            required: true,
            kind: ErrorKind::TypeMismatch { expected, actual: actual.into() },
        }
    }

    pub fn out_of_range(struct_name: impl Into<String>, field_name: impl Into<String>, kdl_key: impl Into<String>, placement: Placement, value: impl Into<String>, target_type: &'static str) -> Self {
        Self {
            struct_name: struct_name.into(),
            field_name: Some(field_name.into()),
            kdl_key: Some(kdl_key.into()),
            placement,
            required: true,
            kind: ErrorKind::OutOfRange { value: value.into(), target_type },
        }
    }

    pub fn conflicting_flags(struct_name: impl Into<String>, field_name: impl Into<String>, pos: impl Into<String>, neg: impl Into<String>) -> Self {
        Self {
            struct_name: struct_name.into(),
            field_name: Some(field_name.into()),
            kdl_key: None,
            placement: Placement::AttrFlag,
            required: false,
            kind: ErrorKind::ConflictingFlags { pos: pos.into(), neg: neg.into() },
        }
    }

    pub fn ambiguous(struct_name: impl Into<String>, field_name: impl Into<String>, kdl_key: impl Into<String>, placement: Placement, details: impl Into<String>) -> Self {
        Self {
            struct_name: struct_name.into(),
            field_name: Some(field_name.into()),
            kdl_key: Some(kdl_key.into()),
            placement,
            required: false,
            kind: ErrorKind::AmbiguousPlacement { details: details.into() },
        }
    }

    pub fn node_name_mismatch(struct_name: impl Into<String>, expected: impl Into<String>, actual: impl Into<String>) -> Self {
        Self {
            struct_name: struct_name.into(),
            field_name: None,
            kdl_key: None,
            placement: Placement::Unknown,
            required: false,
            kind: ErrorKind::NodeNameMismatch { expected: expected.into(), actual: actual.into() },
        }
    }

    pub fn unknown_attribute(struct_name: impl Into<String>, key: impl Into<String>) -> Self {
        let key_string = key.into();
        Self {
            struct_name: struct_name.into(),
            field_name: None,
            kdl_key: Some(key_string.clone()),
            placement: Placement::AttrKeyed,
            required: false,
            kind: ErrorKind::UnknownAttribute { key: key_string },
        }
    }

    pub fn unknown_child(struct_name: impl Into<String>, name: impl Into<String>) -> Self {
        let name_string = name.into();
        Self {
            struct_name: struct_name.into(),
            field_name: None,
            kdl_key: Some(name_string.clone()),
            placement: Placement::Child,
            required: false,
            kind: ErrorKind::UnknownChild { name: name_string },
        }
    }

    pub fn invalid_registry_key(struct_name: impl Into<String>, field_name: impl Into<String>, kdl_key: impl Into<String>, reason: impl Into<String>) -> Self {
        Self {
            struct_name: struct_name.into(),
            field_name: Some(field_name.into()),
            kdl_key: Some(kdl_key.into()),
            placement: Placement::Registry,
            required: false,
            kind: ErrorKind::InvalidRegistryKey { reason: reason.into() },
        }
    }

    pub fn unknown_argument(struct_name: impl Into<String>, index: usize) -> Self {
        Self {
            struct_name: struct_name.into(),
            field_name: None,
            kdl_key: Some(format!("arg[{index}]")),
            placement: Placement::AttrPositional,
            required: false,
            kind: ErrorKind::UnknownArgument { index },
        }
    }

    pub fn invalid_variant(
        struct_name: impl Into<String>,
        value: impl Into<String>,
        valid_variants: Vec<String>,
    ) -> Self {
        Self {
            struct_name: struct_name.into(),
            field_name: Some("variant".into()),
            kdl_key: Some("arg[0]".into()),
            placement: Placement::AttrPositional,
            required: false,
            kind: ErrorKind::InvalidVariant {
                value: value.into(),
                valid_variants,
            },
        }
    }

    pub fn no_matching_choice(
        struct_name: &str,
        discriminator: &str,
        valid_choices: &[&str],
    ) -> Self {
        Self {
            struct_name: struct_name.to_string(),
            field_name: None,
            kdl_key: Some(discriminator.to_string()),
            placement: Placement::Unknown,
            required: true,
            kind: ErrorKind::NoMatchingChoice {
                discriminator: discriminator.to_string(),
                valid_choices: valid_choices.iter().map(|s| s.to_string()).collect(),
            },
        }
    }

    pub fn incompatible_placement(struct_name: impl Into<String>, field_name: impl Into<String>, reason: impl Into<String>) -> Self {
        Self {
            struct_name: struct_name.into(),
            field_name: Some(field_name.into()),
            kdl_key: None,
            placement: Placement::Unknown,
            required: false,
            kind: ErrorKind::IncompatiblePlacement { reason: reason.into() },
        }
    }

    pub fn custom(struct_name: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            struct_name: struct_name.into(),
            field_name: None,
            kdl_key: None,
            placement: Placement::Unknown,
            required: false,
            kind: ErrorKind::Custom(message.into()),
        }
    }
}
