use std::collections::{HashMap, HashSet};

use crate::{BoolMode, ConflictPolicy, DefaultPlacement, FlagStyle, KdlRender};

/// Trait for types that can describe their own KDL schema.
pub trait KdlSchema {
    /// Returns a reference to the schema definition for this type.
    ///
    /// This might be a reference to a registered definition (if it's a struct/enum)
    /// or an inline usage (if it's a primitive).
    fn schema_ref() -> SchemaRef;

    /// Registers the definition of this type and any nested types into the registry.
    ///
    /// If this type is a primitive, this usually does nothing.
    /// If it is a struct/enum, it should add itself to `defs` and recurse.
    fn register_definitions(registry: &mut SchemaRegistry);
}

impl<T: KdlSchema> KdlSchema for Box<T> {
    fn schema_ref() -> SchemaRef {
        T::schema_ref()
    }

    fn register_definitions(registry: &mut SchemaRegistry) {
        T::register_definitions(registry);
    }
}

/// A registry of reusable schema definitions.
#[derive(Debug, Default, Clone)]
pub struct SchemaRegistry {
    pub definitions: HashMap<String, KdlNodeSchema>,
}

impl SchemaRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, name: impl Into<String>, schema: KdlNodeSchema) {
        self.definitions.insert(name.into(), schema);
    }
}

/// Build a schema node for `(type)fragment "name" { ... }`.
pub fn fragment_node_schema(definitions: &SchemaRegistry) -> KdlNodeSchema {
    let mut names: Vec<String> = definitions.definitions.keys().cloned().collect();
    names.sort();
    let def_refs: Vec<SchemaRef> = names.iter().cloned().map(SchemaRef::Ref).collect();
    let allowed = SchemaRef::Choice(def_refs.clone());
    let patch_refs: Vec<SchemaRef> = names
        .iter()
        .filter_map(|name| {
            definitions
                .definitions
                .get(name)
                .map(|schema| (name, schema))
        })
        .map(|(name, schema)| {
            let mut patch_schema = schema.clone();
            let patch_target = patch_schema
                .name
                .clone()
                .unwrap_or_else(|| name.to_string());
            patch_schema.name = Some(format!("~{}", patch_target));
            patch_schema.required = Some(false);
            SchemaRef::Inline(patch_schema)
        })
        .collect();

    let mut schema = KdlNodeSchema::default();
    schema.name = Some("fragment".to_string());
    schema.description = Some("Typed fragment definition".to_string());
    schema.values.push(SchemaValue {
        ty: SchemaType::String,
        required: true,
        description: Some("Fragment name".to_string()),
        enum_values: None,
        validations: vec![],
    });
    schema.type_annotation = Some(Box::new(TypeAnnotationSchema {
        required: false,
        allowed: allowed.clone(),
    }));
    if !def_refs.is_empty() || !patch_refs.is_empty() {
        let mut child_refs = Vec::new();
        if !def_refs.is_empty() {
            child_refs.push(allowed);
        }
        if !patch_refs.is_empty() {
            child_refs.push(SchemaRef::Choice(patch_refs));
        }
        schema.children = Some(Box::new(ChildrenSchema { nodes: child_refs }));
    }
    schema
}

/// A reference to a schema, either inline or by name.
#[derive(Debug, Clone, PartialEq)]
pub enum SchemaRef {
    Ref(String),
    Inline(KdlNodeSchema),
    Choice(Vec<SchemaRef>),
}

/// A complete KDL Document Schema.
#[derive(Debug, Clone, Default)]
pub struct Document {
    pub info: Info,
    pub nodes: Vec<KdlNodeSchema>,
    pub definitions: HashMap<String, KdlNodeSchema>,
}

impl KdlRender for Document {
    fn render<W: std::fmt::Write>(
        &self,
        w: &mut W,
        _name: &str,
        indent: usize,
    ) -> std::fmt::Result {
        // Render document info block
        writeln!(w, "document {{")?;
        self.info.render(w, "info", indent + 1)?;
        writeln!(w, "}}")?;
        writeln!(w)?;

        // Render nodes
        for node in &self.nodes {
            node.render(w, "node", indent)?;
            writeln!(w)?;
        }

        // Render definitions
        if !self.definitions.is_empty() {
            writeln!(w, "definitions {{")?;
            let mut sorted_defs: Vec<_> = self.definitions.iter().collect();
            sorted_defs.sort_by_key(|(k, _)| *k);

            for (name, schema) in sorted_defs {
                schema.render(w, name, indent + 1)?;
                writeln!(w)?;
            }
            writeln!(w, "}}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct Info {
    pub title: Option<String>,
    pub description: Option<String>,
    pub authors: Vec<String>,
    pub version: Option<String>,
    pub license: Option<String>,
}

impl KdlRender for Info {
    fn render<W: std::fmt::Write>(
        &self,
        w: &mut W,
        _name: &str,
        indent: usize,
    ) -> std::fmt::Result {
        crate::write_indent(w, indent)?; // "info" is usually implicit/conceptual in document {} but explicit here
        write!(w, "info")?;

        if self.title.is_none() && self.description.is_none() && self.authors.is_empty() {
            writeln!(w)?;
            return Ok(());
        }

        writeln!(w, " {{")?;

        if let Some(title) = &self.title {
            crate::write_indent(w, indent + 1)?;
            writeln!(w, "title {:?}", title)?;
        }
        if let Some(desc) = &self.description {
            crate::write_indent(w, indent + 1)?;
            writeln!(w, "description {:?}", desc)?;
        }
        // ... other info fields

        crate::write_indent(w, indent)?;
        writeln!(w, "}}")?;
        Ok(())
    }
}

/// Describes the schema for a single KDL node.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct KdlNodeSchema {
    pub name: Option<String>, // if strictly matching a specific name
    pub description: Option<String>,
    pub props: HashMap<String, SchemaProp>,
    pub values: Vec<SchemaValue>,
    pub children: Option<Box<ChildrenSchema>>,
    pub variants: Option<Vec<SchemaRef>>,
    pub type_annotation: Option<Box<TypeAnnotationSchema>>,

    // For when this schema is just a wrapper around another type (e.g. typedef)
    pub ref_type: Option<String>,

    // Schema metadata
    pub defaults: SchemaDefaults,
    pub deny_unknown: Option<bool>,
    pub required: Option<bool>,
    pub registry_key: Option<RegistryKeySchema>,
    pub validations: Vec<Validation>,
}

impl KdlNodeSchema {
    pub fn merge_from(&mut self, other: KdlNodeSchema) -> Result<(), String> {
        for (key, prop) in other.props {
            if let Some(existing) = self.props.get(&key) {
                if existing != &prop {
                    return Err(format!("conflicting schema prop for '{key}'"));
                }
                continue;
            }
            self.props.insert(key, prop);
        }

        if !other.values.is_empty() {
            self.values.extend(other.values);
        }

        if let Some(children) = other.children {
            if let Some(existing) = self.children.as_mut() {
                existing.nodes.extend(children.nodes);
            } else {
                self.children = Some(children);
            }
        }

        if let Some(type_annotation) = other.type_annotation {
            if let Some(existing) = &self.type_annotation {
                if existing.as_ref() != type_annotation.as_ref() {
                    return Err("conflicting type annotations".to_string());
                }
            } else {
                self.type_annotation = Some(type_annotation);
            }
        }

        if self.registry_key.is_none() {
            self.registry_key = other.registry_key;
        }

        self.validations.extend(other.validations);

        Ok(())
    }
}

impl KdlRender for KdlNodeSchema {
    fn render<W: std::fmt::Write>(&self, w: &mut W, name: &str, indent: usize) -> std::fmt::Result {
        crate::write_indent(w, indent)?;

        // If this is a definition, 'name' comes from the key in definitions map generally
        // But for actual 'node' entries, name is "node" and arg is the node name.
        if name == "node" {
            write!(w, "node")?;
            if let Some(n) = &self.name {
                write!(w, " {:?}", n)?;
            }
            if self.required == Some(true) {
                write!(w, " required=#true")?;
            }
        } else {
            // It's a definition
            write!(w, "{}", name)?;
        }

        if let Some(desc) = &self.description {
            writeln!(w, " {{-")?;
            crate::write_indent(w, indent + 1)?;
            writeln!(w, "description {:?}", desc)?;
            crate::write_indent(w, indent)?;
            write!(w, "}}")?;
        }

        writeln!(w, " {{")?;

        if let Some(ref_cur) = &self.ref_type {
            crate::write_indent(w, indent + 1)?;
            writeln!(w, "ref {:?}", ref_cur)?;
        }

        if let Some(deny_unknown) = self.deny_unknown {
            crate::write_indent(w, indent + 1)?;
            writeln!(
                w,
                "deny_unknown {}",
                if deny_unknown { "#true" } else { "#false" }
            )?;
        }

        if let Some(default_placement) = self.defaults.placement {
            crate::write_indent(w, indent + 1)?;
            writeln!(
                w,
                "default_placement {:?}",
                match default_placement {
                    DefaultPlacement::Exhaustive => "exhaustive",
                    DefaultPlacement::Attr => "attr",
                    DefaultPlacement::Value => "value",
                    DefaultPlacement::Child => "child",
                }
            )?;
        }

        if let Some(default_bool) = self.defaults.bool_mode {
            crate::write_indent(w, indent + 1)?;
            writeln!(
                w,
                "default_bool {:?}",
                match default_bool {
                    BoolMode::PresenceAndValue => "presence+value",
                    BoolMode::ValueOnly => "value-only",
                    BoolMode::PresenceOnly => "presence-only",
                }
            )?;
        }

        if let Some(default_flag_style) = self.defaults.flag_style {
            crate::write_indent(w, indent + 1)?;
            writeln!(
                w,
                "default_flag_style {:?}",
                match default_flag_style {
                    FlagStyle::Both => "both",
                    FlagStyle::ValueNo => "value|no",
                    FlagStyle::WithWithout => "with|without",
                }
            )?;
        }

        if let Some(default_conflict) = self.defaults.conflict {
            crate::write_indent(w, indent + 1)?;
            writeln!(
                w,
                "default_conflict {:?}",
                match default_conflict {
                    ConflictPolicy::Error => "error",
                    ConflictPolicy::First => "first",
                    ConflictPolicy::Last => "last",
                    ConflictPolicy::Append => "append",
                }
            )?;
        }

        if let Some(registry_key) = &self.registry_key {
            registry_key.render(w, indent + 1)?;
        }

        if !self.validations.is_empty() {
            crate::write_indent(w, indent + 1)?;
            write!(w, "validate \"")?;
            for (i, v) in self.validations.iter().enumerate() {
                if i > 0 {
                    write!(w, " ")?;
                }
                v.render_inline(w)?;
            }
            writeln!(w, "\"")?;
        }

        if let Some(variants) = &self.variants {
            render_choice_nodes(variants, w, indent + 1)?;
            writeln!(w)?;
        }

        // Props
        let mut sorted_props: Vec<_> = self.props.iter().collect();
        sorted_props.sort_by_key(|(k, _)| *k);
        for (key, prop) in sorted_props {
            crate::write_indent(w, indent + 1)?;
            write!(w, "prop {:?}", key)?;
            prop.render_inline(w)?;
            writeln!(w)?;
        }

        // Values
        for (_i, val) in self.values.iter().enumerate() {
            crate::write_indent(w, indent + 1)?;
            // If min/max count needed, we'd handle it. Usually implied 1.
            write!(w, "value")?;
            val.render_inline(w)?;
            writeln!(w)?;
        }

        // Type annotations
        if let Some(type_annotation) = &self.type_annotation {
            type_annotation.render(w, indent + 1)?;
            writeln!(w)?;
        }

        // Children
        if let Some(children) = &self.children {
            children.render(w, "children", indent + 1)?;
            writeln!(w)?;
        }

        crate::write_indent(w, indent)?;
        write!(w, "}}")?; // no newline here, caller handles
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotationSchema {
    pub required: bool,
    pub allowed: SchemaRef,
}

impl TypeAnnotationSchema {
    fn render<W: std::fmt::Write>(&self, w: &mut W, indent: usize) -> std::fmt::Result {
        crate::write_indent(w, indent)?;
        write!(w, "type_annotation")?;
        if self.required {
            write!(w, " required=#true")?;
        }

        match &self.allowed {
            SchemaRef::Ref(r) => {
                write!(w, " ref={:?}", r)?;
                Ok(())
            }
            SchemaRef::Inline(node) => {
                writeln!(w, " {{")?;
                node.render(w, "node", indent + 1)?;
                writeln!(w)?;
                crate::write_indent(w, indent)?;
                write!(w, "}}")
            }
            SchemaRef::Choice(choices) => {
                writeln!(w, " {{")?;
                render_choice_nodes(choices, w, indent + 1)?;
                writeln!(w)?;
                crate::write_indent(w, indent)?;
                write!(w, "}}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChildrenSchema {
    pub nodes: Vec<SchemaRef>,
}

impl KdlRender for ChildrenSchema {
    fn render<W: std::fmt::Write>(
        &self,
        w: &mut W,
        _name: &str,
        indent: usize,
    ) -> std::fmt::Result {
        crate::write_indent(w, indent)?;
        writeln!(w, "children {{")?;
        for node in &self.nodes {
            match node {
                SchemaRef::Ref(r) => {
                    crate::write_indent(w, indent + 1)?;
                    writeln!(w, "node ref={:?}", r)?;
                }
                SchemaRef::Inline(s) => {
                    s.render(w, "node", indent + 1)?;
                    writeln!(w)?;
                }
                SchemaRef::Choice(choices) => {
                    render_choice_nodes(choices, w, indent + 1)?;
                    writeln!(w)?;
                }
            }
        }
        crate::write_indent(w, indent)?;
        write!(w, "}}")?;
        Ok(())
    }
}

fn render_choice_nodes<W: std::fmt::Write>(
    choices: &[SchemaRef],
    w: &mut W,
    indent: usize,
) -> std::fmt::Result {
    crate::write_indent(w, indent)?;
    writeln!(w, "choice {{")?;
    for choice in choices {
        match choice {
            SchemaRef::Ref(r) => {
                crate::write_indent(w, indent + 1)?;
                writeln!(w, "node ref={:?}", r)?;
            }
            SchemaRef::Inline(s) => {
                s.render(w, "node", indent + 1)?;
                writeln!(w)?;
            }
            SchemaRef::Choice(nested) => {
                render_choice_nodes(nested, w, indent + 1)?;
                writeln!(w)?;
            }
        }
    }
    crate::write_indent(w, indent)?;
    write!(w, "}}")?;
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
pub enum SchemaType {
    String,
    Integer,
    Float,
    Boolean,
    Null,
    Variadic(Box<SchemaType>),
    AnyOf(Vec<SchemaType>),
    // ... formats etc
}

impl SchemaType {
    fn render_inline<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
        match self {
            Self::Variadic(inner) => {
                inner.render_inline(w)?;
                write!(w, " repeat=\"*\"")
            }
            Self::AnyOf(types) => {
                let rendered = types
                    .iter()
                    .filter_map(|ty| ty.type_name())
                    .map(|name| format!("{:?}", name))
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(w, " type=[{}]", rendered)
            }
            Self::String => write!(w, " type=\"string\""),
            Self::Integer => write!(w, " type=\"integer\""),
            Self::Float => write!(w, " type=\"number\""),
            Self::Boolean => write!(w, " type=\"boolean\""),
            Self::Null => write!(w, " type=\"null\""),
        }
    }

    fn type_name(&self) -> Option<&'static str> {
        match self {
            Self::String => Some("string"),
            Self::Integer => Some("integer"),
            Self::Float => Some("number"),
            Self::Boolean => Some("boolean"),
            Self::Null => Some("null"),
            Self::Variadic(inner) => inner.type_name(),
            Self::AnyOf(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SchemaDefaults {
    pub placement: Option<DefaultPlacement>,
    pub bool_mode: Option<BoolMode>,
    pub flag_style: Option<FlagStyle>,
    pub conflict: Option<ConflictPolicy>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SchemaProp {
    pub ty: SchemaType,
    pub required: bool,
    pub bool_mode: Option<BoolMode>,
    pub flag_style: Option<FlagStyle>,
    pub conflict: Option<ConflictPolicy>,
    pub description: Option<String>,
    pub validations: Vec<Validation>,
}

impl SchemaProp {
    fn render_inline<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
        self.ty.render_inline(w)?;
        if self.required {
            write!(w, " required=#true")?;
        }
        if let Some(desc) = &self.description {
            write!(w, " description={:?}", desc)?;
        }
        if let Some(bool_mode) = self.bool_mode {
            write!(
                w,
                " bool={:?}",
                match bool_mode {
                    BoolMode::PresenceAndValue => "presence+value",
                    BoolMode::ValueOnly => "value-only",
                    BoolMode::PresenceOnly => "presence-only",
                }
            )?;
        }
        if let Some(flag_style) = self.flag_style {
            write!(
                w,
                " flag_style={:?}",
                match flag_style {
                    FlagStyle::Both => "both",
                    FlagStyle::ValueNo => "value|no",
                    FlagStyle::WithWithout => "with|without",
                }
            )?;
        }
        if let Some(conflict) = self.conflict {
            write!(
                w,
                " conflict={:?}",
                match conflict {
                    ConflictPolicy::Error => "error",
                    ConflictPolicy::First => "first",
                    ConflictPolicy::Last => "last",
                    ConflictPolicy::Append => "append",
                }
            )?;
        }
        if !self.validations.is_empty() {
            write!(w, " validate=\"")?;
            for (i, v) in self.validations.iter().enumerate() {
                if i > 0 {
                    write!(w, " ")?;
                }
                v.render_inline(w)?;
            }
            write!(w, "\"")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SchemaValue {
    pub ty: SchemaType,
    pub required: bool,
    pub description: Option<String>,
    pub enum_values: Option<Vec<SchemaLiteral>>,
    pub validations: Vec<Validation>,
}

impl SchemaValue {
    fn render_inline<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
        self.ty.render_inline(w)?;
        if self.required {
            write!(w, " required=#true")?;
        }
        if let Some(desc) = &self.description {
            write!(w, " description={:?}", desc)?;
        }
        if let Some(values) = &self.enum_values {
            let rendered = values
                .iter()
                .map(render_literal)
                .collect::<Vec<_>>()
                .join(" ");
            write!(w, " enum=[{}]", rendered)?;
        }
        if !self.validations.is_empty() {
            write!(w, " validate=\"")?;
            for (i, v) in self.validations.iter().enumerate() {
                if i > 0 {
                    write!(w, " ")?;
                }
                v.render_inline(w)?;
            }
            write!(w, "\"")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SchemaLiteral {
    String(String),
    Int(i128),
    Float(f64),
    Bool(bool),
    Null,
}

/// A validation rule that can be applied to values or props.
#[derive(Debug, Clone, PartialEq)]
pub enum Validation {
    /// Minimum numeric value (inclusive).
    Min(f64),
    /// Maximum numeric value (inclusive).
    Max(f64),
    /// Numeric range [min, max] (both inclusive).
    Range(f64, f64),
    /// Value must be a multiple of this number.
    MultipleOf(f64),
    /// Value must be positive (> 0).
    Positive,
    /// Value must be negative (< 0).
    Negative,
    /// Value must be >= 0.
    NonNegative,
    /// Value must be <= 0.
    NonPositive,

    /// Minimum string length.
    MinLen(usize),
    /// Maximum string length.
    MaxLen(usize),
    /// String length range [min, max].
    Len(usize, usize),
    /// String must match regex pattern.
    Pattern(String),
    /// String must be non-empty.
    NonEmpty,
    /// String must be ASCII only.
    Ascii,
    /// String must be alphanumeric only.
    Alphanumeric,

    /// Minimum collection item count.
    MinItems(usize),
    /// Maximum collection item count.
    MaxItems(usize),

    /// Custom validation function path.
    Func(String),

    // === Cross-field reference validators ===
    /// This field's value must be less than the named field's value.
    LessThan(String),
    /// This field's value must be less than or equal to the named field's value.
    LessThanOrEqual(String),
    /// This field's value must be greater than the named field's value.
    GreaterThan(String),
    /// This field's value must be greater than or equal to the named field's value.
    GreaterThanOrEqual(String),
    /// This field's value must equal the named field's value.
    EqualTo(String),
    /// This field's value must not equal the named field's value.
    NotEqualTo(String),
    /// This field's scalar value must exist in the named collection field.
    ExistsIn(String),
    /// This field's collection value must be a subset of the named collection field.
    SubsetOf(String),
}

impl Validation {
    fn render_inline<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
        match self {
            Self::Min(v) => write!(w, "min({})", format_f64(*v)),
            Self::Max(v) => write!(w, "max({})", format_f64(*v)),
            Self::Range(min, max) => {
                write!(w, "range({}, {})", format_f64(*min), format_f64(*max))
            }
            Self::MultipleOf(v) => write!(w, "multiple_of({})", format_f64(*v)),
            Self::Positive => write!(w, "positive"),
            Self::Negative => write!(w, "negative"),
            Self::NonNegative => write!(w, "non_negative"),
            Self::NonPositive => write!(w, "non_positive"),
            Self::MinLen(v) => write!(w, "min_len({})", v),
            Self::MaxLen(v) => write!(w, "max_len({})", v),
            Self::Len(min, max) => write!(w, "len({}, {})", min, max),
            Self::Pattern(p) => write!(w, "pattern({:?})", p),
            Self::NonEmpty => write!(w, "non_empty"),
            Self::Ascii => write!(w, "ascii"),
            Self::Alphanumeric => write!(w, "alphanumeric"),
            Self::MinItems(v) => write!(w, "min_items({})", v),
            Self::MaxItems(v) => write!(w, "max_items({})", v),
            Self::Func(path) => write!(w, "func({:?})", path),
            Self::LessThan(f) => write!(w, "less_than({:?})", f),
            Self::LessThanOrEqual(f) => write!(w, "less_than_or_equal({:?})", f),
            Self::GreaterThan(f) => write!(w, "greater_than({:?})", f),
            Self::GreaterThanOrEqual(f) => write!(w, "greater_than_or_equal({:?})", f),
            Self::EqualTo(f) => write!(w, "equal_to({:?})", f),
            Self::NotEqualTo(f) => write!(w, "not_equal_to({:?})", f),
            Self::ExistsIn(f) => write!(w, "exists_in({:?})", f),
            Self::SubsetOf(f) => write!(w, "subset_of({:?})", f),
        }
    }

    /// Validate a string value against this rule. Returns Ok(()) or an error message.
    pub fn validate_str(&self, value: &str) -> Result<(), String> {
        match self {
            Self::NonEmpty => {
                if value.is_empty() {
                    return Err("value must not be empty".to_string());
                }
            }
            Self::MinLen(min) => {
                if value.len() < *min {
                    return Err(format!(
                        "length {} is less than minimum {}",
                        value.len(),
                        min
                    ));
                }
            }
            Self::MaxLen(max) => {
                if value.len() > *max {
                    return Err(format!("length {} exceeds maximum {}", value.len(), max));
                }
            }
            Self::Len(min, max) => {
                let len = value.len();
                if len < *min || len > *max {
                    return Err(format!("length {} is not in range [{}, {}]", len, min, max));
                }
            }
            Self::Pattern(pattern) => {
                // Pattern validation is best-effort; we store the pattern for schema purposes
                // but runtime regex matching requires the `regex` crate
                let _ = pattern;
            }
            Self::Ascii => {
                if !value.is_ascii() {
                    return Err("value must be ASCII only".to_string());
                }
            }
            Self::Alphanumeric => {
                if !value.chars().all(|c| c.is_alphanumeric()) {
                    return Err("value must be alphanumeric only".to_string());
                }
            }
            _ => {} // Numeric validations don't apply to strings
        }
        Ok(())
    }

    /// Validate a numeric value against this rule.
    pub fn validate_number(&self, value: f64) -> Result<(), String> {
        match self {
            Self::Min(min) => {
                if value < *min {
                    return Err(format!("{} is less than minimum {}", value, min));
                }
            }
            Self::Max(max) => {
                if value > *max {
                    return Err(format!("{} exceeds maximum {}", value, max));
                }
            }
            Self::Range(min, max) => {
                if value < *min || value > *max {
                    return Err(format!("{} is not in range [{}, {}]", value, min, max));
                }
            }
            Self::MultipleOf(divisor) => {
                if *divisor != 0.0 && (value % divisor).abs() > f64::EPSILON {
                    return Err(format!("{} is not a multiple of {}", value, divisor));
                }
            }
            Self::Positive => {
                if value <= 0.0 {
                    return Err(format!("{} is not positive", value));
                }
            }
            Self::Negative => {
                if value >= 0.0 {
                    return Err(format!("{} is not negative", value));
                }
            }
            Self::NonNegative => {
                if value < 0.0 {
                    return Err(format!("{} is not non-negative", value));
                }
            }
            Self::NonPositive => {
                if value > 0.0 {
                    return Err(format!("{} is not non-positive", value));
                }
            }
            _ => {} // String/collection validations don't apply to numbers
        }
        Ok(())
    }

    /// Validate a collection length against this rule.
    pub fn validate_count(&self, count: usize) -> Result<(), String> {
        match self {
            Self::MinItems(min) => {
                if count < *min {
                    return Err(format!("item count {} is less than minimum {}", count, min));
                }
            }
            Self::MaxItems(max) => {
                if count > *max {
                    return Err(format!("item count {} exceeds maximum {}", count, max));
                }
            }
            _ => {} // Other validations don't apply to counts
        }
        Ok(())
    }

    /// Validate this field's numeric value against another field's numeric value.
    /// `other_field` is the name of the referenced field, `other_value` is its value.
    pub fn validate_cross_field(
        &self,
        value: f64,
        other_field: &str,
        other_value: f64,
    ) -> Result<(), String> {
        match self {
            Self::LessThan(_) => {
                if value >= other_value {
                    return Err(format!(
                        "{} must be less than '{}' ({})",
                        value, other_field, other_value
                    ));
                }
            }
            Self::LessThanOrEqual(_) => {
                if value > other_value {
                    return Err(format!(
                        "{} must be less than or equal to '{}' ({})",
                        value, other_field, other_value
                    ));
                }
            }
            Self::GreaterThan(_) => {
                if value <= other_value {
                    return Err(format!(
                        "{} must be greater than '{}' ({})",
                        value, other_field, other_value
                    ));
                }
            }
            Self::GreaterThanOrEqual(_) => {
                if value < other_value {
                    return Err(format!(
                        "{} must be greater than or equal to '{}' ({})",
                        value, other_field, other_value
                    ));
                }
            }
            Self::EqualTo(_) => {
                if (value - other_value).abs() > f64::EPSILON {
                    return Err(format!(
                        "{} must equal '{}' ({})",
                        value, other_field, other_value
                    ));
                }
            }
            Self::NotEqualTo(_) => {
                if (value - other_value).abs() <= f64::EPSILON {
                    return Err(format!(
                        "{} must not equal '{}' ({})",
                        value, other_field, other_value
                    ));
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Returns the referenced field name if this is a cross-field validator.
    pub fn cross_field_ref(&self) -> Option<&str> {
        match self {
            Self::LessThan(f)
            | Self::LessThanOrEqual(f)
            | Self::GreaterThan(f)
            | Self::GreaterThanOrEqual(f)
            | Self::EqualTo(f)
            | Self::NotEqualTo(f)
            | Self::ExistsIn(f)
            | Self::SubsetOf(f) => Some(f.as_str()),
            _ => None,
        }
    }
}

fn format_f64(v: f64) -> String {
    if v == v.floor() && v.abs() < 1e15 {
        format!("{}", v as i64)
    } else {
        v.to_string()
    }
}

/// Parse a validation DSL string into a list of validation rules.
///
/// The DSL supports space/comma-separated rules:
/// - Numeric: `min(N)`, `max(N)`, `range(MIN, MAX)`, `multiple_of(N)`, `positive`, `negative`, `non_negative`, `non_positive`
/// - String: `min_len(N)`, `max_len(N)`, `len(MIN, MAX)`, `pattern("REGEX")`, `non_empty`, `ascii`, `alphanumeric`
/// - Collection: `min_items(N)`, `max_items(N)`
/// - Cross-field: `less_than("field")`, `lte("field")`, `greater_than("field")`, `gte("field")`, `equal_to("field")`, `not_equal_to("field")`, `exists_in("field")`, `subset_of("field")`
/// - Custom: `func("path::to::fn")`
pub fn parse_validation_dsl(input: &str) -> Result<Vec<Validation>, String> {
    let input = input.trim();
    if input.is_empty() {
        return Ok(vec![]);
    }

    let mut rules = Vec::new();
    let mut chars = input.chars().peekable();

    while chars.peek().is_some() {
        while chars
            .peek()
            .map_or(false, |c| c.is_whitespace() || *c == ',')
        {
            chars.next();
        }
        if chars.peek().is_none() {
            break;
        }

        let mut ident = String::new();
        while chars
            .peek()
            .map_or(false, |c| c.is_alphanumeric() || *c == '_')
        {
            ident.push(chars.next().unwrap());
        }

        if ident.is_empty() {
            return Err(format!(
                "unexpected character in validation DSL: {:?}",
                chars.peek()
            ));
        }

        let rule = if chars.peek() == Some(&'(') {
            chars.next();
            let mut depth = 1;
            let mut args = String::new();
            while let Some(c) = chars.next() {
                match c {
                    '(' => {
                        depth += 1;
                        args.push(c);
                    }
                    ')' => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        args.push(c);
                    }
                    _ => args.push(c),
                }
            }
            if depth != 0 {
                return Err("unclosed parenthesis in validation DSL".to_string());
            }
            parse_dsl_rule_with_args(&ident, &args)?
        } else {
            parse_dsl_rule_bare(&ident)?
        };

        rules.push(rule);
    }

    Ok(rules)
}

fn parse_dsl_f64(s: &str) -> Result<f64, String> {
    s.trim()
        .parse::<f64>()
        .map_err(|_| format!("expected number, got {:?}", s.trim()))
}

fn parse_dsl_usize(s: &str) -> Result<usize, String> {
    s.trim()
        .parse::<usize>()
        .map_err(|_| format!("expected non-negative integer, got {:?}", s.trim()))
}

fn parse_dsl_string_arg(s: &str) -> String {
    let s = s.trim();
    if (s.starts_with('"') && s.ends_with('"')) || (s.starts_with('\'') && s.ends_with('\'')) {
        s[1..s.len() - 1].to_string()
    } else {
        s.to_string()
    }
}

fn parse_dsl_rule_with_args(name: &str, args: &str) -> Result<Validation, String> {
    match name {
        "min" => Ok(Validation::Min(parse_dsl_f64(args)?)),
        "max" => Ok(Validation::Max(parse_dsl_f64(args)?)),
        "range" => {
            let parts: Vec<&str> = args.split(',').collect();
            if parts.len() != 2 {
                return Err("range() requires exactly 2 arguments".to_string());
            }
            Ok(Validation::Range(
                parse_dsl_f64(parts[0])?,
                parse_dsl_f64(parts[1])?,
            ))
        }
        "multiple_of" => Ok(Validation::MultipleOf(parse_dsl_f64(args)?)),
        "min_len" => Ok(Validation::MinLen(parse_dsl_usize(args)?)),
        "max_len" => Ok(Validation::MaxLen(parse_dsl_usize(args)?)),
        "len" => {
            let parts: Vec<&str> = args.split(',').collect();
            if parts.len() != 2 {
                return Err("len() requires exactly 2 arguments".to_string());
            }
            Ok(Validation::Len(
                parse_dsl_usize(parts[0])?,
                parse_dsl_usize(parts[1])?,
            ))
        }
        "pattern" => Ok(Validation::Pattern(parse_dsl_string_arg(args))),
        "min_items" => Ok(Validation::MinItems(parse_dsl_usize(args)?)),
        "max_items" => Ok(Validation::MaxItems(parse_dsl_usize(args)?)),
        "func" => Ok(Validation::Func(parse_dsl_string_arg(args))),
        "less_than" | "lt" => Ok(Validation::LessThan(parse_dsl_string_arg(args))),
        "less_than_or_equal" | "lte" => Ok(Validation::LessThanOrEqual(parse_dsl_string_arg(args))),
        "greater_than" | "gt" => Ok(Validation::GreaterThan(parse_dsl_string_arg(args))),
        "greater_than_or_equal" | "gte" => {
            Ok(Validation::GreaterThanOrEqual(parse_dsl_string_arg(args)))
        }
        "equal_to" | "eq" => Ok(Validation::EqualTo(parse_dsl_string_arg(args))),
        "not_equal_to" | "neq" => Ok(Validation::NotEqualTo(parse_dsl_string_arg(args))),
        "exists_in" => Ok(Validation::ExistsIn(parse_dsl_string_arg(args))),
        "subset_of" => Ok(Validation::SubsetOf(parse_dsl_string_arg(args))),
        _ => Err(format!("unknown validation rule: {}", name)),
    }
}

fn parse_dsl_rule_bare(name: &str) -> Result<Validation, String> {
    match name {
        "positive" => Ok(Validation::Positive),
        "negative" => Ok(Validation::Negative),
        "non_negative" => Ok(Validation::NonNegative),
        "non_positive" => Ok(Validation::NonPositive),
        "non_empty" => Ok(Validation::NonEmpty),
        "ascii" => Ok(Validation::Ascii),
        "alphanumeric" => Ok(Validation::Alphanumeric),
        _ => Err(format!(
            "unknown validation rule or missing arguments: {}",
            name
        )),
    }
}

fn render_literal(lit: &SchemaLiteral) -> String {
    match lit {
        SchemaLiteral::String(s) => crate::escape_string(s),
        SchemaLiteral::Int(n) => n.to_string(),
        SchemaLiteral::Float(f) => {
            let s = f.to_string();
            if s.contains('.') || s.contains('e') || s.contains('E') {
                s
            } else {
                format!("{s}.0")
            }
        }
        SchemaLiteral::Bool(true) => "#true".to_string(),
        SchemaLiteral::Bool(false) => "#false".to_string(),
        SchemaLiteral::Null => "#null".to_string(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegistryKeySource {
    Arg,
    Attr,
    Function,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegistryKeySchema {
    pub source: RegistryKeySource,
    pub arg_index: Option<usize>,
    pub attr: Option<String>,
    pub func: Option<String>,
}

impl RegistryKeySchema {
    fn render<W: std::fmt::Write>(&self, w: &mut W, indent: usize) -> std::fmt::Result {
        crate::write_indent(w, indent)?;
        write!(w, "registry_key")?;
        match self.source {
            RegistryKeySource::Arg => {
                write!(w, " source=\"arg\"")?;
                if let Some(index) = self.arg_index {
                    write!(w, " index={}", index)?;
                }
            }
            RegistryKeySource::Attr => {
                write!(w, " source=\"attr\"")?;
                if let Some(attr) = &self.attr {
                    write!(w, " name={:?}", attr)?;
                }
            }
            RegistryKeySource::Function => {
                write!(w, " source=\"function\"")?;
                if let Some(func) = &self.func {
                    write!(w, " path={:?}", func)?;
                }
            }
        }
        writeln!(w)?;
        Ok(())
    }
}

impl<T: KdlSchema> KdlSchema for HashSet<T> {
    fn schema_ref() -> SchemaRef {
        T::schema_ref()
    }

    fn register_definitions(registry: &mut SchemaRegistry) {
        T::register_definitions(registry);
    }
}

// Implementations for primitives
impl KdlSchema for String {
    fn schema_ref() -> SchemaRef {
        SchemaRef::Inline(KdlNodeSchema {
            props: HashMap::new(),
            values: vec![SchemaValue {
                ty: SchemaType::String,
                required: true,
                description: None,
                enum_values: None,
                validations: vec![],
            }],
            ..Default::default()
        })
    }
    fn register_definitions(_registry: &mut SchemaRegistry) {}
}

impl KdlSchema for std::path::PathBuf {
    fn schema_ref() -> SchemaRef {
        SchemaRef::Inline(KdlNodeSchema {
            values: vec![SchemaValue {
                ty: SchemaType::String,
                required: true,
                description: None,
                enum_values: None,
                validations: vec![],
            }],
            ..Default::default()
        })
    }
    fn register_definitions(_registry: &mut SchemaRegistry) {}
}

impl KdlSchema for i64 {
    fn schema_ref() -> SchemaRef {
        SchemaRef::Inline(KdlNodeSchema {
            values: vec![SchemaValue {
                ty: SchemaType::Integer,
                required: true,
                description: None,
                enum_values: None,
                validations: vec![],
            }],
            ..Default::default()
        })
    }
    fn register_definitions(_registry: &mut SchemaRegistry) {}
}

impl KdlSchema for u16 {
    fn schema_ref() -> SchemaRef {
        SchemaRef::Inline(KdlNodeSchema {
            values: vec![SchemaValue {
                ty: SchemaType::Integer,
                required: true,
                description: None,
                enum_values: None,
                validations: vec![],
            }],
            ..Default::default()
        })
    }
    fn register_definitions(_registry: &mut SchemaRegistry) {}
}

impl KdlSchema for i128 {
    fn schema_ref() -> SchemaRef {
        SchemaRef::Inline(KdlNodeSchema {
            values: vec![SchemaValue {
                ty: SchemaType::Integer,
                required: true,
                description: None,
                enum_values: None,
                validations: vec![],
            }],
            ..Default::default()
        })
    }
    fn register_definitions(_registry: &mut SchemaRegistry) {}
}

impl KdlSchema for i32 {
    fn schema_ref() -> SchemaRef {
        SchemaRef::Inline(KdlNodeSchema {
            values: vec![SchemaValue {
                ty: SchemaType::Integer,
                required: true,
                description: None,
                enum_values: None,
                validations: vec![],
            }],
            ..Default::default()
        })
    }
    fn register_definitions(_registry: &mut SchemaRegistry) {}
}

impl KdlSchema for u64 {
    fn schema_ref() -> SchemaRef {
        SchemaRef::Inline(KdlNodeSchema {
            values: vec![SchemaValue {
                ty: SchemaType::Integer,
                required: true,
                description: None,
                enum_values: None,
                validations: vec![],
            }],
            ..Default::default()
        })
    }
    fn register_definitions(_registry: &mut SchemaRegistry) {}
}

impl KdlSchema for u32 {
    fn schema_ref() -> SchemaRef {
        SchemaRef::Inline(KdlNodeSchema {
            values: vec![SchemaValue {
                ty: SchemaType::Integer,
                required: true,
                description: None,
                enum_values: None,
                validations: vec![],
            }],
            ..Default::default()
        })
    }
    fn register_definitions(_registry: &mut SchemaRegistry) {}
}

impl KdlSchema for usize {
    fn schema_ref() -> SchemaRef {
        SchemaRef::Inline(KdlNodeSchema {
            values: vec![SchemaValue {
                ty: SchemaType::Integer,
                required: true,
                description: None,
                enum_values: None,
                validations: vec![],
            }],
            ..Default::default()
        })
    }
    fn register_definitions(_registry: &mut SchemaRegistry) {}
}

impl KdlSchema for f64 {
    fn schema_ref() -> SchemaRef {
        SchemaRef::Inline(KdlNodeSchema {
            values: vec![SchemaValue {
                ty: SchemaType::Float,
                required: true,
                description: None,
                enum_values: None,
                validations: vec![],
            }],
            ..Default::default()
        })
    }
    fn register_definitions(_registry: &mut SchemaRegistry) {}
}

impl KdlSchema for f32 {
    fn schema_ref() -> SchemaRef {
        SchemaRef::Inline(KdlNodeSchema {
            values: vec![SchemaValue {
                ty: SchemaType::Float,
                required: true,
                description: None,
                enum_values: None,
                validations: vec![],
            }],
            ..Default::default()
        })
    }
    fn register_definitions(_registry: &mut SchemaRegistry) {}
}

impl KdlSchema for bool {
    fn schema_ref() -> SchemaRef {
        SchemaRef::Inline(KdlNodeSchema {
            values: vec![SchemaValue {
                ty: SchemaType::Boolean,
                required: true,
                description: None,
                enum_values: None,
                validations: vec![],
            }],
            ..Default::default()
        })
    }
    fn register_definitions(_registry: &mut SchemaRegistry) {}
}

impl<T: KdlSchema> KdlSchema for Option<T> {
    fn schema_ref() -> SchemaRef {
        T::schema_ref()
    }

    fn register_definitions(registry: &mut SchemaRegistry) {
        T::register_definitions(registry);
    }
}

impl<T: KdlSchema> KdlSchema for Vec<T> {
    fn schema_ref() -> SchemaRef {
        T::schema_ref()
    }

    fn register_definitions(registry: &mut SchemaRegistry) {
        T::register_definitions(registry);
    }
}

impl<T: KdlSchema> KdlSchema for std::collections::HashMap<String, T> {
    fn schema_ref() -> SchemaRef {
        T::schema_ref()
    }

    fn register_definitions(registry: &mut SchemaRegistry) {
        T::register_definitions(registry);
    }
}

impl<T: KdlSchema> KdlSchema for (String, T) {
    fn schema_ref() -> SchemaRef {
        T::schema_ref()
    }

    fn register_definitions(registry: &mut SchemaRegistry) {
        T::register_definitions(registry);
    }
}
