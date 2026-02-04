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

    // For when this schema is just a wrapper around another type (e.g. typedef)
    pub ref_type: Option<String>,

    // Schema metadata
    pub defaults: SchemaDefaults,
    pub deny_unknown: Option<bool>,
    pub required: Option<bool>,
    pub registry_key: Option<RegistryKeySchema>,
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
    // ... formats etc
}

impl SchemaType {
    fn render_inline<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
        match self {
            Self::Variadic(inner) => {
                inner.render_inline(w)?;
                write!(w, " repeat=\"*\"")
            }
            Self::String => write!(w, " type=\"string\""),
            Self::Integer => write!(w, " type=\"integer\""),
            Self::Float => write!(w, " type=\"number\""),
            Self::Boolean => write!(w, " type=\"boolean\""),
            Self::Null => write!(w, " type=\"null\""),
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
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SchemaValue {
    pub ty: SchemaType,
    pub required: bool,
    pub description: Option<String>,
    pub enum_values: Option<Vec<SchemaLiteral>>,
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
