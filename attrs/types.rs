//! Enum types for KDL attribute configuration.
//!
//! These types use `darling::FromMeta` for automatic parsing from attribute strings.

use darling::FromMeta;
use proc_macro2::TokenStream;
use quote::quote;

/// Rename strategy for field/node names.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, FromMeta)]
pub enum RenameStrategy {
    #[default]
    #[darling(rename = "none")]
    None,
    #[darling(rename = "kebab-case")]
    KebabCase,
    #[darling(rename = "snake_case")]
    SnakeCase,
    #[darling(rename = "lowercase")]
    Lowercase,
    #[darling(rename = "UPPERCASE")]
    Uppercase,
}

impl RenameStrategy {
    pub fn apply(&self, name: &str) -> String {
        match self {
            RenameStrategy::None => name.to_string(),
            RenameStrategy::KebabCase => to_kebab_case(name),
            RenameStrategy::SnakeCase => to_snake_case(name),
            RenameStrategy::Lowercase => name.to_lowercase(),
            RenameStrategy::Uppercase => name.to_uppercase(),
        }
    }
}

/// Default placement strategy for fields without explicit placement.
#[derive(Debug, Clone, Copy, FromMeta)]
pub enum DefaultPlacement {
    #[darling(rename = "exhaustive")]
    Exhaustive,
    #[darling(rename = "attr")]
    Attr,
    #[darling(rename = "value")]
    Value,
    #[darling(rename = "child")]
    Child,
}

/// Boolean parsing mode for attribute values.
#[derive(Debug, Clone, Copy, FromMeta)]
pub enum BoolMode {
    #[darling(rename = "presence+value")]
    PresenceAndValue,
    #[darling(rename = "value-only")]
    ValueOnly,
    #[darling(rename = "presence-only")]
    PresenceOnly,
}

impl quote::ToTokens for BoolMode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            BoolMode::PresenceAndValue => tokens.extend(quote! { BoolMode::PresenceAndValue }),
            BoolMode::ValueOnly => tokens.extend(quote! { BoolMode::ValueOnly }),
            BoolMode::PresenceOnly => tokens.extend(quote! { BoolMode::PresenceOnly }),
        }
    }
}

/// Style for rendering boolean flags.
#[derive(Debug, Clone, Copy, FromMeta)]
pub enum FlagStyle {
    #[darling(rename = "both")]
    Both,
    #[darling(rename = "value|no")]
    ValueNo,
    #[darling(rename = "with|without")]
    WithWithout,
}

impl quote::ToTokens for FlagStyle {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            FlagStyle::Both => tokens.extend(quote! { FlagStyle::Both }),
            FlagStyle::ValueNo => tokens.extend(quote! { FlagStyle::ValueNo }),
            FlagStyle::WithWithout => tokens.extend(quote! { FlagStyle::WithWithout }),
        }
    }
}

/// Policy for handling duplicate values.
#[derive(Debug, Clone, Copy, FromMeta)]
pub enum ConflictPolicy {
    #[darling(rename = "error")]
    Error,
    #[darling(rename = "first")]
    First,
    #[darling(rename = "last")]
    Last,
    #[darling(rename = "append")]
    Append,
}

impl quote::ToTokens for ConflictPolicy {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            ConflictPolicy::Error => tokens.extend(quote! { ConflictPolicy::Error }),
            ConflictPolicy::First => tokens.extend(quote! { ConflictPolicy::First }),
            ConflictPolicy::Last => tokens.extend(quote! { ConflictPolicy::Last }),
            ConflictPolicy::Append => tokens.extend(quote! { ConflictPolicy::Append }),
        }
    }
}

/// Override for render placement.
#[derive(Debug, Clone, Copy, FromMeta)]
pub enum RenderPlacement {
    #[darling(rename = "attr")]
    Attr,
    #[darling(rename = "value")]
    Value,
    #[darling(rename = "child")]
    Child,
    #[darling(rename = "children")]
    Children,
    #[darling(rename = "registry")]
    Registry,
}

/// Schema type override for scalar fields.
#[derive(Debug, Clone, Copy, FromMeta)]
pub enum SchemaTypeOverride {
    #[darling(rename = "string")]
    String,
    #[darling(rename = "integer")]
    Integer,
    #[darling(rename = "int")]
    Int,
    #[darling(rename = "float")]
    Float,
    #[darling(rename = "number")]
    Number,
    #[darling(rename = "boolean")]
    Boolean,
    #[darling(rename = "bool")]
    Bool,
    #[darling(rename = "null")]
    Null,
}

impl SchemaTypeOverride {
    /// Normalize aliases to canonical form.
    pub fn canonical(&self) -> Self {
        match self {
            SchemaTypeOverride::Int => SchemaTypeOverride::Integer,
            SchemaTypeOverride::Number => SchemaTypeOverride::Float,
            SchemaTypeOverride::Bool => SchemaTypeOverride::Boolean,
            other => *other,
        }
    }
}

/// Registry key extraction method.
#[derive(Debug, Clone)]
pub enum RegistryKey {
    Arg(usize),
    Attr(String),
    Function(String),
}

/// Kind of children map container.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChildrenMapKind {
    HashMap,
    Vec,
    OptionVec,
}

/// Default value specification.
#[derive(Debug, Clone)]
pub enum DefaultSpec {
    /// Use `Default::default()`.
    Derive,
    /// Use a literal value.
    Literal(DefaultLiteral),
    /// Call a function by name.
    Function(String),
}

/// Literal default value.
#[derive(Debug, Clone)]
pub enum DefaultLiteral {
    Int(i128),
    Float(f64),
    Bool(bool),
    String(String),
}

// ============================================================================
// Helper functions for case conversion
// ============================================================================

fn to_kebab_case(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 4);
    let mut prev_lower = false;

    for c in s.chars() {
        if c == '_' {
            result.push('-');
            prev_lower = false;
        } else if c.is_ascii_uppercase() {
            if prev_lower {
                result.push('-');
            }
            result.push(c.to_ascii_lowercase());
            prev_lower = false;
        } else {
            result.push(c);
            prev_lower = c.is_ascii_lowercase();
        }
    }

    result
}

fn to_snake_case(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut prev_lower = false;

    for c in s.chars() {
        if c == '-' {
            result.push('_');
            prev_lower = false;
        } else if c.is_ascii_uppercase() {
            if prev_lower {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase());
            prev_lower = false;
        } else {
            result.push(c);
            prev_lower = c.is_ascii_lowercase();
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::RenameStrategy;

    #[test]
    fn rename_strategy_kebab() {
        assert_eq!(
            RenameStrategy::KebabCase.apply("myFieldName"),
            "my-field-name"
        );
        assert_eq!(
            RenameStrategy::KebabCase.apply("my_field_name"),
            "my-field-name"
        );
    }

    #[test]
    fn rename_strategy_snake() {
        assert_eq!(
            RenameStrategy::SnakeCase.apply("myFieldName"),
            "my_field_name"
        );
        assert_eq!(
            RenameStrategy::SnakeCase.apply("my-field-name"),
            "my_field_name"
        );
    }

    #[test]
    fn rename_strategy_case() {
        assert_eq!(RenameStrategy::Lowercase.apply("MyField"), "myfield");
        assert_eq!(RenameStrategy::Uppercase.apply("myField"), "MYFIELD");
    }
}
