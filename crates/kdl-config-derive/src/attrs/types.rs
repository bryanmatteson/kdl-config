//! Enum types for KDL attribute configuration.
//!
//! These types are parsed from string values by manual helper functions in `parse.rs`.

use proc_macro2::TokenStream;
use quote::quote;

/// Rename strategy for field/node names.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum RenameStrategy {
    #[default]
    None,
    KebabCase,
    SnakeCase,
    Lowercase,
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
#[derive(Debug, Clone, Copy)]
pub enum DefaultPlacement {
    Exhaustive,
    Attr,
    Value,
    Child,
}

/// Boolean parsing mode for attribute values.
#[derive(Debug, Clone, Copy)]
pub enum BoolMode {
    PresenceAndValue,
    ValueOnly,
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
#[derive(Debug, Clone, Copy)]
pub enum FlagStyle {
    Both,
    ValueNo,
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
#[derive(Debug, Clone, Copy)]
pub enum ConflictPolicy {
    Error,
    First,
    Last,
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
#[derive(Debug, Clone, Copy)]
pub enum RenderPlacement {
    Attr,
    Value,
    Child,
    Children,
    Registry,
}

/// Schema type override for scalar fields.
///
/// Only canonical forms are stored; aliases (int→Integer, number→Float, bool→Boolean)
/// are resolved at parse time in `parse.rs`.
#[derive(Debug, Clone, Copy)]
pub enum SchemaTypeOverride {
    String,
    Integer,
    Float,
    Boolean,
    Null,
}

/// Selector AST for locating values in a KDL node.
#[derive(Debug, Clone)]
pub enum SelectorAst {
    Arg(u32),
    Attr(String),
    Name,
    Func(String),
    Any(Vec<SelectorAst>),
}

impl quote::ToTokens for SelectorAst {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            SelectorAst::Arg(idx) => tokens.extend(quote! { ::kdl_config::SelectorAst::Arg(#idx) }),
            SelectorAst::Attr(name) => {
                tokens.extend(quote! { ::kdl_config::SelectorAst::Attr(#name.to_string()) })
            }
            SelectorAst::Name => tokens.extend(quote! { ::kdl_config::SelectorAst::Name }),
            SelectorAst::Func(path) => {
                tokens.extend(quote! { ::kdl_config::SelectorAst::Func(#path.to_string()) })
            }
            SelectorAst::Any(list) => {
                tokens.extend(quote! { ::kdl_config::SelectorAst::Any(vec![#(#list),*]) })
            }
        }
    }
}

/// Select options for selector use.
#[derive(Debug, Clone, Default)]
pub struct SelectOpts {
    pub consume: Option<bool>,
    pub inject: Option<InjectOpt>,
}

impl quote::ToTokens for SelectOpts {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let consume = match self.consume {
            Some(v) => quote! { Some(#v) },
            None => quote! { None },
        };
        let inject = match &self.inject {
            Some(opt) => quote! { Some(#opt) },
            None => quote! { None },
        };
        tokens.extend(quote! { ::kdl_config::SelectOpts { consume: #consume, inject: #inject } });
    }
}

/// Inject options for selectors.
#[derive(Debug, Clone)]
pub enum InjectOpt {
    Implicit,
    Field(String),
}

impl quote::ToTokens for InjectOpt {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            InjectOpt::Implicit => tokens.extend(quote! { ::kdl_config::InjectOpt::Implicit }),
            InjectOpt::Field(name) => {
                tokens.extend(quote! { ::kdl_config::InjectOpt::Field(#name.to_string()) })
            }
        }
    }
}

/// Selector + options.
#[derive(Debug, Clone)]
pub struct SelectSpec {
    pub selector: SelectorAst,
    pub opts: SelectOpts,
}

impl quote::ToTokens for SelectSpec {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let selector = &self.selector;
        let opts = &self.opts;
        tokens.extend(quote! { ::kdl_config::SelectSpec { selector: #selector, opts: #opts } });
    }
}

/// Collection mode for registry/children_map fields.
#[derive(Debug, Clone)]
pub enum CollectionMode {
    Registry { container: String },
    ChildrenMapAll,
    ChildrenMapNode { node: String },
}

/// Derived collection specification.
#[derive(Debug, Clone)]
pub struct CollectionSpec {
    pub mode: CollectionMode,
    pub selector: SelectorAst,
    pub consume: bool,
    pub inject: Option<InjectOpt>,
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

/// A validation rule parsed from derive macro attributes.
#[derive(Debug, Clone)]
pub enum ValidationRule {
    Min(f64),
    Max(f64),
    Range(f64, f64),
    MultipleOf(f64),
    Positive,
    Negative,
    NonNegative,
    NonPositive,
    MinLen(usize),
    MaxLen(usize),
    Len(usize, usize),
    Pattern(String),
    NonEmpty,
    Ascii,
    Alphanumeric,
    MinItems(usize),
    MaxItems(usize),
    Func(String),
    LessThan(String),
    LessThanOrEqual(String),
    GreaterThan(String),
    GreaterThanOrEqual(String),
    EqualTo(String),
    NotEqualTo(String),
}

impl quote::ToTokens for ValidationRule {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            ValidationRule::Min(v) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::Min(#v) })
            }
            ValidationRule::Max(v) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::Max(#v) })
            }
            ValidationRule::Range(min, max) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::Range(#min, #max) })
            }
            ValidationRule::MultipleOf(v) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::MultipleOf(#v) })
            }
            ValidationRule::Positive => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::Positive })
            }
            ValidationRule::Negative => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::Negative })
            }
            ValidationRule::NonNegative => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::NonNegative })
            }
            ValidationRule::NonPositive => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::NonPositive })
            }
            ValidationRule::MinLen(v) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::MinLen(#v) })
            }
            ValidationRule::MaxLen(v) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::MaxLen(#v) })
            }
            ValidationRule::Len(min, max) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::Len(#min, #max) })
            }
            ValidationRule::Pattern(p) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::Pattern(#p.to_string()) })
            }
            ValidationRule::NonEmpty => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::NonEmpty })
            }
            ValidationRule::Ascii => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::Ascii })
            }
            ValidationRule::Alphanumeric => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::Alphanumeric })
            }
            ValidationRule::MinItems(v) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::MinItems(#v) })
            }
            ValidationRule::MaxItems(v) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::MaxItems(#v) })
            }
            ValidationRule::Func(path) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::Func(#path.to_string()) })
            }
            ValidationRule::LessThan(f) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::LessThan(#f.to_string()) })
            }
            ValidationRule::LessThanOrEqual(f) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::LessThanOrEqual(#f.to_string()) })
            }
            ValidationRule::GreaterThan(f) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::GreaterThan(#f.to_string()) })
            }
            ValidationRule::GreaterThanOrEqual(f) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::GreaterThanOrEqual(#f.to_string()) })
            }
            ValidationRule::EqualTo(f) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::EqualTo(#f.to_string()) })
            }
            ValidationRule::NotEqualTo(f) => {
                tokens.extend(quote! { ::kdl_config::schema::Validation::NotEqualTo(#f.to_string()) })
            }
        }
    }
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
