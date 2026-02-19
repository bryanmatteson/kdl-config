//! Field-level attribute definitions.

use proc_macro2::Span;

use super::types::{
    BoolMode, ConflictPolicy, DefaultLiteral, DefaultSpec, FlagStyle, InjectOpt, RenderPlacement,
    SchemaTypeOverride, SelectOpts, SelectSpec, SelectorAst, ValidationRule,
};

/// Field placement specification.
#[derive(Debug, Clone, Default)]
pub struct FieldPlacement {
    pub attr: bool,
    pub keyed: bool,
    pub positional: Option<usize>,
    pub positional_list: bool,
    pub flag: Option<(Option<String>, Option<String>)>,
    pub value: bool,
    pub child: bool,
    pub children: bool,
    pub children_any: bool,
    pub registry: bool,
    pub modifier: bool,
}

/// Schema overrides for field-level attributes.
#[derive(Debug, Clone, Default)]
pub struct FieldSchemaOverride {
    /// Skip this field in schema generation.
    pub skip: bool,
    /// Override the schema field name.
    pub name: Option<String>,
    /// Alias for name.
    pub rename: Option<String>,
    /// Override required status.
    pub required: Option<bool>,
    /// Override optional status (inverse of required).
    pub optional: Option<bool>,
    /// Override the schema type (use `kind` instead of `type` since `type` is a Rust keyword).
    pub kind: Option<SchemaTypeOverride>,
    /// Schema description (set by both `description` and `desc` in attributes).
    pub description: Option<String>,
    /// Validation rules parsed from `#[kdl(validate(...))]` or `#[kdl(validate = "...")]`.
    pub validations: Vec<ValidationRule>,
}

impl FieldSchemaOverride {
    /// Get the resolved name.
    pub fn resolved_name(&self) -> Option<&String> {
        self.name.as_ref().or(self.rename.as_ref())
    }

    /// Get the resolved description.
    pub fn resolved_description(&self) -> Option<&String> {
        self.description.as_ref()
    }

    /// Get the resolved required status.
    pub fn resolved_required(&self) -> Option<bool> {
        self.required.or(self.optional.map(|v| !v))
    }

    /// Get the resolved type override.
    pub fn resolved_kind(&self) -> Option<SchemaTypeOverride> {
        self.kind
    }
}

/// Processed field attributes.
#[derive(Debug)]
pub struct FieldAttrs {
    pub span: Span,
    pub placement: FieldPlacement,
    pub required: Option<bool>,
    pub name: Option<String>,
    pub path: Option<FieldPath>,
    pub container: Option<String>,
    pub children_map: bool,
    pub flatten: bool,
    pub map_node: Option<String>,
    pub select: Option<SelectSpec>,
    pub default: Option<DefaultSpec>,
    pub skip: bool,
    pub no_skip_serialize: bool,
    pub skip_serializing_if: Option<String>,
    pub bool_mode: Option<BoolMode>,
    pub flag_style: Option<FlagStyle>,
    pub conflict: Option<ConflictPolicy>,
    pub render: Option<RenderPlacement>,
    pub scalar: bool,
    pub schema: FieldSchemaOverride,
}

/// Positional argument specification (can be index or "rest"/"*").
#[derive(Debug, Clone)]
pub enum PositionalArg {
    Index(usize),
    Rest,
}

impl Default for PositionalArg {
    fn default() -> Self {
        PositionalArg::Index(0)
    }
}

/// Flag attribute - can be bare, or have a string value.
#[derive(Debug, Clone, Default)]
pub struct FlagAttr(pub Option<String>);

/// Default attribute - can be bare, have a literal value, or be absent.
#[derive(Debug, Clone, Default)]
pub enum DefaultAttr {
    #[default]
    Absent,
    Derive,
    Literal(DefaultLiteral),
}

/// Raw parsed field attributes.
///
/// Populated by manual `parse_nested_meta` parsing in `parse.rs`,
/// then converted to `FieldAttrs` via `into_field_attrs()`.
#[derive(Debug, Default)]
pub struct RawFieldAttrs {
    // === Placement flags ===
    pub attr: bool,
    pub keyed: bool,
    pub positional: Option<PositionalArg>,
    pub positional_list: bool,
    pub flag: Option<FlagAttr>,
    pub neg_flag: Option<String>,
    pub value: bool,
    pub child: bool,
    pub children: bool,
    pub children_any: bool,
    pub choice: bool, // alias for children_any
    pub registry: bool,
    pub modifier: bool,

    // === Configuration ===
    pub name: Option<String>,
    pub rename: Option<String>, // alias for name
    pub path: Option<FieldPath>,
    pub container: Option<String>,
    pub children_map: bool,
    pub map_node: Option<String>,
    pub flatten: Option<bool>,

    // === Required/optional ===
    pub required: bool,
    pub optional: bool,

    // === Selectors ===
    pub selector: Option<SelectorAst>,
    pub select: Option<SelectSpec>,
    pub consume: Option<bool>,
    pub inject: Option<InjectOpt>,

    // === Legacy registry keys ===
    pub key_arg: Option<usize>,
    pub key_attr: Option<String>,
    pub key_fn: Option<String>,

    // === Defaults ===
    pub default: DefaultAttr,
    pub default_fn: Option<String>,

    // === Skip ===
    pub skip: bool,
    pub no_skip_serialize: bool,
    pub skip_serializing_if: Option<String>,

    // === Mode overrides ===
    pub bool_mode: Option<BoolMode>,
    pub flag_style: Option<FlagStyle>,
    pub conflict: Option<ConflictPolicy>,
    pub render: Option<RenderPlacement>,

    // === Type hints ===
    pub scalar: bool,
    pub value_type: bool, // alias for scalar
    pub value_like: bool, // alias for scalar
    pub kdl_value: bool,  // alias for scalar

    // === Schema ===
    pub schema: FieldSchemaOverride,

    // === Validation ===
    pub validations: Vec<ValidationRule>,
}

/// Parsed field path for re-rooting decoding.
#[derive(Debug, Clone)]
pub struct FieldPath {
    pub absolute: bool,
    pub segments: Vec<String>,
    pub raw: String,
}

impl FieldPath {
    pub fn parse(raw: &str, span: Span) -> syn::Result<Self> {
        let mut absolute = false;
        let mut path = raw;
        if let Some(rest) = raw.strip_prefix('/') {
            absolute = true;
            path = rest;
        }
        if path.is_empty() {
            return Err(syn::Error::new(span, "path cannot be empty"));
        }
        if path.contains('/') {
            return Err(syn::Error::new(
                span,
                "path may only use '/' as a leading root marker",
            ));
        }
        let segments: Vec<String> = path.split('.').map(|s| s.to_string()).collect();
        if segments.iter().any(|s| s.is_empty()) {
            return Err(syn::Error::new(span, "path segments must be non-empty"));
        }
        Ok(Self {
            absolute,
            segments,
            raw: raw.to_string(),
        })
    }
}

impl RawFieldAttrs {
    /// Convert raw parsed attributes to processed form.
    pub fn into_field_attrs(mut self, span: Span) -> syn::Result<FieldAttrs> {
        // Build placement
        let mut placement = FieldPlacement::default();
        placement.attr = self.attr;
        placement.keyed = self.keyed;
        placement.value = self.value;
        placement.child = self.child;
        placement.children = self.children;
        placement.children_any = self.children_any || self.choice;
        placement.registry = self.registry;
        placement.modifier = self.modifier;

        // Handle positional
        match self.positional {
            Some(PositionalArg::Index(idx)) => placement.positional = Some(idx),
            Some(PositionalArg::Rest) => placement.positional_list = true,
            None => {}
        }
        if self.positional_list {
            placement.positional_list = true;
        }

        // Handle flag
        if let Some(flag_attr) = self.flag {
            let neg = self.neg_flag.clone();
            placement.flag = Some((flag_attr.0, neg));
        } else if self.neg_flag.is_some() {
            placement.flag = Some((None, self.neg_flag.clone()));
        }

        // Resolve required
        let required = if self.required {
            Some(true)
        } else if self.optional {
            Some(false)
        } else {
            None
        };

        // Resolve name
        let name = self.name.or(self.rename);
        let path = self.path;

        // Resolve flatten
        let flatten = self.flatten.unwrap_or(false);

        // Resolve selector (select/selector/key_*)
        let mut select = self.select;
        if let Some(selector) = self.selector {
            if select.is_some() {
                return Err(syn::Error::new(
                    span,
                    "field cannot specify both select(...) and selector=...",
                ));
            }
            select = Some(SelectSpec {
                selector,
                opts: SelectOpts::default(),
            });
        }

        if select.is_none() {
            if let Some(idx) = self.key_arg {
                select = Some(SelectSpec {
                    selector: SelectorAst::Arg(idx as u32),
                    opts: SelectOpts::default(),
                });
            } else if let Some(attr) = self.key_attr {
                select = Some(SelectSpec {
                    selector: SelectorAst::Attr(attr),
                    opts: SelectOpts::default(),
                });
            } else if let Some(func) = self.key_fn {
                select = Some(SelectSpec {
                    selector: SelectorAst::Func(func),
                    opts: SelectOpts::default(),
                });
            }
        }

        if let Some(consume) = self.consume {
            match select.as_mut() {
                Some(spec) => {
                    if spec.opts.consume.is_some() {
                        return Err(syn::Error::new(
                            span,
                            "field cannot specify consume/preserve more than once",
                        ));
                    }
                    spec.opts.consume = Some(consume);
                }
                None => {
                    return Err(syn::Error::new(
                        span,
                        "consume/preserve requires selector or select(...)",
                    ));
                }
            }
        }

        if let Some(inject) = self.inject {
            match select.as_mut() {
                Some(spec) => {
                    if spec.opts.inject.is_some() {
                        return Err(syn::Error::new(
                            span,
                            "field cannot specify inject more than once",
                        ));
                    }
                    spec.opts.inject = Some(inject);
                }
                None => {
                    return Err(syn::Error::new(
                        span,
                        "inject requires selector or select(...)",
                    ));
                }
            }
        }

        if let Some(spec) = select.as_ref() {
            if let Some(InjectOpt::Implicit) = spec.opts.inject {
                match spec.selector {
                    SelectorAst::Attr(ref name) if name == "name" => {}
                    _ => {
                        return Err(syn::Error::new(
                            span,
                            "inject (implicit) is only valid with select(attr(\"name\"), ...)",
                        ));
                    }
                }
            }
        }

        // Resolve default
        let mut default_count = 0u8;
        let default = match self.default {
            DefaultAttr::Absent => {
                if let Some(func) = self.default_fn {
                    default_count += 1;
                    Some(DefaultSpec::Function(func))
                } else {
                    None
                }
            }
            DefaultAttr::Derive => {
                default_count += 1;
                if self.default_fn.is_some() {
                    default_count += 1;
                }
                Some(DefaultSpec::Derive)
            }
            DefaultAttr::Literal(lit) => {
                default_count += 1;
                if self.default_fn.is_some() {
                    default_count += 1;
                }
                Some(DefaultSpec::Literal(lit))
            }
        };

        if default_count > 1 {
            return Err(syn::Error::new(
                span,
                "field cannot have multiple default specifications (`default`, `default = ...`, `default_fn`)",
            ));
        }

        // Resolve scalar
        let scalar = self.scalar || self.value_type || self.value_like || self.kdl_value;

        // Merge top-level validate(...) into schema validations
        if !self.validations.is_empty() {
            self.schema.validations.extend(self.validations);
        }

        Ok(FieldAttrs {
            span,
            placement,
            required,
            name,
            path,
            container: self.container,
            children_map: self.children_map,
            flatten,
            map_node: self.map_node,
            select,
            default,
            skip: self.skip,
            no_skip_serialize: self.no_skip_serialize,
            skip_serializing_if: self.skip_serializing_if,
            bool_mode: self.bool_mode,
            flag_style: self.flag_style,
            conflict: self.conflict,
            render: self.render,
            scalar,
            schema: self.schema,
        })
    }
}
