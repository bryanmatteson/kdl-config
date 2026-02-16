//! KDL configuration loading utilities.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::layer::{LayerMerge, merge_layers_with};
use crate::{
    KdlConfigError, KdlDecode, MergeModifierPolicy, Modifier, Node, ParseConfig, parse_config,
    parse_str_with_config, render_node,
};

/// Errors that can occur while loading config layers.
#[derive(Debug)]
pub enum LoadError {
    Io(std::io::Error),
    Parse(KdlConfigError),
    Layer { id: String, source: Box<LoadError> },
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadError::Io(err) => write!(f, "io error: {err}"),
            LoadError::Parse(err) => write!(f, "parse error: {err}"),
            LoadError::Layer { id, source } => write!(f, "layer '{id}': {source}"),
        }
    }
}

impl std::error::Error for LoadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            LoadError::Io(err) => Some(err),
            LoadError::Parse(err) => Some(err),
            LoadError::Layer { source, .. } => Some(source),
        }
    }
}

impl From<std::io::Error> for LoadError {
    fn from(err: std::io::Error) -> Self {
        LoadError::Io(err)
    }
}

impl From<KdlConfigError> for LoadError {
    fn from(err: KdlConfigError) -> Self {
        LoadError::Parse(err)
    }
}

/// Metadata describing a config layer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LayerMeta {
    pub id: String,
    pub precedence: i32,
    pub tags: Vec<String>,
}

impl LayerMeta {
    pub fn new(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            precedence: 0,
            tags: Vec::new(),
        }
    }

    pub fn with_precedence(mut self, precedence: i32) -> Self {
        self.precedence = precedence;
        self
    }

    pub fn with_tags<I, S>(mut self, tags: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.tags = tags.into_iter().map(Into::into).collect();
        self
    }
}

/// Sources that can contribute a layer.
#[derive(Debug, Clone)]
pub enum LayerInput {
    File(PathBuf),
    Text { id: String, content: String },
    Node(Node),
}

impl LayerInput {
    pub fn file(path: impl Into<PathBuf>) -> Self {
        Self::File(path.into())
    }

    pub fn text(id: impl Into<String>, content: impl Into<String>) -> Self {
        Self::Text {
            id: id.into(),
            content: content.into(),
        }
    }

    pub fn node(node: Node) -> Self {
        Self::Node(node)
    }

    fn default_id(&self) -> String {
        match self {
            LayerInput::File(path) => path.display().to_string(),
            LayerInput::Text { id, .. } => id.clone(),
            LayerInput::Node(_) => "node".to_string(),
        }
    }
}

/// A concrete layer definition with source plus metadata.
#[derive(Debug, Clone)]
pub struct LayerSpec {
    pub input: LayerInput,
    pub meta: LayerMeta,
}

impl LayerSpec {
    pub fn new(input: LayerInput) -> Self {
        let id = input.default_id();
        Self {
            input,
            meta: LayerMeta::new(id),
        }
    }

    pub fn with_meta(input: LayerInput, mut meta: LayerMeta) -> Self {
        if meta.id.trim().is_empty() {
            meta.id = input.default_id();
        }
        Self { input, meta }
    }
}

/// Loaded layer data after parsing an input.
#[derive(Debug, Clone)]
pub struct LoadedLayer {
    pub meta: LayerMeta,
    pub node: Node,
}

/// Provenance event kind.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WinnerKind {
    Node,
    Attr(String),
    Arg(usize),
}

/// Provenance record for the winning layer at a path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WinnerRecord {
    pub path: String,
    pub kind: WinnerKind,
    pub layer_id: String,
    pub precedence: i32,
    pub tags: Vec<String>,
}

/// Layer merge report with winners.
#[derive(Debug, Clone, Default)]
pub struct LoadReport {
    pub layers: Vec<LayerMeta>,
    pub winners: Vec<WinnerRecord>,
}

/// Result bundle for typed load with merged node and provenance report.
#[derive(Debug, Clone)]
pub struct LoadWithReport<T> {
    pub value: T,
    pub merged: Node,
    pub report: LoadReport,
}

/// Load a single KDL file into a parsed Node.
pub fn load_kdl_file(path: &Path) -> Result<Node, LoadError> {
    let contents = std::fs::read_to_string(path)?;
    Ok(parse_config(&contents)?)
}

/// Builder for loading layered configurations from multiple sources.
#[derive(Debug)]
pub struct KdlLoader {
    layers: Vec<LayerSpec>,
    merge_mode: LayerMerge,
}

impl Default for KdlLoader {
    fn default() -> Self {
        Self {
            layers: Vec::new(),
            merge_mode: LayerMerge::ByName,
        }
    }
}

impl KdlLoader {
    /// Create a new loader with no layers.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set merge behavior used by [`load_merged`], [`load`] and report variants.
    pub fn with_merge_mode(mut self, merge_mode: LayerMerge) -> Self {
        self.merge_mode = merge_mode;
        self
    }

    /// Set merge behavior used by [`load_merged`], [`load`] and report variants.
    pub fn set_merge_mode(&mut self, merge_mode: LayerMerge) -> &mut Self {
        self.merge_mode = merge_mode;
        self
    }

    /// Add a file-backed layer (later / higher precedence wins).
    pub fn add_layer(mut self, path: impl Into<PathBuf>) -> Self {
        self.layers.push(LayerSpec::new(LayerInput::file(path)));
        self
    }

    /// Add a file-backed layer (later / higher precedence wins).
    pub fn push_layer(&mut self, path: impl Into<PathBuf>) -> &mut Self {
        self.layers.push(LayerSpec::new(LayerInput::file(path)));
        self
    }

    /// Add an arbitrary layer input with default metadata.
    pub fn add_input(mut self, input: LayerInput) -> Self {
        self.layers.push(LayerSpec::new(input));
        self
    }

    /// Add an arbitrary layer input with default metadata.
    pub fn push_input(&mut self, input: LayerInput) -> &mut Self {
        self.layers.push(LayerSpec::new(input));
        self
    }

    /// Add an arbitrary layer input with explicit metadata.
    pub fn add_with_meta(mut self, input: LayerInput, meta: LayerMeta) -> Self {
        self.layers.push(LayerSpec::with_meta(input, meta));
        self
    }

    /// Add an arbitrary layer input with explicit metadata.
    pub fn push_with_meta(&mut self, input: LayerInput, meta: LayerMeta) -> &mut Self {
        self.layers.push(LayerSpec::with_meta(input, meta));
        self
    }

    /// Load all layers into parsed nodes using effective precedence order.
    pub fn load_layers(&self) -> Result<Vec<Node>, LoadError> {
        Ok(self
            .load_layers_with_meta()?
            .into_iter()
            .map(|layer| layer.node)
            .collect())
    }

    /// Load all layers into parsed nodes with metadata.
    pub fn load_layers_with_meta(&self) -> Result<Vec<LoadedLayer>, LoadError> {
        let mut ordered: Vec<(usize, &LayerSpec)> = self.layers.iter().enumerate().collect();
        ordered.sort_by(|(left_idx, left), (right_idx, right)| {
            left.meta
                .precedence
                .cmp(&right.meta.precedence)
                .then(left_idx.cmp(right_idx))
        });

        let mut loaded = Vec::with_capacity(ordered.len());
        for (_, spec) in ordered {
            let parsed = parse_layer_input(&spec.input).map_err(|source| LoadError::Layer {
                id: spec.meta.id.clone(),
                source: Box::new(source),
            })?;
            loaded.push(LoadedLayer {
                meta: spec.meta.clone(),
                node: parsed,
            });
        }

        Ok(loaded)
    }

    /// Load all layers and merge into a single root node.
    pub fn load_merged(&self) -> Result<Node, LoadError> {
        let layers = self.load_layers()?;
        Ok(merge_layers_with(&layers, self.merge_mode))
    }

    /// Load all layers and parse into a typed config with default parse config.
    pub fn load<T: KdlDecode>(&self) -> Result<T, LoadError> {
        self.load_with_config(&ParseConfig::default())
    }

    /// Load all layers and parse into a typed config with custom parse config.
    pub fn load_with_config<T: KdlDecode>(&self, config: &ParseConfig) -> Result<T, LoadError> {
        Ok(self.load_with_config_and_report(config)?.value)
    }

    /// Load all layers, merge, parse typed config, and emit provenance report.
    pub fn load_with_report<T: KdlDecode>(&self) -> Result<LoadWithReport<T>, LoadError> {
        self.load_with_config_and_report(&ParseConfig::default())
    }

    /// Load all layers, merge, parse typed config, and emit provenance report.
    pub fn load_with_config_and_report<T: KdlDecode>(
        &self,
        config: &ParseConfig,
    ) -> Result<LoadWithReport<T>, LoadError> {
        let loaded_layers = self.load_layers_with_meta()?;
        let layer_nodes: Vec<Node> = loaded_layers
            .iter()
            .map(|layer| layer.node.clone())
            .collect();
        let merged = merge_layers_with(&layer_nodes, self.merge_mode);
        let value = parse_merged_with_config(&merged, config)?;
        let report = build_load_report(&loaded_layers, self.merge_mode);

        Ok(LoadWithReport {
            value,
            merged,
            report,
        })
    }
}

fn parse_layer_input(input: &LayerInput) -> Result<Node, LoadError> {
    match input {
        LayerInput::File(path) => load_kdl_file(path),
        LayerInput::Text { content, .. } => Ok(parse_config(content)?),
        LayerInput::Node(node) => {
            if node.name.is_empty() {
                Ok(node.clone())
            } else {
                let mut root = Node::new();
                root.add_child(node.clone());
                Ok(root)
            }
        }
    }
}

fn parse_merged_with_config<T: KdlDecode>(
    merged: &Node,
    config: &ParseConfig,
) -> Result<T, KdlConfigError> {
    let children = merged.children();
    match children.len() {
        0 => Err(KdlConfigError::custom(
            "KDL Document",
            "expected a single top-level node, found none",
        )),
        1 => {
            let rendered = render_node(&children[0]);
            parse_str_with_config(&rendered, config)
        }
        count => Err(KdlConfigError::custom(
            "KDL Document",
            format!("expected a single top-level node, found {count}"),
        )),
    }
}

#[derive(Debug, Clone)]
struct ProvenanceNode {
    name: String,
    node_layer: usize,
    arg_layers: Vec<usize>,
    attr_layers: HashMap<String, usize>,
    children: Vec<ProvenanceNode>,
}

impl ProvenanceNode {
    fn from_node(node: &Node, layer_index: usize) -> Self {
        let mut attr_layers = HashMap::new();
        for key in node.attrs().keys() {
            attr_layers.insert(key.clone(), layer_index);
        }

        Self {
            name: node.name.clone(),
            node_layer: layer_index,
            arg_layers: vec![layer_index; node.args().len()],
            attr_layers,
            children: node
                .children()
                .iter()
                .map(|child| Self::from_node(child, layer_index))
                .collect(),
        }
    }

    fn merge_with_node(&mut self, other: &Node, layer_index: usize, policy: MergeModifierPolicy) {
        if other.modifier == Modifier::Replace {
            *self = Self::from_node(other, layer_index);
            return;
        }

        self.node_layer = layer_index;

        for _ in other.args() {
            self.arg_layers.push(layer_index);
        }

        for key in other.attrs().keys() {
            self.attr_layers.insert(key.clone(), layer_index);
        }

        for child in other.children() {
            self.merge_child_by_name_with(child, layer_index, policy);
        }
    }

    fn merge_child_by_name_with(
        &mut self,
        child: &Node,
        layer_index: usize,
        policy: MergeModifierPolicy,
    ) {
        match child.modifier {
            Modifier::Remove => {
                self.children.retain(|existing| existing.name != child.name);
            }
            Modifier::Replace => {
                self.children.retain(|existing| existing.name != child.name);
                self.children.push(Self::from_node(child, layer_index));
            }
            Modifier::Append => {
                self.children.push(Self::from_node(child, layer_index));
            }
            Modifier::Flatten => {
                if let Some(existing) = self
                    .children
                    .iter_mut()
                    .find(|existing| existing.name == child.name)
                {
                    existing.merge_with_node(child, layer_index, policy);
                } else {
                    self.children.push(Self::from_node(child, layer_index));
                }
            }
            Modifier::Inherit => {
                if let Some(existing) = self
                    .children
                    .iter_mut()
                    .find(|existing| existing.name == child.name)
                {
                    existing.merge_with_node(child, layer_index, policy);
                } else {
                    self.children.push(Self::from_node(child, layer_index));
                }
            }
        }
    }
}

fn build_load_report(loaded_layers: &[LoadedLayer], mode: LayerMerge) -> LoadReport {
    let preserve = matches!(
        mode,
        LayerMerge::ByNamePreserveModifiers | LayerMerge::AppendPreserveModifiers
    );
    let by_name = matches!(
        mode,
        LayerMerge::ByName | LayerMerge::ByNamePreserveModifiers
    );
    let policy = if preserve {
        MergeModifierPolicy::Preserve
    } else {
        MergeModifierPolicy::Consume
    };

    let mut root_children: Vec<ProvenanceNode> = Vec::new();

    for (layer_index, layer) in loaded_layers.iter().enumerate() {
        for child in layer.node.children() {
            apply_root_child(&mut root_children, child, layer_index, by_name, policy);
        }
    }

    let mut winners = Vec::new();
    collect_winners(&root_children, "", loaded_layers, &mut winners);

    LoadReport {
        layers: loaded_layers
            .iter()
            .map(|layer| layer.meta.clone())
            .collect(),
        winners,
    }
}

fn apply_root_child(
    root_children: &mut Vec<ProvenanceNode>,
    child: &Node,
    layer_index: usize,
    by_name: bool,
    policy: MergeModifierPolicy,
) {
    match child.modifier {
        Modifier::Remove => {
            root_children.retain(|existing| existing.name != child.name);
        }
        Modifier::Replace => {
            root_children.retain(|existing| existing.name != child.name);
            root_children.push(ProvenanceNode::from_node(child, layer_index));
        }
        Modifier::Append => {
            root_children.push(ProvenanceNode::from_node(child, layer_index));
        }
        Modifier::Flatten => {
            merge_root_child_by_name(root_children, child, layer_index, policy);
        }
        Modifier::Inherit => {
            if by_name {
                merge_root_child_by_name(root_children, child, layer_index, policy);
            } else {
                root_children.push(ProvenanceNode::from_node(child, layer_index));
            }
        }
    }
}

fn merge_root_child_by_name(
    root_children: &mut Vec<ProvenanceNode>,
    child: &Node,
    layer_index: usize,
    policy: MergeModifierPolicy,
) {
    if let Some(existing) = root_children
        .iter_mut()
        .find(|existing| existing.name == child.name)
    {
        existing.merge_with_node(child, layer_index, policy);
    } else {
        root_children.push(ProvenanceNode::from_node(child, layer_index));
    }
}

fn collect_winners(
    children: &[ProvenanceNode],
    parent_path: &str,
    layers: &[LoadedLayer],
    winners: &mut Vec<WinnerRecord>,
) {
    let mut sibling_counts: HashMap<&str, usize> = HashMap::new();

    for child in children {
        let index = sibling_counts
            .entry(child.name.as_str())
            .and_modify(|count| *count += 1)
            .or_insert(0);

        let path = if parent_path.is_empty() {
            format!("/{}[{}]", child.name, *index)
        } else {
            format!("{parent_path}/{}[{}]", child.name, *index)
        };

        winners.push(make_winner(
            &path,
            WinnerKind::Node,
            child.node_layer,
            layers,
        ));

        let mut attr_keys: Vec<&String> = child.attr_layers.keys().collect();
        attr_keys.sort_unstable();
        for key in attr_keys {
            if let Some(layer_index) = child.attr_layers.get(key) {
                winners.push(make_winner(
                    &path,
                    WinnerKind::Attr(key.clone()),
                    *layer_index,
                    layers,
                ));
            }
        }

        for (arg_index, layer_index) in child.arg_layers.iter().enumerate() {
            winners.push(make_winner(
                &path,
                WinnerKind::Arg(arg_index),
                *layer_index,
                layers,
            ));
        }

        collect_winners(&child.children, &path, layers, winners);
    }
}

fn make_winner(
    path: &str,
    kind: WinnerKind,
    layer_index: usize,
    layers: &[LoadedLayer],
) -> WinnerRecord {
    let meta = &layers[layer_index].meta;
    WinnerRecord {
        path: path.to_string(),
        kind,
        layer_id: meta.id.clone(),
        precedence: meta.precedence,
        tags: meta.tags.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::{KdlLoader, LayerInput, LayerMeta, WinnerKind, load_kdl_file};
    use crate::{ConflictPolicy, Kdl, ParseConfig};
    use std::fs;
    use std::path::PathBuf;

    #[derive(Debug, Clone, PartialEq, Eq, Kdl)]
    #[kdl(node = "config")]
    struct TestConfig {
        #[kdl(attr)]
        name: String,
    }

    fn write_temp(name: &str, contents: &str) -> PathBuf {
        use std::time::{SystemTime, UNIX_EPOCH};

        let mut path = std::env::temp_dir();
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        path.push(format!(
            "kdl_config_{}_{}_{}.kdl",
            std::process::id(),
            name,
            stamp
        ));
        fs::write(&path, contents).unwrap();
        path
    }

    #[test]
    fn load_single_file() {
        let path = write_temp("single", "config name=\"demo\"");
        let node = load_kdl_file(&path).unwrap();
        assert_eq!(node.children().len(), 1);
    }

    #[test]
    fn load_multiple_layers() {
        let a = write_temp("layer_a", "config name=\"one\"");
        let b = write_temp("layer_b", "config name=\"two\"");
        let loader = KdlLoader::new().add_layer(a).add_layer(b);
        let merged = loader.load_merged().unwrap();
        let config = merged.child("config").unwrap();
        let values = config.attr_values("name").unwrap();
        assert_eq!(values.len(), 2);
    }

    #[test]
    fn load_with_precedence_orders_layers_deterministically() {
        let base = LayerInput::text("base", "config name=\"base\"");
        let workspace = LayerInput::text("workspace", "config name=\"workspace\"");

        let loader = KdlLoader::new()
            .add_with_meta(base, LayerMeta::new("base").with_precedence(10))
            .add_with_meta(workspace, LayerMeta::new("workspace").with_precedence(20));

        let config = ParseConfig {
            default_conflict: ConflictPolicy::Last,
            ..ParseConfig::default()
        };
        let loaded: TestConfig = loader.load_with_config(&config).unwrap();
        assert_eq!(loaded.name, "workspace");
    }

    #[test]
    fn load_with_report_tracks_winning_layer() {
        let loader = KdlLoader::new()
            .add_with_meta(
                LayerInput::text("defaults", "config name=\"base\""),
                LayerMeta::new("defaults").with_precedence(1),
            )
            .add_with_meta(
                LayerInput::text("workspace", "config name=\"workspace\""),
                LayerMeta::new("workspace")
                    .with_precedence(2)
                    .with_tags(["workspace"]),
            );

        let config = ParseConfig {
            default_conflict: ConflictPolicy::Last,
            ..ParseConfig::default()
        };
        let loaded = loader
            .load_with_config_and_report::<TestConfig>(&config)
            .unwrap();
        assert_eq!(loaded.value.name, "workspace");

        let name_winner = loaded
            .report
            .winners
            .iter()
            .find(|winner| {
                winner.path == "/config[0]" && winner.kind == WinnerKind::Attr("name".to_string())
            })
            .expect("name winner is reported");

        assert_eq!(name_winner.layer_id, "workspace");
        assert_eq!(name_winner.precedence, 2);
        assert_eq!(name_winner.tags, vec!["workspace".to_string()]);
    }
}
