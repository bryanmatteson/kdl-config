//! KDL configuration loading utilities.

use std::path::{Path, PathBuf};

use crate::layer::{merge_layers, parse_layered_with_config};
use crate::{parse_config, KdlConfigError, KdlDecode, Node, ParseConfig};

/// Errors that can occur while loading config layers.
#[derive(Debug)]
pub enum LoadError {
    Io(std::io::Error),
    Parse(KdlConfigError),
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadError::Io(err) => write!(f, "io error: {err}"),
            LoadError::Parse(err) => write!(f, "parse error: {err}"),
        }
    }
}

impl std::error::Error for LoadError {}

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

/// Load a single KDL file into a parsed Node.
pub fn load_kdl_file(path: &Path) -> Result<Node, LoadError> {
    let contents = std::fs::read_to_string(path)?;
    Ok(parse_config(&contents)?)
}

/// Builder for loading layered configurations from multiple files.
#[derive(Debug, Default)]
pub struct KdlLoader {
    layers: Vec<PathBuf>,
}

impl KdlLoader {
    /// Create a new loader with no layers.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a layer path (later paths take precedence).
    pub fn add_layer(mut self, path: impl Into<PathBuf>) -> Self {
        self.layers.push(path.into());
        self
    }

    /// Add a layer path (later paths take precedence).
    pub fn push_layer(&mut self, path: impl Into<PathBuf>) -> &mut Self {
        self.layers.push(path.into());
        self
    }

    /// Load all layers into parsed nodes.
    pub fn load_layers(&self) -> Result<Vec<Node>, LoadError> {
        let mut nodes = Vec::with_capacity(self.layers.len());
        for path in &self.layers {
            nodes.push(load_kdl_file(path)?);
        }
        Ok(nodes)
    }

    /// Load all layers and merge into a single root node.
    pub fn load_merged(&self) -> Result<Node, LoadError> {
        let layers = self.load_layers()?;
        Ok(merge_layers(&layers))
    }

    /// Load all layers and parse into a typed config with default parse config.
    pub fn load<T: KdlDecode>(&self) -> Result<T, LoadError> {
        self.load_with_config(&ParseConfig::default())
    }

    /// Load all layers and parse into a typed config with custom parse config.
    pub fn load_with_config<T: KdlDecode>(&self, config: &ParseConfig) -> Result<T, LoadError> {
        let layers = self.load_layers()?;
        Ok(parse_layered_with_config(&layers, config)?)
    }
}

#[cfg(test)]
mod tests {
    use super::{load_kdl_file, KdlLoader};
    use std::fs;
    use std::path::PathBuf;

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
}
