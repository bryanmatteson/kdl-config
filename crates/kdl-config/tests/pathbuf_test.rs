use std::path::PathBuf;

use kdl_config::derive::KdlNode;
use kdl_config::runtime::parse_str;

#[derive(Debug, PartialEq, KdlNode)]
struct Config {
    #[kdl(attr, keyed)]
    config_path: PathBuf,
}

#[test]
fn test_pathbuf_parsing() {
    let kdl = r#"config config_path="/etc/app/config.toml""#;
    let config: Config = parse_str(kdl).unwrap();
    assert_eq!(config.config_path, PathBuf::from("/etc/app/config.toml"));
}

#[test]
fn test_pathbuf_render() {
    let config = Config {
        config_path: PathBuf::from("/home/user/file.txt"),
    };
    let rendered = kdl_config::runtime::to_kdl(&config, "config");
    assert!(rendered.contains("/home/user/file.txt"));
}
