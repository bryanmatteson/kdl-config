use std::path::PathBuf;

use kdl_config::parse_str;
use kdl_config_derive::KdlNode;

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
    let rendered = kdl_config::to_kdl(&config, "config");
    assert!(rendered.contains("/home/user/file.txt"));
}
