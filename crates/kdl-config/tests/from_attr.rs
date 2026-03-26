use std::sync::Arc;

use kdl_config::Kdl;

// ---------- newtype with From<String> + Into<String> ----------
#[derive(Debug, Clone, PartialEq, Default)]
struct ProviderId(Arc<str>);

impl From<String> for ProviderId {
    fn from(s: String) -> Self {
        Self(Arc::from(s.as_str()))
    }
}

impl From<&str> for ProviderId {
    fn from(s: &str) -> Self {
        Self(Arc::from(s))
    }
}

impl From<ProviderId> for String {
    fn from(p: ProviderId) -> Self {
        p.0.to_string()
    }
}

impl std::fmt::Display for ProviderId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// ---------- newtype with TryFrom<String> ----------
#[derive(Debug, Clone, PartialEq)]
struct Port(u16);

impl TryFrom<i64> for Port {
    type Error = String;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        if value >= 1 && value <= 65535 {
            Ok(Port(value as u16))
        } else {
            Err(format!("port out of range: {value}"))
        }
    }
}

impl From<Port> for i64 {
    fn from(p: Port) -> Self {
        p.0 as i64
    }
}

// ---------- struct using from ----------
#[derive(Debug, Clone, PartialEq, Kdl)]
struct Config {
    #[kdl(attr, from = "String")]
    provider: ProviderId,

    #[kdl(attr, from = "String")]
    optional_provider: Option<ProviderId>,
}

// ---------- struct using try_from ----------
#[derive(Debug, Clone, PartialEq, Kdl)]
struct ServerConfig {
    #[kdl(attr, try_from = "i64")]
    port: Port,
}

// ---------- vec with from ----------
#[derive(Debug, Clone, PartialEq, Kdl)]
struct TagConfig {
    #[kdl(value, from = "String")]
    tags: Vec<ProviderId>,
}

#[test]
fn from_parses_string_to_newtype() {
    let config: Config = kdl_config::parse_str(r#"config provider="openai""#).unwrap();
    assert_eq!(config.provider, ProviderId::from("openai"));
    assert_eq!(config.optional_provider, None);
}

#[test]
fn from_parses_optional() {
    let config: Config =
        kdl_config::parse_str(r#"config provider="openai" optional_provider="anthropic""#).unwrap();
    assert_eq!(config.provider, ProviderId::from("openai"));
    assert_eq!(
        config.optional_provider,
        Some(ProviderId::from("anthropic"))
    );
}

#[test]
fn from_renders_back_to_string() {
    let config = Config {
        provider: ProviderId::from("openai"),
        optional_provider: None,
    };
    let rendered = kdl_config::to_kdl(&config, "config");
    assert!(rendered.contains(r#"provider="openai""#));
}

#[test]
fn try_from_parses_valid_value() {
    let config: ServerConfig = kdl_config::parse_str(r#"server port=8080"#).unwrap();
    assert_eq!(config.port, Port(8080));
}

#[test]
fn try_from_rejects_invalid_value() {
    let result = kdl_config::parse_str::<ServerConfig>(r#"server port=99999"#);
    assert!(result.is_err());
}

#[test]
fn from_vec_parses_elements() {
    let config: TagConfig = kdl_config::parse_str(
        r#"
tag-config {
    tags "alpha"
    tags "beta"
}
"#,
    )
    .unwrap();
    assert_eq!(
        config.tags,
        vec![ProviderId::from("alpha"), ProviderId::from("beta")]
    );
}
