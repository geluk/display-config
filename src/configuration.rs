use serde::{de::Visitor, Deserialize};

use crate::match_rule_parser::{self, MatchRule};

/// The root of the configuration file.
#[derive(Debug, Deserialize)]
pub struct ConfigurationRoot {
    pub configurations: Vec<Setup>,
}

/// The configuration for a single desktop setup.
#[derive(Debug, Deserialize)]
pub struct Setup {
    pub name: String,
    pub monitors: Vec<MonitorMatch>,
}

/// Matches a single monitor.
#[derive(Debug, Deserialize)]
pub struct MonitorMatch {
    /// The name of the output to which the monitor is connected, as reported by xrandr.
    pub output: Option<String>,
    pub dimensions: Option<MatchRule>,
    pub alias: String,
}

impl<'de> Deserialize<'de> for MatchRule {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_str(MatchRuleVisitor {})
    }
}

struct MatchRuleVisitor;
impl<'de> Visitor<'de> for MatchRuleVisitor {
    type Value = MatchRule;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a string")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        match_rule_parser::parse(v).map_err(|err| E::custom(err.to_string()))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn no_configurations() {
        let config: ConfigurationRoot = serde_yaml::from_str("configurations: []").unwrap();
        println!("Configuration: {:#?}", config);
    }

    #[test]
    fn complex_yaml_ok() {
        let input = r#"
configurations:
- name: example
  monitors:
  - output: DVI-I-1
    dimensions: width >= 1920 and height >= 1080
    alias: m0
  - output: DP-4
    alias: m1
"#;
        let config: ConfigurationRoot = serde_yaml::from_str(input).unwrap();
        println!("Configuration: {:#?}", config);
    }
}
