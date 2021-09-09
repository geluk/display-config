use std::{
    env::{self, VarError},
    fs::{self, File},
    path::PathBuf,
};

use anyhow::{bail, Context, Result};
use log::{debug, trace};
use serde::{
    de::{self, Visitor},
    Deserialize,
};

use crate::parser::{self, MatchRule};

const CONFIG_DIR: &str = "display-config";
const CONFIG_FILE: &str = "config.yml";

/// The root of the configuration file.
#[derive(Debug, Deserialize)]
pub struct ConfigurationRoot {
    pub configurations: Vec<Setup>,
}

/// The configuration for a single desktop setup.
#[derive(Debug, Deserialize)]
pub struct Setup {
    pub name: String,
    pub require_monitors: Vec<RequiredMonitor>,
    #[serde(default)]
    pub apply_commands: Vec<String>,
}

/// Matches a single monitor.
#[derive(Debug, Deserialize)]
pub struct RequiredMonitor {
    /// The name of the output to which the monitor is connected, as reported by xrandr.
    pub output: Option<String>,
    #[serde(deserialize_with = "match_rules", default)]
    pub r#match: Vec<MatchRule>,
    pub alias: String,
}

fn match_rules<'de, D>(deserializer: D) -> Result<Vec<MatchRule>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let visitor = MatchRuleVisitor {};

    deserializer.deserialize_any(visitor)
}

struct MatchRuleVisitor;
impl<'de> Visitor<'de> for MatchRuleVisitor {
    type Value = Vec<MatchRule>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a string or a sequence")
    }

    fn visit_none<E>(self) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(vec![])
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        let res = parser::parse(v).map_err(|err| E::custom(err.to_string()))?;
        Ok(vec![res])
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let mut items = Vec::new();
        while let Some(elem) = seq.next_element::<String>()? {
            let res = parser::parse(&elem).map_err(|err| de::Error::custom(err.to_string()))?;
            items.push(res);
        }
        Ok(items)
    }
}

pub fn read(config_path: Option<String>) -> Result<ConfigurationRoot> {
    let config_path = config_path
        .map(PathBuf::from)
        .ok_or(())
        .or_else(|_| default_config_path())
        .context("Could not determine default configuration file path")?;

    if !config_path.exists() {
        bail!(
            "No configuration file found. Please create one at {:?}",
            config_path
        );
    }

    debug!("Opening configuration file: {:?}", config_path);
    let file = File::open(config_path)?;
    trace!("Reading configuration");
    let config_root: ConfigurationRoot = serde_yaml::from_reader(file)?;

    Ok(config_root)
}

fn default_config_path() -> Result<PathBuf> {
    let home = dirs::home_dir();
    let mut config_dir = match (env::var("XDG_CONFIG_HOME"), home) {
        (Ok(path), _) => PathBuf::from(path),
        (Err(VarError::NotPresent), Some(mut home)) => {
            home.push(".config");
            home
        }
        (Err(VarError::NotPresent), None) => {
            bail!("XDG_CONFIG_HOME not set and home directory unknown")
        }
        (Err(VarError::NotUnicode(_)), _) => bail!("XDG_CONFIG_HOME contains non-unicode data"),
    };
    config_dir.push(CONFIG_DIR);

    if !config_dir.exists() {
        fs::create_dir(&config_dir).context(format!(
            "Unable to create configuration directory: {:?}",
            config_dir
        ))?;
    }

    config_dir.push(CONFIG_FILE);
    Ok(config_dir)
}

#[cfg(test)]
mod test {
    use super::*;
    type Result = anyhow::Result<()>;

    #[test]
    fn no_configurations() -> Result {
        let config: ConfigurationRoot = serde_yaml::from_str("configurations: []").unwrap();
        println!("Configuration: {:#?}", config);
        Ok(())
    }

    #[test]
    fn complex_yaml_ok() -> Result {
        let input = r#"
configurations:
- name: example
  require_monitors:
  - output: DVI-I-1
    match: width >= 1920 and height >= 1080
    alias: m0
  - output: DP-4
    alias: m1
"#;
        let config: ConfigurationRoot = serde_yaml::from_str(input)?;
        println!("Configuration: {:#?}", config);
        Ok(())
    }
}
