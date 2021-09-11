//! Configuration file related structures and logic.

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
    #[serde(deserialize_with = "string_vec", default)]
    pub apply_commands: Vec<String>,
}

/// Matches a single monitor.
#[derive(Debug, Deserialize)]
pub struct RequiredMonitor {
    #[serde(deserialize_with = "match_rules", default)]
    pub r#match: Vec<MatchRule>,
    pub alias: String,
}

fn match_rules<'de, D>(deserializer: D) -> Result<Vec<MatchRule>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let visitor = StringOrSeqVisitor {
        mapper: parser::parse,
    };

    deserializer.deserialize_any(visitor)
}

fn string_vec<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let visitor = StringOrSeqVisitor {
        mapper: |str| Ok(str.to_string()),
    };

    deserializer.deserialize_any(visitor)
}

/// Parses a sequence of strings, a string, or none to a vector, applying
/// `mapper` to each string, and produces the result.
struct StringOrSeqVisitor<T> {
    mapper: fn(input: &str) -> Result<T>,
}
impl<'de, T> Visitor<'de> for StringOrSeqVisitor<T> {
    type Value = Vec<T>;

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
        Ok(vec![
            (self.mapper)(v).map_err(|err| de::Error::custom(err.to_string()))?
        ])
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let mut items = Vec::new();
        while let Some(elem) = seq.next_element::<&str>()? {
            items.push((self.mapper)(elem).map_err(|err| de::Error::custom(err.to_string()))?);
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
