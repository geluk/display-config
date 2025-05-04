use anyhow::{Context, Result};
use serde::Deserialize;
use serde_json;

use super::{ConnectedOutput, Dimensions, Mode, Output, OutputProvider, Resolution};

pub struct Hyprland;
impl Hyprland {
    pub fn new() -> Self {
        Self {}
    }
}

impl OutputProvider for Hyprland {
    fn get_connected_outputs(&self) -> Result<Vec<ConnectedOutput>> {
        let x = get_from_hyprctl()?;

        Ok(x.into_iter()
            .map(|m| ConnectedOutput {
                id: m.id,
                name: m.name,
                description: Some(m.description),
                dimensions: Dimensions::new(0, 0),
                crtc: None,
                edid: None,
                edid_sha256: None,
                preferred_mode: None,
                supported_modes: m
                    .available_modes
                    .into_iter()
                    .map(|m| Mode {
                        id: 0,
                        resolution: Resolution::new(m.width, m.height),
                        refresh_rate: Some(m.refresh_rate),
                        hsync: None,
                    })
                    .collect(),
            })
            .collect())
    }

    fn get_all_outputs(&self) -> anyhow::Result<Vec<Output>> {
        let outputs = self.get_connected_outputs()?;

        Ok(outputs.into_iter().map(Output::Connected).collect())
    }
}

fn get_from_hyprctl() -> Result<Vec<HyprctlMonitor>> {
    let output = std::process::Command::new("hyprctl")
        .args(["monitors", "-j"])
        .output()
        .context("Failed to execute hyprctl")?;

    if !output.status.success() {
        return Err(anyhow::anyhow!(
            "Failed to get output from hyprctl: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    let monitors = String::from_utf8(output.stdout).context("Failed to parse hyprctl output")?;

    Ok(serde_json::from_str(&monitors)?)
}

#[derive(Debug, Deserialize)]
#[allow(unused)]
struct HyprctlMonitor {
    id: u32,
    name: String,
    description: String,
    make: String,
    model: String,
    serial: String,
    width: u32,
    height: u32,
    #[serde(rename = "refreshRate")]
    refresh_rate: f32,
    x: i32,
    y: i32,
    scale: f32,
    #[serde(rename = "availableModes")]
    available_modes: Vec<HyprctlMode>,
}

#[derive(Debug)]
struct HyprctlMode {
    width: u32,
    height: u32,
    refresh_rate: f32,
}

// Hyprctl returns modes as a list of strings, e.g. "1920x1080@60.00Hz".
// Implement Deserialize for HyprctlMode to parse this format.
impl<'de> Deserialize<'de> for HyprctlMode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        let s = String::deserialize(deserializer)?;
        let parts: Vec<_> = s.split('@').collect();
        if parts.len() != 2 {
            return Err(Error::custom("Invalid mode format"));
        }

        let resolution: Vec<&str> = parts[0].split('x').collect();
        if resolution.len() != 2 {
            return Err(Error::custom("Invalid resolution format"));
        }

        let width = resolution[0].parse().map_err(Error::custom)?;
        let height = resolution[1].parse().map_err(Error::custom)?;

        let refresh_rate = parts[1]
            .trim_end_matches("Hz")
            .parse()
            .map_err(Error::custom)?;

        Ok(HyprctlMode {
            width,
            height,
            refresh_rate,
        })
    }
}
