use std::collections::HashMap;

use anyhow::{anyhow, Result};
use log::{debug, error, trace};

use crate::{
    configuration::*,
    evaluator::{self, Value},
    xrandr::ConnectedOutput,
};

pub fn find_matching_setup<'config>(
    monitors: &[ConnectedOutput],
    setups: &'config [Setup],
) -> Result<Option<(&'config Setup, Vec<MatchedMonitor>)>> {
    for setup in setups {
        if let Some(matches) = match_setup(setup, monitors)? {
            return Ok(Some((setup, matches)));
        }
    }
    Ok(None)
}
#[derive(Clone, Debug)]
pub struct MatchedMonitor {
    pub monitor: ConnectedOutput,
    pub alias: String,
}

/// Try to match a single setup.
fn match_setup(setup: &Setup, monitors: &[ConnectedOutput]) -> Result<Option<Vec<MatchedMonitor>>> {
    debug!("Evaluating setup: '{}'", setup.name);
    let mut matched_monitors = Vec::new();
    for mon_match in &setup.require_monitors {
        match match_setup_monitor(mon_match, monitors)? {
            Some(matched_monitor) => {
                matched_monitors.push(matched_monitor);
            }
            None => {
                debug!(
                    "Rejecting '{}' (no matches for ${})",
                    setup.name, mon_match.alias
                );
                return Ok(None);
            }
        }
    }
    Ok(Some(matched_monitors))
}

/// Try to match a single required monitor to all physical monitors.
fn match_setup_monitor(
    mon_match: &RequiredMonitor,
    monitors: &[ConnectedOutput],
) -> Result<Option<MatchedMonitor>> {
    let mut matching_monitors = Vec::new();
    trace!("Trying to match ${}", mon_match.alias);
    for monitor in monitors {
        if match_single_monitor(mon_match, monitor)? {
            matching_monitors.push(monitor);
        }
    }

    match matching_monitors.len() {
        0 => {
            trace!("No matches for ${}", mon_match.alias);
            Ok(None)
        }
        1 => {
            trace!("Found match for ${}", mon_match.alias);
            Ok(Some(MatchedMonitor {
                monitor: (**matching_monitors.first().unwrap()).clone(),
                alias: mon_match.alias.clone(),
            }))
        }
        _ => {
            error!("Multiple matches for ${}", mon_match.alias);
            Err(anyhow!(
                "Multiple matches for '{}': {:?}",
                mon_match.alias,
                matching_monitors
                    .iter()
                    .map(|m| &m.name)
                    .collect::<Vec<_>>()
            ))
        }
    }
}

// Try to match a single required monitor to a single physical monitor.
fn match_single_monitor(mon_match: &RequiredMonitor, mon_info: &ConnectedOutput) -> Result<bool> {
    trace!(
        "  - Checking if {} matches ${}",
        mon_info.name,
        mon_match.alias
    );

    if let Some(name) = &mon_match.output {
        if name == &mon_info.name {
            trace!("    [v] Name matches");
        } else {
            trace!(
                "    [×] Name does not match ('{}' != '{}')",
                mon_info.name,
                mon_match.output.as_ref().unwrap()
            );
            return Ok(false);
        }
    }

    if let Some(output) = &mon_match.output {
        let m = mon_info.name == *output;
        if !m {
            return Ok(false);
        }
    }

    let mode = mon_info
        .preferred_mode
        .as_ref()
        .ok_or_else(|| anyhow!("Output '{}' has no preferred mode", mon_info.name))?;
    let width = mode.resolution.width as u32;
    let height = mode.resolution.height as u32;
    let edid_hash = mon_info
        .edid_sha256
        .as_ref()
        .cloned()
        .unwrap_or_else(|| "".to_string());
    let variables = HashMap::from([
        ("width", Value::Number(width)),
        ("height", Value::Number(height)),
        ("edid_hash", Value::String(edid_hash)),
    ]);
    let evl = evaluator::Evaluator::new(variables);

    for rule in &mon_match.r#match {
        if evl.evaluate(rule)? {
            trace!("    [v] Rule matches: '{}'", rule.expression);
        } else {
            trace!("    [x] Rule does not match: '{}'", rule.expression);
            return Ok(false);
        }
    }

    trace!("    Match found");
    Ok(true)
}
