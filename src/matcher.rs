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

    let variables = generate_variables(mon_info)?.into_iter().collect();
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

pub fn generate_variables(output: &ConnectedOutput) -> Result<Vec<(&'static str, Value)>> {
    let mode = output
        .preferred_mode
        .as_ref()
        .ok_or_else(|| anyhow!("Output '{}' has no preferred mode", output.name))?;
    let width = mode.resolution.width as u32;
    let height = mode.resolution.height as u32;

    let mm_width = output.dimensions.mm_width;
    let mm_height = output.dimensions.mm_height;

    let edid_hash = output
        .edid_sha256
        .as_ref()
        .cloned()
        .unwrap_or_else(|| "".to_string());

    Ok(vec![
        ("width", Value::Number(width)),
        ("height", Value::Number(height)),
        ("mm_width", Value::Number(mm_width)),
        ("mm_height", Value::Number(mm_height)),
        ("edid_hash", Value::String(edid_hash)),
        ("output", Value::String(output.name.clone())),
        ("xid", Value::Number(output.id)),
    ])
}
