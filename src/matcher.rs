use std::collections::HashMap;

use anyhow::{anyhow, Result};

use crate::{configuration::*, evaluator, xrandr::Output};

pub fn find_matching_setup<'config>(
    monitors: &[Output],
    setups: &'config [Setup],
) -> Result<Option<(&'config Setup, Vec<MatchedMonitor>)>> {
    let monitors: Vec<&Output> = monitors.iter().filter(|m| m.is_connected()).collect();
    for setup in setups {
        if let Some(matches) = match_setup(setup, &monitors)? {
            return Ok(Some((setup, matches)));
        }
    }
    Ok(None)
}
#[derive(Clone, Debug)]
pub struct MatchedMonitor {
    pub monitor: Output,
    pub alias: String,
}

fn match_setup(setup: &Setup, monitors: &[&Output]) -> Result<Option<Vec<MatchedMonitor>>> {
    let mut matched_monitors = Vec::new();
    for mon_match in &setup.monitors {
        match match_setup_monitor(mon_match, monitors)? {
            Some(matched_monitor) => {
                matched_monitors.push(matched_monitor);
            }
            None => return Ok(None),
        }
    }
    Ok(Some(matched_monitors))
}

fn match_setup_monitor(
    mon_match: &MonitorMatch,
    monitors: &[&Output],
) -> Result<Option<MatchedMonitor>> {
    let mut matching_monitors = Vec::new();
    for monitor in monitors {
        if match_single_monitor(mon_match, monitor)? {
            matching_monitors.push(monitor);
        }
    }

    match matching_monitors.len() {
        0 => Ok(None),
        1 => Ok(Some(MatchedMonitor {
            monitor: (**matching_monitors.first().unwrap()).clone(),
            alias: mon_match.alias.clone(),
        })),
        _ => Err(anyhow!(
            "Multiple matches for '{}': {:?}",
            mon_match.alias,
            matching_monitors
                .iter()
                .map(|m| &m.name)
                .collect::<Vec<_>>()
        )),
    }
}

fn match_single_monitor(mon_match: &MonitorMatch, mon_info: &Output) -> Result<bool> {
    if !mon_match.output.as_ref().map_or(true, |_o| true) {
        // TODO: match against output name
        return Ok(false);
    }

    if let Some(output) = &mon_match.output {
        let m = mon_info.name == *output;
        if !m {
            return Ok(false);
        }
    }

    if let Some(rule) = &mon_match.dimensions {
        let mode = mon_info
            .preferred_mode
            .as_ref()
            .ok_or_else(|| anyhow!("Output '{}' has no preferred mode", mon_info.name))?;

        let width = mode.resolution.width as u32;
        let height = mode.resolution.height as u32;

        let variables = HashMap::from([("width", width), ("height", height)]);

        let evl = evaluator::Evaluator::new(variables);
        evl.evaluate(rule)
    } else {
        Ok(true)
    }
}
