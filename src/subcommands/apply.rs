//! Logic for the `apply` subcommand.

use anyhow::*;
use log::*;
use std::{
    collections::HashMap,
    io::BufRead,
    process::{Command, Stdio},
};

use crate::{
    configuration::{ConfigurationRoot, Setup},
    matcher::{self, MatchedMonitor},
    xrandr::Xrandr,
};

type Environment = HashMap<String, String>;

/// Tries to apply a setup. Queries Xrandr for available monitors, matches them
/// against the setups in the provided configuration file, then executes the
/// matched setup, or returns an error if there is no matching setup.
pub fn try_apply(randr: &Xrandr, config_root: ConfigurationRoot, dry_run: bool) -> Result<()> {
    let matching_setup =
        matcher::find_matching_setup(&randr.get_connected_outputs()?, &config_root.setups)?;

    match matching_setup {
        Some((setup, monitors)) => apply(setup, monitors, dry_run),
        None => {
            bail!("No setup matches the current configuration")
        }
    }
}

fn apply(setup: &Setup, monitors: Vec<MatchedMonitor>, dry_run: bool) -> Result<()> {
    info!("Applying setup: '{}'", setup.name);
    let mut env = HashMap::new();
    for monitor in monitors.iter() {
        include_variables(&mut env, monitor, dry_run)?;
    }

    if dry_run {
        for cmd in setup.apply_commands.iter() {
            eprintln!("Execute: '{}'", cmd);
        }
    } else {
        let result = setup
            .apply_commands
            .iter()
            .try_for_each(|c| execute_command(c, &env));

        if let Err(err) = result {
            error!("Unable to apply '{}': {}", setup.name, err);
        }
    }
    Ok(())
}

fn include_variables(env: &mut Environment, monitor: &MatchedMonitor, dry_run: bool) -> Result<()> {
    if dry_run {
        eprintln!(
            "  Environment variables for {} ({}):",
            monitor.alias, monitor.monitor.name
        );
    } else {
        debug!("{} = {}", monitor.alias, monitor.monitor.name);
    }

    let vars = matcher::generate_variables(&monitor.monitor)?;
    for (key, value) in vars {
        let key = format!("{}_{}", monitor.alias, key).to_uppercase();
        let value = value.fmt_bash();

        if dry_run {
            eprintln!("    {} = {}", key, value);
        } else {
            debug!("    {} = {}", key, value);
        }
        env.insert(key, value);
    }
    Ok(())
}

fn execute_command(command: &str, environment: &Environment) -> Result<()> {
    debug!("Executing command: '{}'", command);
    let cmd = Command::new("bash")
        .args(&["-c", command])
        .stderr(Stdio::piped())
        .stdout(Stdio::piped())
        .envs(environment)
        .output()?;

    for line in cmd.stdout.lines() {
        let line = line?;
        info!("O> {}", line);
    }
    for line in cmd.stderr.lines() {
        let line = line?;
        info!("E> {}", line);
    }

    match cmd.status.success() {
        true => Ok(()),
        false => Err(anyhow!("Command exited with nonzero status")),
    }
}
