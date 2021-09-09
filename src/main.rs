#![feature(destructuring_assignment)]

mod configuration;
mod evaluator;
mod lexer;
mod matcher;
mod opt;
mod parser;
mod xorg;
mod xrandr;

use std::{
    collections::HashMap,
    fs::File,
    io::BufRead,
    process::{Command, Stdio},
};

use anyhow::{anyhow, bail, Result};
use configuration::Setup;
use log::{debug, error, info, trace};
use matcher::MatchedMonitor;

use crate::{configuration::ConfigurationRoot, xrandr::Output, xrandr::Xrandr};

fn main() -> Result<()> {
    let opt = opt::Opt::from_args();
    let timestamp = match opt.log_timestamps {
        true => stderrlog::Timestamp::Millisecond,
        false => stderrlog::Timestamp::Off,
    };

    stderrlog::new()
        .timestamp(timestamp)
        .show_level(false)
        .module(module_path!())
        .verbosity(1 + opt.verbose)
        .init()?;

    if opt.dry_run {
        eprintln!("Dry-run enabled: command execution will be simulated.");
    }

    debug!("Opening configuration file");
    let file = File::open("config.yml")?;
    trace!("Reading configuration");
    let config_root: ConfigurationRoot = serde_yaml::from_reader(file)?;
    trace!("Connecting to X11");
    let connection = xorg::connect()?;
    let randr = Xrandr::new(&connection);

    match opt.operation {
        opt::Operation::Print => {
            print_configuration(&randr.get_all_outputs()?);
        }
        opt::Operation::Apply => {
            let matching_setup = matcher::find_matching_setup(
                &randr.get_connected_outputs()?,
                &config_root.configurations,
            )?;

            match matching_setup {
                Some((setup, monitors)) => apply(setup, monitors, opt.dry_run),
                None => {
                    bail!("No setup matches the current configuration")
                }
            }
        }
    }

    Ok(())
}

fn apply(setup: &Setup, monitors: Vec<MatchedMonitor>, dry_run: bool) {
    info!("Applying setup: '{}'", setup.name);
    let mut env = HashMap::new();
    for monitor in monitors.iter() {
        debug!("    {} -> {}", monitor.alias, monitor.monitor.name);
        // env.push((&monitor.alias, &monitor.monitor.name));
        env.insert(monitor.alias.clone(), &monitor.monitor.name);
        env.insert(format!("{}_output", monitor.alias), &monitor.monitor.name);
    }
    if dry_run {
        eprintln!("Environment variables available to commands: {:#?}", env);
    } else {
        debug!("Environment variables available to commands: {:#?}", env);
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
}

fn execute_command(command: &str, environment: &HashMap<String, &String>) -> Result<()> {
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

fn print_configuration(outputs: &[Output]) {
    for state in outputs {
        match state {
            Output::Disconnected(output) => {
                println!("Output {}: (disconnected) - xid {}", output.name, output.id);
            }
            Output::Unknown(output) => {
                println!("Output {}: (unknown) - xid {}", output.name, output.id);
            }
            Output::Connected(output) => {
                let active = match output.is_active() {
                    true => ", active",
                    false => ", inactive",
                };

                let dimensions = format!(
                    "({}mm × {}mm{})",
                    output.dimensions.mm_width, output.dimensions.mm_height, active
                );
                println!("Output {}: {}", output.name, dimensions);
                match &output.edid_sha256 {
                    Some(hash) => println!("    EDID hash: {}", hash),
                    None => println!("    EDID hash: <unavailable>"),
                }

                for mode in &output.supported_modes {
                    let active = mode.is_active_on(output);
                    let pref = mode.is_preferred_by(output);

                    let actpref = match (active, pref) {
                        (true, true) => " (active, preferred)",
                        (true, false) => " (active)",
                        (false, true) => " (preferred)",
                        (false, false) => "",
                    };

                    println!(
                        "    Mode {}: {}×{} ({:.2}Hz){}",
                        mode.id,
                        mode.resolution.width,
                        mode.resolution.height,
                        mode.refresh_rate.unwrap(),
                        actpref,
                    )
                }
            }
        }
    }
}
