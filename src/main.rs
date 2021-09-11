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
    io::BufRead,
    process::{Command, Stdio},
};

use anyhow::{anyhow, bail, Result};
use configuration::Setup;
use log::{debug, error, info, trace};
use matcher::MatchedMonitor;
use xrandr::{ConnectedOutput, Mode};

use crate::{xrandr::Output, xrandr::Xrandr};

fn main() {
    if let Err(error) = entry() {
        error!("Error: {}\n", error);
        error!("Caused by: ");
        for (number, cause) in error.chain().enumerate().skip(1) {
            error!("  {}: {}", number, cause);
        }
    }
}

fn entry() -> Result<()> {
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
        eprintln!("Dry-run enabled: commands will only be printed, not executed.");
    }

    trace!("Connecting to X11");
    let connection = xorg::connect()?;
    let randr = Xrandr::new(&connection);

    match opt.operation {
        opt::Operation::Print => {
            print_configuration(randr.get_all_outputs()?)?;
        }
        opt::Operation::Apply => {
            let config_root = configuration::read(opt.config_file)?;
            let matching_setup = matcher::find_matching_setup(
                &randr.get_connected_outputs()?,
                &config_root.configurations,
            )?;

            match matching_setup {
                Some((setup, monitors)) => apply(setup, monitors, opt.dry_run)?,
                None => {
                    bail!("No setup matches the current configuration")
                }
            }
        }
    }

    Ok(())
}

fn apply(setup: &Setup, monitors: Vec<MatchedMonitor>, dry_run: bool) -> Result<()> {
    info!("Applying setup: '{}'", setup.name);
    let mut env = HashMap::new();
    for monitor in monitors.iter() {
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

fn execute_command(command: &str, environment: &HashMap<String, String>) -> Result<()> {
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

fn print_configuration(outputs: Vec<Output>) -> Result<()> {
    for state in outputs {
        match state {
            Output::Disconnected(output) => {
                println!("Output {}: (disconnected)", output.name)
            }
            Output::Unknown(output) => {
                println!("Output {}: (unknown)", output.name)
            }
            Output::Connected(output) => print_connected_output(output)?,
        }
    }
    Ok(())
}

fn print_connected_output(output: ConnectedOutput) -> Result<()> {
    let active = match output.is_active() {
        true => ", active",
        false => ", inactive",
    };

    let dimensions = format!(
        "({}mm × {}mm{})",
        output.dimensions.mm_width, output.dimensions.mm_height, active
    );
    println!("Output {}: {}", output.name, dimensions);

    println!("  Supported modes: ");
    for mode in &output.supported_modes {
        print_mode(mode, &output);
    }

    println!("  To match this output, try using one or more of these variables: ");
    let vars = matcher::generate_variables(&output)?;
    let align_width = vars.iter().map(|(k, _)| k.len()).max().unwrap_or_default();
    for (key, value) in vars.into_iter() {
        println!("    * {:2$} = {}", key, value, align_width);
    }

    Ok(())
}

fn print_mode(mode: &Mode, output: &ConnectedOutput) {
    let active = mode.is_active_on(output);
    let pref = mode.is_preferred_by(output);

    let actpref = match (active, pref) {
        (true, true) => " (active, preferred)",
        (true, false) => " (active)",
        (false, true) => " (preferred)",
        (false, false) => "",
    };

    println!(
        "    * {}×{} ({:.2}Hz){}",
        mode.resolution.width,
        mode.resolution.height,
        mode.refresh_rate.unwrap(),
        actpref,
    )
}
