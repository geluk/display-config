#![feature(destructuring_assignment)]

mod configuration;
mod evaluator;
mod lexer;
mod matcher;
mod opt;
mod parser;
mod xorg;
mod xrandr;

use std::fs::File;

use anyhow::Result;
use log::{debug, trace, warn};

use crate::{configuration::ConfigurationRoot, xrandr::Output, xrandr::Xrandr};

fn main() -> Result<()> {
    let opt = opt::Opt::from_args();

    stderrlog::new()
        .timestamp(stderrlog::Timestamp::Microsecond)
        .show_level(false)
        .module(module_path!())
        .verbosity(1 + opt.verbose)
        .init()?;

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
                Some((_setup, monitors)) => {
                    debug!("Found match");
                    for monitor in monitors {
                        debug!("    {} -> {}", monitor.alias, monitor.monitor.name)
                    }
                }
                None => warn!("No setup matches the current configuration"),
            }
        }
    }

    Ok(())
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
                println!("Output {}: {} - xid {}", output.name, dimensions, output.id);

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
                        "   Mode {}: {}×{} ({:.2}Hz){}",
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
