mod apply;
mod configuration;
mod evaluator;
mod lexer;
mod matcher;
mod opt;
mod parser;
mod print;
mod xorg;
mod xrandr;

use anyhow::{bail, Result};
use log::{error, trace};

use crate::xrandr::Xrandr;

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
            print::print_configuration(randr.get_all_outputs()?)?;
        }
        opt::Operation::Apply => {
            let config_root = configuration::read(opt.config_file)?;
            let matching_setup = matcher::find_matching_setup(
                &randr.get_connected_outputs()?,
                &config_root.configurations,
            )?;

            match matching_setup {
                Some((setup, monitors)) => apply::apply(setup, monitors, opt.dry_run)?,
                None => {
                    bail!("No setup matches the current configuration")
                }
            }
        }
    }

    Ok(())
}
