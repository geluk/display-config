mod configuration;
mod evaluator;
mod lexer;
mod matcher;
mod opt;
mod parser;
mod subcommands;
mod xorg;
mod xrandr;

use anyhow::*;
use log::*;
use opt::Opt;

use crate::xrandr::Xrandr;

fn main() {
    if let Err(error) = entry() {
        error!("Error: {}", error);

        let causes: Vec<_> = error.chain().enumerate().skip(1).collect();
        if !causes.is_empty() {
            error!("\nCaused by: ");
            for (number, cause) in causes {
                error!("  {}: {}", number, cause);
            }
        }
        std::process::exit(1)
    }
}

fn entry() -> Result<()> {
    let opt = Opt::from_args();
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

    trace!("Connecting to X11");
    let connection = xorg::connect()?;
    let randr = Xrandr::new(&connection);

    execute_operation(opt, randr)
}

fn execute_operation(opt: Opt, randr: Xrandr) -> Result<()> {
    match opt.operation {
        opt::Operation::Print {} => {
            subcommands::print::print_configuration(randr.get_all_outputs()?)
        }
        opt::Operation::Apply { dry_run } => {
            if dry_run {
                eprintln!("Dry-run enabled: commands will only be printed, not executed.");
            }
            let config_root = configuration::read(opt.config_file)?;
            subcommands::apply::try_apply(&randr, config_root, dry_run)
        }
        opt::Operation::Eval { expression } => {
            subcommands::eval::evaluate_expression(&randr, &expression.join(" "))
        }
    }
}
