//! Command line options.

use structopt::StructOpt;

/// Commandline options for the application.
#[derive(Debug, StructOpt)]
#[structopt(
    name = "display-config",
    about = "Automatically configure your desktop based on the displays connected to it."
)]
pub struct Opt {
    #[structopt(subcommand, help = "Which action to perform")]
    pub operation: Operation,

    #[structopt(short, long)]
    pub config_file: Option<String>,

    #[structopt(
        short,
        long,
        parse(from_occurrences),
        help = "Set log verbosity, repeat multiple times to raise"
    )]
    pub verbose: usize,

    #[structopt(long, help = "Include timestamps in log output")]
    pub log_timestamps: bool,
}

#[derive(Debug, StructOpt)]
pub enum Operation {
    #[structopt(help = "Show all connected monitors and supported modes")]
    Print {},
    #[structopt(help = "Find a matching setup from the configuration file and apply it.")]
    Apply {
        #[structopt(
            short,
            long,
            help = "Do not execute any commands, only print what would be done"
        )]
        dry_run: bool,
    },
    Eval {
        #[structopt()]
        expression: Vec<String>,
    },
}

impl Opt {
    pub fn from_args() -> Opt {
        StructOpt::from_args()
    }
}
