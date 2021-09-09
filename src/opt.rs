use structopt::{clap::arg_enum, StructOpt};

/// Commandline options for the application.
#[derive(Debug, StructOpt)]
#[structopt(
    name = "display-config",
    about = "Automatically configure your desktop based on the displays connected to it."
)]
pub struct Opt {
    #[structopt(possible_values = &Operation::variants(), case_insensitive = true, help = "Which action to perform")]
    pub operation: Operation,

    #[structopt(short, long)]
    pub config_file: Option<String>,

    #[structopt(
        short,
        long,
        help = "Do not execute any commands, only print what would be done"
    )]
    pub dry_run: bool,

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

impl Opt {
    pub fn from_args() -> Opt {
        StructOpt::from_args()
    }
}

arg_enum! {
    #[derive(Debug)]
    pub enum Operation {
        Print,
        Apply,
    }
}
