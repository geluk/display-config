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

    #[structopt(
        short,
        long,
        parse(from_occurrences),
        help = "Set log verbosity, repeat multiple times to raise"
    )]
    pub verbose: usize,
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
