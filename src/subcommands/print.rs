//! Logic for the `print` subcommand.

use anyhow::*;

use crate::{
    matcher,
    xrandr::{ConnectedOutput, Mode, Output},
};

/// Print connected monitors to standard output.
pub fn print_configuration(outputs: Vec<Output>) -> Result<()> {
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
