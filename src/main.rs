mod configuration;
mod evaluator;
mod match_rule_parser;
mod matcher;
mod xorg;
mod xrandr;

use std::fs::File;

use anyhow::Result;

use crate::{configuration::ConfigurationRoot, xrandr::Output, xrandr::Xrandr};

fn main() -> Result<()> {
    let file = File::open("config.yml")?;
    let config_root: ConfigurationRoot = serde_yaml::from_reader(file)?;

    let connection = xorg::connect()?;

    println!(
        "Connected to X11 - screen: {}, vendor: {}",
        connection.screen, connection.vendor
    );
    println!(
        "Root: {}×{}",
        connection.root.width_in_pixels, connection.root.height_in_pixels
    );

    let randr = Xrandr::new(&connection);
    let desktop = randr.get_configuration()?;

    let matching_setup = matcher::find_matching_setup(&desktop, &config_root.configurations)?;

    match matching_setup {
        Some((_setup, monitors)) => {
            println!("Found match");
            for monitor in monitors {
                println!("    {} -> {}", monitor.alias, monitor.monitor.name)
            }
        }
        None => println!("No setup matches the current configuration."),
    }

    Ok(())
}

fn _print_configuration(outputs: &[Output]) {
    for output in outputs {
        let dimensions = match &output.dimensions {
            Some(dimensions) => {
                format!("({}mm × {}mm)", dimensions.mm_width, dimensions.mm_height)
            }
            None => "(disconnected)".to_string(),
        };

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
