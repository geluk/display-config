use anyhow::Result;

use crate::{evaluator::Evaluator, matcher, parser, xrandr::Xrandr};

pub fn evaluate_expression(randr: &Xrandr, expression: &str) -> Result<()> {
    let rule = parser::parse(expression)?;
    println!("Evaluating expression: '{}'", rule.expression);
    println!("Results per monitor:");
    for monitor in randr.get_connected_outputs()? {
        let variables = matcher::generate_variables_hashmap(&monitor)?;
        let evaluator = Evaluator::new(variables);

        let outcome = evaluator.evaluate_expr(&rule.expression)?;
        println!(" - {} -> {}", monitor.name, outcome);
    }
    Ok(())
}
