use anyhow::Result;

use crate::{evaluator::Evaluator, matcher, parser, sources::ConnectedOutput};

pub fn evaluate_expression(outputs: &[ConnectedOutput], expression: &str) -> Result<()> {
    let rule = parser::parse(expression)?;
    println!("Evaluating expression: '{}'", rule.expression);
    println!("Results per monitor:");
    for monitor in outputs {
        let variables = matcher::generate_variables_hashmap(monitor)?;
        let evaluator = Evaluator::new(variables);

        let outcome = evaluator.evaluate_expr(&rule.expression)?;
        println!(" - {} -> {}", monitor.name, outcome);
    }
    Ok(())
}
