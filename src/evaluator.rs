use std::collections::HashMap;

use anyhow::{anyhow, Result};

use crate::match_rule_parser::{BoolOp, Cmp, CompoundExpr, Expr, MatchRule, NumOp, Number, Value};

pub type Variables = HashMap<&'static str, Number>;

pub struct Evaluator {
    variables: Variables,
}

impl Evaluator {
    pub fn new(variables: Variables) -> Self {
        Self { variables }
    }
    pub fn evaluate(&self, rule: &MatchRule) -> Result<bool> {
        self.evaluate_expr(&rule.expression)
    }

    fn evaluate_expr(&self, expr: &Expr) -> Result<bool> {
        match expr {
            Expr::Comparison(cmp) => self.evaluate_comparison(cmp),
            Expr::CompoundExpr(cpd) => self.evaluate_compound(cpd),
        }
    }

    fn evaluate_compound(&self, cpd: &CompoundExpr) -> Result<bool> {
        let left = self.evaluate_expr(&cpd.left)?;
        Ok(match cpd.operator {
            BoolOp::And => left && self.evaluate_expr(&cpd.right)?,
            BoolOp::Or => left || self.evaluate_expr(&cpd.right)?,
        })
    }

    fn evaluate_comparison(&self, cmp: &Cmp) -> Result<bool> {
        let left = self.evaluate_value(&cmp.left)?;
        let right = self.evaluate_value(&cmp.right)?;

        Ok(match cmp.operator {
            NumOp::Eq => left == right,
            NumOp::Gt => left > right,
            NumOp::Lt => left < right,
            NumOp::Gte => left >= right,
            NumOp::Lte => left <= right,
        })
    }

    fn evaluate_value(&self, val: &Value) -> Result<Number> {
        match val {
            Value::Variable(var) => self
                .variables
                .get(var.as_str())
                .copied()
                .ok_or_else(|| anyhow!("Unknown variable: '{}'", var)),
            Value::Number(num) => Ok(*num),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::match_rule_parser;

    use super::*;

    #[test]
    fn evaluate_unbound_variable_error() {
        let variables = HashMap::new();
        let rule = match_rule_parser::parse("width >= 1920 and height >= 1080").unwrap();
        let evaluator = Evaluator::new(variables);

        evaluator.evaluate(&rule).unwrap_err();
    }

    #[test]
    fn evaluate_expr_is_true() {
        let variables = HashMap::from([("width", 1920), ("height", 1080)]);

        let expr = match_rule_parser::parse("width >= 1920 and height >= 1080").unwrap();
        let evaluator = Evaluator::new(variables);
        let res = evaluator.evaluate(&expr).unwrap();

        assert!(res);
    }

    #[test]
    fn evaluate_expr_is_false() {
        let variables = HashMap::from([("width", 1920), ("height", 1080)]);

        let expr = match_rule_parser::parse("width > 1920 or height > 1080").unwrap();
        let evaluator = Evaluator::new(variables);
        let res = evaluator.evaluate(&expr).unwrap();

        assert!(!res);
    }
}
