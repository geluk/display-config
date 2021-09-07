use std::collections::HashMap;

use anyhow::{anyhow, bail, Result};

use crate::{
    lexer::{Literal, Op},
    parser::*,
};

pub type Variables = HashMap<&'static str, Value>;

pub struct Evaluator {
    variables: Variables,
}

#[derive(Clone, Copy)]
pub enum Value {
    Number(Number),
    Bool(bool),
}

impl Evaluator {
    pub fn new(variables: Variables) -> Self {
        Self { variables }
    }
    pub fn evaluate(&self, rule: &MatchRule) -> Result<bool> {
        match self.evaluate_expr(&rule.expression)? {
            Value::Number(_) => {
                bail!("Expected match rule to produce a boolean, but got a number.")
            }
            Value::Bool(b) => Ok(b),
        }
    }

    fn evaluate_expr(&self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Nested(inner) => self.evaluate_expr(inner),
            Expr::Binary(op) => self.evaluate_binary(op),
            Expr::Unary(un) => self.evaluate_unary(un),
            Expr::Literal(lit) => self.evaluate_literal(*lit),
            Expr::Ident(id) => self.evaluate_ident(id.clone()),
        }
    }

    fn evaluate_binary(&self, bin: &BinExpr) -> Result<Value> {
        let left = self.evaluate_expr(&bin.left)?;
        let right = self.evaluate_expr(&bin.right)?;
        let outcome = match (left, right) {
            (Value::Number(ln), Value::Number(rn)) => match bin.operator {
                Op::Eq => ln == rn,
                Op::Neq => ln != rn,
                Op::Gt => ln > rn,
                Op::Lt => ln < rn,
                Op::Gte => ln >= rn,
                Op::Lte => ln <= rn,
                _ => bail!("Invalid operands for '{}'", bin.operator),
            },
            (Value::Bool(lb), Value::Bool(rb)) => match bin.operator {
                Op::And => lb && rb,
                Op::Or => lb || rb,
                _ => bail!("Invalid operands for '{}'", bin.operator),
            },
            (Value::Number(_), Value::Bool(_)) | (Value::Bool(_), Value::Number(_)) => {
                bail!("Unable to compare boolean with number");
            }
        };

        Ok(Value::Bool(outcome))
    }

    fn evaluate_unary(&self, un: &UnExpr) -> Result<Value> {
        let val = self.evaluate_expr(&un.right)?;
        Ok(match val {
            Value::Number(_) => {
                bail!("Invalid operand for '{}'", un.operator);
            }
            Value::Bool(v) => match un.operator {
                Op::Not => Value::Bool(!v),
                _ => bail!("Invalid operands for '{}'", un.operator),
            },
        })
    }

    fn evaluate_literal(&self, lit: Literal) -> Result<Value> {
        match lit {
            Literal::Number(num) => Ok(Value::Number(num)),
        }
    }

    fn evaluate_ident(&self, id: String) -> Result<Value> {
        Ok(self
            .variables
            .get::<str>(&id)
            .copied()
            .ok_or_else(|| anyhow!("Unknown variable: '{}'", id))?)
    }
}

#[cfg(test)]
mod test {
    use crate::parser;

    use super::*;

    #[test]
    fn evaluate_unbound_variable_error() {
        let variables = HashMap::new();
        let rule = parser::parse("width >= 1920 and height >= 1080").unwrap();
        let evaluator = Evaluator::new(variables);

        evaluator.evaluate(&rule).unwrap_err();
    }

    #[test]
    fn evaluate_not_negates() {
        let variables = HashMap::from([
            ("width", Value::Number(1920)),
            ("height", Value::Number(1080)),
        ]);

        let expr = parser::parse("not 10 > 15 or 10 > 20").unwrap();
        let evaluator = Evaluator::new(variables);
        let res = evaluator.evaluate(&expr).unwrap();

        assert!(res);
    }

    #[test]
    fn evaluate_expr_is_true() {
        let variables = HashMap::from([
            ("width", Value::Number(1920)),
            ("height", Value::Number(1080)),
        ]);

        let expr = parser::parse("width >= 1920 and height >= 1080").unwrap();
        let evaluator = Evaluator::new(variables);
        let res = evaluator.evaluate(&expr).unwrap();

        assert!(res);
    }

    #[test]
    fn evaluate_expr_is_false() {
        let variables = HashMap::from([
            ("width", Value::Number(1920)),
            ("height", Value::Number(1080)),
        ]);

        let expr = parser::parse("width > 1920 or height > 1080").unwrap();
        let evaluator = Evaluator::new(variables);
        let res = evaluator.evaluate(&expr).unwrap();

        assert!(!res);
    }
}
