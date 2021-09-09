use std::{collections::HashMap, fmt::Display};

use anyhow::{anyhow, bail, Result};

use crate::{
    lexer::{Literal, Op},
    parser::*,
};

pub type Variables = HashMap<&'static str, Value>;

#[derive(Debug)]
pub struct Evaluator {
    variables: Variables,
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(Number),
    Bool(bool),
    String(String),
}
impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Value::Number(_) => "number",
            Value::Bool(_) => "boolean",
            Value::String(_) => "string",
        }
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "\"{}\"", v),
        }
    }
}

impl Evaluator {
    pub fn new(variables: Variables) -> Self {
        Self { variables }
    }
    pub fn evaluate(&self, rule: &MatchRule) -> Result<bool> {
        match self.evaluate_expr(&rule.expression)? {
            Value::Bool(b) => Ok(b),
            v => {
                bail!(
                    "Expected match rule to produce a boolean, but got a {}",
                    v.type_name()
                )
            }
        }
    }

    fn evaluate_expr(&self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Binary(op) => self.evaluate_binary(op),
            Expr::Unary(un) => self.evaluate_unary(un),
            Expr::Literal(lit) => self.evaluate_literal(lit),
            Expr::Ident(id) => self.evaluate_ident(id.clone()),
        }
    }

    fn evaluate_binary(&self, bin: &BinExpr) -> Result<Value> {
        let left = self.evaluate_expr(&bin.left)?;
        let right = self.evaluate_expr(&bin.right)?;
        let outcome = match (left, right) {
            (Value::Number(l), Value::Number(r)) => match bin.operator {
                Op::Eq => l == r,
                Op::Neq => l != r,
                Op::Gt => l > r,
                Op::Lt => l < r,
                Op::Gte => l >= r,
                Op::Lte => l <= r,
                _ => bail!("Invalid operands for '{}'", bin.operator),
            },
            (Value::Bool(l), Value::Bool(r)) => match bin.operator {
                Op::And => l && r,
                Op::Or => l || r,
                _ => bail!("Invalid operands for '{}'", bin.operator),
            },
            (Value::String(l), Value::String(r)) => match bin.operator {
                Op::Eq => l == r,
                _ => bail!("Invalid operands for '{}'", bin.operator),
            },
            (l, r) => {
                bail!("Unable to compare {} with {}", l.type_name(), r.type_name());
            }
        };

        Ok(Value::Bool(outcome))
    }

    fn evaluate_unary(&self, un: &UnExpr) -> Result<Value> {
        let val = self.evaluate_expr(&un.right)?;
        Ok(match val {
            Value::Bool(v) => match un.operator {
                Op::Not => Value::Bool(!v),
                _ => bail!("Invalid operand for '{}'", un.operator),
            },
            _ => {
                bail!("Invalid operand for '{}'", un.operator);
            }
        })
    }

    fn evaluate_literal(&self, lit: &Literal) -> Result<Value> {
        match lit {
            Literal::Number(num) => Ok(Value::Number(*num)),
            Literal::Bool(b) => Ok(Value::Bool(*b)),
            Literal::String(str) => Ok(Value::String(str.clone())),
        }
    }

    fn evaluate_ident(&self, id: String) -> Result<Value> {
        self.variables
            .get::<str>(&id)
            .cloned()
            .ok_or_else(|| anyhow!("Unknown variable: '{}'", id))
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
