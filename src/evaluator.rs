//! Match rule evaluator.

use std::{collections::HashMap, fmt::Display};

use anyhow::*;

use crate::{
    lexer::{CmpOp, Literal, Number, Op},
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
impl Value {
    pub fn fmt_bash(&self) -> String {
        match self {
            Value::Number(v) => format!("{}", v),
            Value::Bool(true) => "1".to_string(),
            Value::Bool(false) => "0".to_string(),
            Value::String(v) => v.clone(),
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
    pub fn evaluate_bool(&self, rule: &MatchRule) -> Result<bool> {
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
            Expr::Cmp(cmp) => self.evaluate_cmp(cmp),
        }
    }

    fn evaluate_cmp(&self, cmp: &CmpExpr) -> Result<Value> {
        for (i, op) in cmp.operators.iter().enumerate() {
            let left = cmp
                .operands
                .get(i)
                .ok_or_else(|| anyhow!("Invalid comparison expression"))?;
            let right = cmp
                .operands
                .get(i + 1)
                .ok_or_else(|| anyhow!("Invalid comparison expression"))?;

            // If we want to prevent multiple evaluation in the future,
            // we can fix it here.
            let left = self.evaluate_expr(left)?;
            let right = self.evaluate_expr(right)?;

            let outcome = match (left, right) {
                (Value::Number(l), Value::Number(r)) => match op {
                    CmpOp::Eq => l == r,
                    CmpOp::Neq => l != r,
                    CmpOp::Gt => l > r,
                    CmpOp::Lt => l < r,
                    CmpOp::Gte => l >= r,
                    CmpOp::Lte => l <= r,
                },
                (Value::Bool(l), Value::Bool(r)) => match op {
                    CmpOp::Eq => l == r,
                    CmpOp::Neq => l != r,
                    _ => bail!("Invalid operands for '{}'", op),
                },
                (Value::String(l), Value::String(r)) => match op {
                    CmpOp::Eq => l == r,
                    CmpOp::Neq => l != r,
                    _ => bail!("Invalid operands for '{}'", op),
                },
                (l, r) => {
                    bail!("Unable to compare {} with {}", l.type_name(), r.type_name());
                }
            };
            // We can short-circuit if evaluation fails.
            if !outcome {
                return Ok(Value::Bool(false));
            }
        }
        Ok(Value::Bool(true))
    }

    fn evaluate_binary(&self, bin: &BinExpr) -> Result<Value> {
        let left = self.evaluate_expr(&bin.left)?;
        let right = self.evaluate_expr(&bin.right)?;
        let outcome = match (left, right) {
            (Value::Bool(l), Value::Bool(r)) => Value::Bool(match bin.operator {
                Op::And => l && r,
                Op::Or => l || r,
                Op::Not => bail!("Operator '{}' cannot be used here", bin.operator),
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Exp => {
                    bail!("Invalid operands for '{}'", bin.operator)
                }
            }),
            (Value::Number(l), Value::Number(r)) => Value::Number(match bin.operator {
                Op::Add => l + r,
                Op::Sub => l - r,
                Op::Mul => l * r,
                Op::Div => l / r,
                Op::Exp => l.powf(r),
                Op::And | Op::Or | Op::Not => {
                    bail!(
                        "Cannot use numeric operands with boolean operator '{}'",
                        bin.operator
                    )
                }
            }),
            (_, _) => {
                bail!("Invalid operands for '{}'", bin.operator);
            }
        };

        Ok(outcome)
    }

    fn evaluate_unary(&self, un: &UnExpr) -> Result<Value> {
        let val = self.evaluate_expr(&un.operand)?;
        Ok(match (un.operator, val) {
            (Op::Not, Value::Bool(v)) => Value::Bool(!v),
            (Op::Not, _) => bail!("Invalid operand for '{}'", un.operator),
            _ => bail!("Operator '{}' cannot be used here", un.operator),
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
    fn unbound_variable_error() {
        let variables = HashMap::new();
        let rule = parser::parse("width >= 1920 and height >= 1080").unwrap();
        let evaluator = Evaluator::new(variables);

        evaluator.evaluate_bool(&rule).unwrap_err();
    }

    #[test]
    fn comparison_true() {
        let variables = HashMap::new();
        let rule = parser::parse("1 <= 1 < 2 < 3").unwrap();
        let evaluator = Evaluator::new(variables);

        let res = evaluator.evaluate_bool(&rule).unwrap();
        assert!(res);
    }

    #[test]
    fn comparison_false() {
        let variables = HashMap::new();
        let rule = parser::parse("1 <= 1 < 2 < 2").unwrap();
        let evaluator = Evaluator::new(variables);

        let res = evaluator.evaluate_bool(&rule).unwrap();
        assert!(!res);
    }

    #[test]
    fn comparison_equality_true() {
        let variables = HashMap::new();
        let rule = parser::parse("1 = 1 != 2 = 2").unwrap();
        let evaluator = Evaluator::new(variables);

        let res = evaluator.evaluate_bool(&rule).unwrap();
        assert!(res);
    }

    #[test]
    fn addition_adds() {
        let variables = HashMap::new();
        let rule = parser::parse("100 + 2.1 = 102.1").unwrap();
        let evaluator = Evaluator::new(variables);

        let res = evaluator.evaluate_bool(&rule).unwrap();
        assert!(res);
    }

    #[test]
    fn multiplication_multiplies() {
        let variables = HashMap::new();
        let rule = parser::parse("3.3 * 2.9e2 = 9.57e2").unwrap();
        let evaluator = Evaluator::new(variables);

        let res = evaluator.evaluate_bool(&rule).unwrap();
        assert!(res);
    }

    #[test]
    fn not_negates() {
        let mut variables = HashMap::new();
        variables.insert("width", Value::Number(1920.));
        variables.insert("height", Value::Number(1080.));

        let expr = parser::parse("not 10 > 15 or 10 > 20").unwrap();
        let evaluator = Evaluator::new(variables);
        let res = evaluator.evaluate_bool(&expr).unwrap();

        assert!(res);
    }

    #[test]
    fn expr_is_true() {
        let mut variables = HashMap::new();
        variables.insert("width", Value::Number(1920.));
        variables.insert("height", Value::Number(1080.));

        let expr = parser::parse("width >= 1920 and height >= 1080").unwrap();
        let evaluator = Evaluator::new(variables);
        let res = evaluator.evaluate_bool(&expr).unwrap();

        assert!(res);
    }

    #[test]
    fn expr_is_false() {
        let mut variables = HashMap::new();
        variables.insert("width", Value::Number(1920.));
        variables.insert("height", Value::Number(1080.));

        let expr = parser::parse("width > 1920 or height > 1080").unwrap();
        let evaluator = Evaluator::new(variables);
        let res = evaluator.evaluate_bool(&expr).unwrap();

        assert!(!res);
    }
}
