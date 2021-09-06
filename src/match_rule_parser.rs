use anyhow::{anyhow, Result};
use nom::{
    branch::*,
    bytes::complete::*,
    character::{self, complete::satisfy},
    combinator::{eof, map},
    error::{context, convert_error, VerboseError, VerboseErrorKind},
    multi::many1,
    Finish, IResult,
};

type MResult<I, O> = IResult<I, O, VerboseError<I>>;

#[derive(Debug)]
pub struct MatchRule {
    pub expression: Expr,
}

#[derive(Debug)]
pub enum Expr {
    Comparison(Cmp),
    CompoundExpr(Box<CompoundExpr>),
}

#[derive(Debug)]
pub struct Cmp {
    pub left: Value,
    pub operator: NumOp,
    pub right: Value,
}

#[derive(Debug)]
pub struct CompoundExpr {
    pub left: Expr,
    pub operator: BoolOp,
    pub right: Expr,
}

#[derive(Debug)]
pub enum Value {
    Variable(String),
    Number(Number),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NumOp {
    Eq,
    Gt,
    Lt,
    Gte,
    Lte,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BoolOp {
    And,
    Or,
}

pub type Number = u32;

pub fn parse(input: &str) -> Result<MatchRule> {
    let result = match_rule(input).finish();
    match result {
        Ok((_, rule)) => Ok(rule),
        Err(error) => {
            // println!("Verbose error: {:#?}", error);
            // TODO: collapse errors: if there is a contextual error referring
            // to the same string as a number of nom errors, drop the nom errors.
            let mut filtered = Vec::new();
            for error in error.errors {
                if filtered.is_empty() {
                    // We don't have any errors yet.
                    filtered.push(error);
                    continue;
                }
                let idx = filtered.len() - 1;
                let last = &filtered[idx];

                if last.0 != error.0 {
                    // This looks like an error for a new string, append it.
                    filtered.push(error);
                    continue;
                }
                if let VerboseErrorKind::Context(_) = error.1 {
                    // We have a context error for this string
                    if let VerboseErrorKind::Context(_) = last.1 {
                        // The last error is also a context error, add our
                        // error to it instead of replacing it.
                        filtered.push(error);
                    } else {
                        // The last error is a generic nom error,
                        // replace it with a context error.
                        filtered[idx] = error;
                    }
                }
            }

            Err(anyhow!(convert_error(
                input,
                VerboseError { errors: filtered }
            )))
        }
    }
}

fn match_rule(input: &str) -> MResult<&str, MatchRule> {
    let (input, _) = opt_whitespace(input)?;
    let (input, expression) = expr(input)?;
    let (input, _) = opt_whitespace(input)?;
    let (input, _) = context(
        "end of input (expected end of input, or another valid expression)",
        eof,
    )(input)?;

    Ok((input, MatchRule { expression }))
}

fn expr(input: &str) -> MResult<&str, Expr> {
    context(
        "expression (expected a valid expression)",
        alt((
            map(compound_expr, |c| Expr::CompoundExpr(Box::new(c))),
            map(cmp, Expr::Comparison),
        )),
    )(input)
}

fn compound_expr(input: &str) -> MResult<&str, CompoundExpr> {
    let (input, left) = cmp(input)?;
    let (input, _) = mnd_whitespace(input)?;
    let (input, operator) = bool_op(input)?;
    let (input, _) = mnd_whitespace(input)?;
    let (input, right) = expr(input)?;

    Ok((
        input,
        CompoundExpr {
            left: Expr::Comparison(left),
            operator,
            right,
        },
    ))
}

fn cmp(input: &str) -> MResult<&str, Cmp> {
    let (input, left) = value(input)?;
    let (input, _) = opt_whitespace(input)?;
    let (input, operator) = number_op(input)?;
    let (input, _) = opt_whitespace(input)?;
    let (input, right) = value(input)?;

    Ok((
        input,
        Cmp {
            left,
            operator,
            right,
        },
    ))
}

fn opt_whitespace(input: &str) -> MResult<&str, ()> {
    map(take_while(|x: char| x.is_ascii_whitespace()), |_| ())(input)
}

fn mnd_whitespace(input: &str) -> MResult<&str, ()> {
    map(many1(satisfy(|x| x.is_ascii_whitespace())), |_| ())(input)
}

fn value(input: &str) -> MResult<&str, Value> {
    context(
        "value (expected either a variable or a number)",
        alt((map(variable, Value::Variable), map(number, Value::Number))),
    )(input)
}

fn variable(input: &str) -> MResult<&str, String> {
    let valid_char = |c: char| ('a'..='z').contains(&c) || c == '_';

    map(many1(satisfy(valid_char)), |res| res.into_iter().collect())(input)
}

fn number(input: &str) -> MResult<&str, Number> {
    character::complete::u32(input)
}

fn bool_op(input: &str) -> MResult<&str, BoolOp> {
    let map_op = |op_str, op| map(tag(op_str), move |_| op);
    let and = map_op("and", BoolOp::And);
    let or = map_op("or", BoolOp::Or);

    context(
        "boolean operator (expected any of: and, or)",
        alt((and, or)),
    )(input)
}

fn number_op(input: &str) -> MResult<&str, NumOp> {
    let map_op = |op_str, op| map(tag(op_str), move |_| op);
    let eq = map_op("=", NumOp::Eq);
    let gt = map_op(">", NumOp::Gt);
    let lt = map_op("<", NumOp::Lt);
    let gte = map_op(">=", NumOp::Gte);
    let lte = map_op("<=", NumOp::Lte);

    context(
        "numeric operator (expected any of: =, >, <, >=, <=)",
        alt((gte, lte, eq, gt, lt)),
    )(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn invalid_bool_op_error() {
        let value = "hoi > 10 what";
        let result = parse(value).unwrap_err();
        println!("Result: {}", result);
    }

    #[test]
    fn invalid_num_op_error() {
        let value = "hoi ! yes";
        let result = parse(value).unwrap_err();
        println!("Result: {}", result);
    }

    #[test]
    fn invalid_value_error() {
        let value = "hoi < <";
        let result = parse(value).unwrap_err();
        println!("Result: {}", result);
    }

    #[test]
    fn invalid_compound_rule_error() {
        let value = "hoi > 10 what";
        let result = parse(value).unwrap_err();
        println!("Result: {}", result);
    }

    #[test]
    fn incomplete_compound_rule_error() {
        let value = "hoi > 10 and 10";
        let result = parse(value).unwrap_err();
        println!("Result: {}", result);
    }

    #[test]
    fn valid_compound_rule() {
        let value = "hoi <= 41 and something_else = 900";
        let (output, result) = expr(value).unwrap();
        println!("Output: '{}', result: {:#?}", output, result);
    }

    #[test]
    fn valid_multiple_compound_rules() {
        let value = "hoi <= 41 and something_else = 900 or test = 10";
        let (output, result) = expr(value).unwrap();
        println!("Output: '{}', result: {:#?}", output, result);
    }

    #[test]
    fn valid_comparison() {
        let value = "hoi > 10";
        let (output, result) = expr(value).unwrap();
        println!("Output: '{}', result: {:#?}", output, result);
    }

    #[test]
    fn valid_comparison_no_whitespace() {
        let value = "hoi>10";
        let (output, result) = cmp(value).unwrap();
        println!("Output: '{}', result: {:#?}", output, result);
    }

    #[test]
    fn valid_comparison_multi_whitespace() {
        let value = "some_thing =  \t  5000";
        let (output, result) = cmp(value).unwrap();
        println!("Output: '{}', result: {:#?}", output, result);
    }

    #[test]
    fn opt_whitespace_takes_all() {
        let value = "   >=";
        let (output, _) = opt_whitespace(value).unwrap();
        assert_eq!(output, ">=");
    }

    #[test]
    fn mnd_whitespace_takes_all() {
        let value = "   yes";
        let (output, _) = mnd_whitespace(value).unwrap();
        assert_eq!(output, "yes");
    }

    #[test]
    fn mnd_whitespace_no_whitespace_given_fails() {
        let value = "yes";
        let res = mnd_whitespace(value);
        assert!(res.is_err());
    }

    #[test]
    fn variable_parses() {
        let value = "example_var";
        let (output, result) = variable(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, "example_var".to_string());
    }

    #[test]
    fn variable_whitespace_parses_until_whitespace() {
        let value = "end   ";
        let (output, result) = variable(value).unwrap();
        assert_eq!(output, "   ");
        assert_eq!(result, "end".to_string());
    }

    #[test]
    fn variable_bounded_parses_until_bound() {
        let value = "end>test";
        let (output, result) = variable(value).unwrap();
        assert_eq!(output, ">test");
        assert_eq!(result, "end".to_string());
    }

    #[test]
    fn or_parses() {
        let value = "or";
        let (output, result) = bool_op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, BoolOp::Or);
    }

    #[test]
    fn lt_parses() {
        let value = "<";
        let (output, result) = number_op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, NumOp::Lt);
    }

    #[test]
    fn gt_parses() {
        let value = ">";
        let (output, result) = number_op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, NumOp::Gt);
    }

    #[test]
    fn eq_parses() {
        let value = "=";
        let (output, result) = number_op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, NumOp::Eq);
    }

    #[test]
    fn gte_parses() {
        let value = ">=";
        let (output, result) = number_op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, NumOp::Gte);
    }

    #[test]
    fn lte_parses() {
        let value = "<=";
        let (output, result) = number_op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, NumOp::Lte);
    }
}
