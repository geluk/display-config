//! Match rule parser. Takes a token stream and converts it into an AST.

use std::{
    fmt::{self, Display},
    iter,
};

use anyhow::*;
use log::*;

use crate::lexer::{self, CmpOp, Literal, Op, Sep, Token};

#[derive(Debug)]
pub struct MatchRule {
    pub expression: Expr,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Binary(Box<BinExpr>),
    Unary(Box<UnExpr>),
    Cmp(Box<CmpExpr>),
}
impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Ident(id) => write!(f, "{}", id),
            Expr::Binary(bin) => write!(f, "{}", bin),
            Expr::Unary(un) => write!(f, "{}", un),
            Expr::Cmp(cmp) => write!(f, "{}", cmp),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct UnExpr {
    pub operator: Op,
    pub operand: Expr,
}
impl Display for UnExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {})", self.operator, self.operand)
    }
}

#[derive(Debug, PartialEq)]
pub struct BinExpr {
    pub left: Expr,
    pub operator: Op,
    pub right: Expr,
}
impl Display for BinExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug, PartialEq)]
pub struct CmpExpr {
    pub operators: Vec<CmpOp>,
    pub operands: Vec<Expr>,
}
impl CmpExpr {
    pub fn new(operators: Vec<CmpOp>, operands: Vec<Expr>) -> Self {
        if operators.len() != operands.len() - 1 {
            panic!("attempted to construct an invalid comparison expression")
        }
        Self {
            operators,
            operands,
        }
    }
}
impl Display for CmpExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut operands_iter = self.operands.iter();
        let mut operators_iter = self.operators.iter();
        let mut take_operand = false;
        let next = || {
            take_operand = !take_operand;
            if take_operand {
                operands_iter.next().map(|e| e.to_string())
            } else {
                operators_iter.next().map(|o| o.to_string())
            }
        };

        let inner = iter::from_fn(next).collect::<Vec<_>>().join(" ");
        write!(f, "({})", inner)
    }
}

pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Self {
        tokens.reverse();
        Self { tokens }
    }

    pub fn next(&mut self) -> Result<Token> {
        self.tokens
            .pop()
            .ok_or_else(|| anyhow!("Expected another token"))
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.last()
    }

    pub fn expect_sep(&mut self, expected: Sep) -> Result<Sep> {
        if let Token::Sep(sep) = self.next()? {
            if sep == expected {
                Ok(sep)
            } else {
                Err(anyhow!("Expected {}", expected))
            }
        } else {
            Err(anyhow!("Expected {}", expected))
        }
    }
}

pub fn parse(input: &str) -> Result<MatchRule> {
    debug!("Parsing match rule: '{}'", input);
    let tokens = lexer::lex(input)?;

    let expression = match_rule(tokens)?;
    trace!("Match rule parsed to: '{}'", expression);

    Ok(MatchRule { expression })
}

fn match_rule(tokens: Vec<Token>) -> Result<Expr> {
    let mut parser = Parser::new(tokens);
    let expr = expr(&mut parser, 0)?;
    if let Some(token) = parser.peek() {
        bail!("Unexpected token: '{}'", token);
    }
    Ok(expr)
}

fn expr(parser: &mut Parser, prev_p: u8) -> Result<Expr> {
    // We're beginning a new (sub)expression. Let's start by trying to build
    // an atomic expression out of the first token we encounter.
    let token = parser.next()?;
    let lhs = match token {
        // These are easy enough.
        Token::Literal(lit) => Expr::Literal(lit),
        Token::Ident(ident) => Expr::Ident(ident),
        // We've encountered an operator right at the start,
        // try to treat it as unary.
        Token::Op(op) => un_expr(parser, op)?,
        Token::CmpOp(cmp) => {
            bail!(
                "Unexpected comparison operator at expression start: '{}'",
                cmp
            );
        }
        Token::Sep(Sep::LParen) => {
            let lhs = expr(parser, 0)?;
            parser.expect_sep(Sep::RParen)?;
            lhs
        }
        Token::Sep(Sep::RParen) => {
            bail!("Unexpected closing parenthesis at expression start")
        }
    };
    // Now let's finish the expression based on what kind of token comes next.
    let next = parser.peek();
    match next {
        Some(Token::Op(op)) => {
            // Required to satisfy the borrow checker.
            let op = *op;
            bin_expr(parser, lhs, op, prev_p)
        }
        Some(Token::CmpOp(op)) => {
            let op = *op;
            cmp_expr(parser, lhs, op, prev_p)
        }
        None | Some(Token::Sep(Sep::RParen)) => Ok(lhs),
        _ => {
            bail!(
                "Invalid token in expression '{}'",
                next.expect("aieee, parser borked")
            )
        }
    }
}

fn un_expr(parser: &mut Parser, op: Op) -> Result<Expr> {
    // Look up the unary binding power of this operator.
    // If the lookup fails, the operator cannot be used in unary
    // position, and we'll bail.
    let rp = get_unop_power(op)?;
    // Build an expression from anything that comes after it...
    let rhs = expr(parser, rp)?;
    // ...and construct a unary expression from the operator and the
    // expression we just parsed.
    Ok(Expr::Unary(Box::new(UnExpr {
        operator: op,
        operand: rhs,
    })))
}

// Thanks to Aleksey Kladov for his clear breakdown of Pratt's algorithm.
// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
// In short: given an existing LHS, keep folding the next operator and the RHS
// into the LHS as long as that operator is stronger than the previous,
// and return as soon as we encounter a weaker operator.
fn bin_expr(parser: &mut Parser, mut lhs: Expr, mut op: Op, prev_p: u8) -> Result<Expr> {
    loop {
        let (lp, rp) = get_binop_power(op)?;
        if lp < prev_p {
            // The next operator is weaker than the previous one we found,
            // so we'll finish the current expression.
            return Ok(lhs);
        }
        // Due to the binding power rules we've defined, it's not possible for
        // operators to be equally strong, and since we just found that it isn't
        //  weaker, it has to be stronger.
        // That means we should create a new expression based on the lhs we've
        // built up to this point, the token we just discovered, and anything
        // that may come after that token.

        // We know we're going to use the operator now, so we can consume the
        // token.
        parser.next()?;

        // Raise the minimum binding power to that of the rhs,
        // and start evaluating everything that comes after the operator.
        // Depending on precedence, this may instantly produce the next atomic
        // expression, or it may continue to  expand the right-hand-side
        // until it finds a weaker token. Whatever it ends up producing can be
        // used as the rhs for our expression.
        let rhs = expr(parser, rp)?;

        // Now we have a lhs (the expression we were working on),
        // an operator (the token we just consumed),
        // and an rhs (the expression we just got back).
        // Put it all together into a new expression, and replace the original
        // left-hand-side expression with it.
        lhs = Expr::Binary(Box::new(BinExpr {
            left: lhs,
            operator: op,
            right: rhs,
        }));

        op = match parser.peek() {
            Some(Token::Op(op)) => *op,
            Some(Token::Sep(Sep::RParen)) | None => return Ok(lhs),
            Some(Token::CmpOp(op)) => {
                let op = *op;
                return cmp_expr(parser, lhs, op, prev_p);
            }
            Some(token) => bail!("Invalid token in binary expression '{}'", token),
        };
    }
}

fn cmp_expr(parser: &mut Parser, lhs: Expr, mut op: CmpOp, prev_p: u8) -> Result<Expr> {
    let mut exprs = vec![lhs];
    let mut ops = vec![];

    loop {
        let p = get_cmp_power(op)?;
        if p <= prev_p {
            break;
        }
        parser.next()?;

        ops.push(op);
        exprs.push(expr(parser, p)?);

        op = match parser.peek() {
            Some(Token::CmpOp(op)) => *op,
            Some(Token::Op(op)) => {
                let lhs = Expr::Cmp(Box::new(CmpExpr {
                    operators: ops,
                    operands: exprs,
                }));
                let op = *op;
                return bin_expr(parser, lhs, op, prev_p);
            }
            Some(Token::Sep(Sep::RParen)) | None => break,
            Some(token) => bail!("Invalid token in comparison expression '{}'", token),
        };
    }
    Ok(if exprs.len() == 1 {
        exprs.pop().unwrap()
    } else {
        Expr::Cmp(Box::new(CmpExpr::new(ops, exprs)))
    })
}

fn get_binop_power(op: Op) -> Result<(u8, u8)> {
    Ok(match op {
        // `or` and `and` should always have the lowest precedence.
        Op::Or => (1, 2),
        Op::And => (3, 4),
        Op::Add | Op::Sub => (9, 10),
        Op::Mul | Op::Div => (11, 12),
        Op::Exp => (15, 14),
        op => bail!("Not a binary operator: '{}'", op),
    })
}

fn get_unop_power(op: Op) -> Result<u8> {
    Ok(match op {
        Op::Not => 5,
        Op::Add | Op::Sub => 13,
        op => bail!("Not a unary operator: '{}'", op),
    })
}

fn get_cmp_power(op: CmpOp) -> Result<u8> {
    Ok(match op {
        // Because we support comparison operations in the style of a > b = c,
        // all comparison operators are treated equally.
        CmpOp::Eq | CmpOp::Neq | CmpOp::Gt | CmpOp::Lt | CmpOp::Gte | CmpOp::Lte => 7,
    })
}

#[cfg(test)]
mod test {
    use super::*;

    fn tokens(input: &str) -> Vec<Token> {
        lexer::lex(input).unwrap()
    }

    fn valid_match_rule(input: &str) -> Expr {
        super::match_rule(tokens(input)).unwrap()
    }

    fn invalid_match_rule(input: &str) -> Error {
        super::match_rule(tokens(input)).unwrap_err()
    }

    #[test]
    fn unfinished_fails() {
        let err = invalid_match_rule("10 and");
        println!("{}", err);
    }

    #[test]
    fn unbalanced_close_parentheses_fails() {
        let err = invalid_match_rule("(test))");
        println!("{}", err);
    }

    #[test]
    fn stacked_operators_fails() {
        let err = invalid_match_rule("10 > > 4");
        println!("{}", err);
    }

    #[test]
    fn unbalanced_open_parentheses_fails() {
        let err = invalid_match_rule("(test");
        println!("{}", err);
    }

    #[test]
    fn unbalanced_close_parentheses_at_start_fails() {
        let err = invalid_match_rule(")test)");
        println!("{}", err);
    }

    #[test]
    fn invalid_operator_fails() {
        let err = invalid_match_rule("10 not 4");
        println!("{}", err);
    }

    #[test]
    fn stacked_parentheses() {
        let expr = valid_match_rule("((((a))))");
        assert_eq!(expr.to_string(), "a");
    }

    #[test]
    fn addition() {
        let expr = valid_match_rule("a + b and c");
        assert_eq!(expr.to_string(), "((a + b) and c)");
        println!("{}", expr);
    }

    #[test]
    fn big_expression() {
        let expr = valid_match_rule(r#"a and b or c and "something" > 1 > 2 and not 3 > 1 and 2"#);
        println!("{}", expr);
    }

    #[test]
    fn true_or_1_gt_2() {
        let expr = valid_match_rule("true or 1 > 2");
        assert_eq!(expr.to_string(), "(true or (1 > 2))");
    }

    #[test]
    fn and_or_precedence() {
        let expr = valid_match_rule("a and b or c");
        assert_eq!(expr.to_string(), "((a and b) or c)");
    }

    #[test]
    fn or_and_precedence() {
        let expr = valid_match_rule("a or b and c");
        assert_eq!(expr.to_string(), "(a or (b and c))");
    }

    #[test]
    fn comparison_gt_eq() {
        let expr = valid_match_rule("a = b > c");
        assert_eq!(expr.to_string(), "(a = b > c)");
    }

    #[test]
    fn parentheses() {
        let expr = valid_match_rule("a > (b and c)");
        assert_eq!(expr.to_string(), "(a > (b and c))");
    }

    #[test]
    fn simple_comparison() {
        let expr = valid_match_rule("123 > something");
        assert_eq!(expr.to_string(), "(123 > something)");
    }

    #[test]
    fn expr_boolean_gt_correct_precedence() {
        let expr = valid_match_rule("left and 123 > something");
        assert_eq!(expr.to_string(), "(left and (123 > something))");
    }

    #[test]
    fn expr_gt_boolean_correct_precedence() {
        let expr = valid_match_rule("left > 123 and something");
        assert_eq!(expr.to_string(), "((left > 123) and something)");
    }

    #[test]
    fn expr_mathematical_comparison() {
        let expr = valid_match_rule("10 > 9 > 8");
        println!("{:#?}", expr);
        assert_eq!(expr.to_string(), "(10 > 9 > 8)");
    }

    #[test]
    fn expr_div_mult_precedence() {
        let expr = valid_match_rule("a / b * c");
        assert_eq!(expr.to_string(), "((a / b) * c)");
    }

    #[test]
    fn expr_unop_comp() {
        let expr = valid_match_rule("not 10 > 15");
        assert_eq!(expr.to_string(), "(not (10 > 15))");
    }

    #[test]
    fn expr_unop_and() {
        let expr = valid_match_rule("right and not left and right");
        assert_eq!(expr.to_string(), "((right and (not left)) and right)");
    }

    #[test]
    fn expr_literal_parses() {
        let expr = valid_match_rule("123");
        assert_eq!(expr, Expr::Literal(Literal::Number(123.)));
    }
}
