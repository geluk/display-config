//! Match rule parser. Takes a token stream and converts it into an AST.

use std::fmt::Display;

use anyhow::*;
use log::*;

use crate::lexer::{self, Literal, Op, Sep, Token};

/// Result type for match parsers
type PResult<'a, T> = Result<(&'a [Token], T)>;

pub type Number = u32;

#[derive(Debug)]
pub struct MatchRule {
    pub expression: Expr,
    pub original_input: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Binary(Box<BinExpr>),
    Unary(Box<UnExpr>),
}
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Ident(id) => write!(f, "{}", id),
            Expr::Binary(bin) => write!(f, "{}", bin),
            Expr::Unary(un) => write!(f, "{}", un),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnExpr {
    pub operator: Op,
    pub right: Expr,
}
impl Display for UnExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {})", self.operator, self.right)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BinExpr {
    pub left: Expr,
    pub operator: Op,
    pub right: Expr,
}
impl Display for BinExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
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

    pub fn is_done(&self) -> bool {
        self.tokens.is_empty()
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

    Ok(MatchRule {
        expression,
        original_input: input.to_string(),
    })
}

fn match_rule(tokens: Vec<Token>) -> Result<Expr> {
    let mut parser = Parser::new(tokens);
    let expr = expr(&mut parser, 0)?;
    if let Some(token) = parser.peek() {
        bail!("Unexpected token: '{}'", token);
    }
    Ok(expr)
}

// Thanks to Aleksey Kladov for his clear breakdown of Pratt's algorithm.
// The implementation in this function mostly follows his example.
// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
// TODO: consume tokens instead of taking a reference to them.
fn expr(parser: &mut Parser, prev_p: u8) -> Result<Expr> {
    // We're beginning a new (sub)expression. Let's start by trying to build
    // an atomic expression out of the first token we encounter.
    let token = parser.next()?;
    let mut lhs = match token {
        // These are easy enough.
        Token::Literal(lit) => Expr::Literal(lit),
        Token::Ident(ident) => Expr::Ident(ident),
        // We've encountered an operator right at the start,
        // try to treat it as unary.
        Token::Op(op) => {
            // Look up the unary binding power of this operator.
            // If the lookup fails, the operator cannot be used in unary
            // position, and we'll bail.
            let rp = get_unop_power(op)?;
            // Build an expression from anything that comes after it...
            let rhs = expr(parser, rp)?;
            // ...and construct a unary expression from the operator and the
            // expression we just parsed.
            Expr::Unary(Box::new(UnExpr {
                operator: op,
                right: rhs,
            }))
        }
        Token::Sep(_sep) => {
            let lhs = expr(parser, 0)?;
            parser.expect_sep(Sep::RParen)?;
            lhs
        }
    };

    loop {
        // We've parsed the lhs, now we need to check if there's anything else
        // coming up. We could find an operator, in which case we'll build a
        // binary expression, or we could be at the end of the expression, in
        // which case we'll produce the lhs. If we find anything else, we bail.
        let op = match parser.peek() {
            Some(Token::Op(op)) => *op,
            Some(Token::Sep(Sep::RParen)) | None => return Ok(lhs),
            Some(token) => bail!("Invalid token '{}'", token),
        };
        // We have an operator, let's build a binary expression.
        let (lp, rp) = get_binop_power(op)?;
        if lp < prev_p {
            // The next operator is weaker than the previous one we found,
            // so we'll finish the current expression and leave the next
            // operator to be processed later.
            break;
        }
        // Due to the binding power rules we've defined, it's not possible for
        // the next operator to be equally strong, and since we've already
        // determined that it isn't weaker, it therefore has to be stronger.
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
    }

    Ok(lhs)
}

fn get_unop_power(op: Op) -> Result<u8> {
    Ok(match op {
        Op::Not => 5,
        op => bail!("Not a unary operator: '{}'", op),
    })
}

fn get_binop_power(op: Op) -> Result<(u8, u8)> {
    Ok(match op {
        Op::Or => (1, 2),
        Op::And => (3, 4),
        Op::Eq | Op::Neq => (7, 8),
        Op::Gt | Op::Lt | Op::Gte | Op::Lte => (9, 10),
        op => bail!("Not a binary operator: '{}'", op),
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
    fn unbalanced_closed_parentheses_fails() {
        let err = invalid_match_rule("(test))");
        println!("{}", err);
    }

    #[test]
    fn stacked_operators_fails() {
        let err = invalid_match_rule("10 > > 4");
        println!("{}", err);
    }

    #[test]
    fn unfinished_fails() {
        let err = invalid_match_rule("10 and");
        println!("{}", err);
    }

    #[test]
    fn unbalanced_open_parentheses_fails() {
        let err = invalid_match_rule("(test");
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
    fn expr_parentheses() {
        let expr = valid_match_rule("a > (b and c)");
        assert_eq!(expr.to_string(), "(a > (b and c))");
    }

    #[test]
    fn expr_simple_comparison_parses() {
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
    fn expr_unop_comp_parses() {
        let expr = valid_match_rule("not 10 > 15");
        assert_eq!(expr.to_string(), "(not (10 > 15))");
    }

    #[test]
    fn expr_unop_and_parses() {
        let expr = valid_match_rule("right and not left and right");
        assert_eq!(expr.to_string(), "((right and (not left)) and right)");
    }

    #[test]
    fn expr_literal_parses() {
        let expr = valid_match_rule("123");
        assert_eq!(expr, Expr::Literal(Literal::Number(123)));
    }
}
