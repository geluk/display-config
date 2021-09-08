use std::fmt::Display;

use crate::lexer::{self, Literal, Op, Sep, Token};
use anyhow::{anyhow, bail, Context, Result};
use log::{debug, trace};

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
    let (tokens, expr) = expr(&tokens, 0)?;
    if !tokens.is_empty() {
        bail!("Unexpected token: '{}'", tokens[0]);
    }
    Ok(expr)
}

fn expr(tokens: &[Token], prev_p: u8) -> PResult<Expr> {
    // We're beginning a new (sub)expression. Let's start by trying to build
    // an atomic expression out of the first token.
    let (mut tokens, token) = next(tokens)?;
    let mut lhs = match token {
        // These are easy enough.
        Token::Literal(lit) => Expr::Literal(*lit),
        Token::Ident(ident) => Expr::Ident(ident.clone()),
        // We've encountered an operator right at the start,
        // try to treat it as unary.
        Token::Op(op) => {
            // Calculate its binding power...
            let rp = get_unop_power(*op)?;
            let rhs;
            // ...build an expression from anything that comes after it...
            (tokens, rhs) = expr(tokens, rp)?;
            // ...and construct a unary expression from the operator and the
            // expression we just parsed.
            Expr::Unary(Box::new(UnExpr {
                operator: *op,
                right: rhs,
            }))
        }
        Token::Sep(_sep) => {
            let lhs;
            (tokens, lhs) = expr(tokens, 0)?;
            (tokens, _) = expect(tokens, |t| matches!(t, Token::Sep(Sep::RParen)))
                .context(format!("Expected '{}'", Sep::RParen))?;
            lhs
        }
    };

    loop {
        // The next token should either be a valid operator,
        // or the end of the expression. Let's try looking for that.
        let op = match peek_next_op(tokens)? {
            // We found an operator.
            Some(op) => op,
            // No tokens left, finish the expression.
            None => return Ok((tokens, lhs)),
        };
        // Next, figure out what to do with the operator.
        let (lp, rp) = get_binop_power(op)?;
        if lp < prev_p {
            // The next operator is weaker than the previous one we found,
            // so we'll finish the current expression and leave the next
            // operator to be processed later.
            break;
        }
        // Due to the binding power rules we've defined, it's not possible for
        // the next operator to be equally strong, so it has to be stronger.
        // That means we should create a new expression based on the lhs we've
        // built up to this point, the token we just discovered, and anything
        // that may come after that token.

        // We know we're going to use the operator now, so we can consume the
        // token.
        tokens = &tokens[1..];

        let rhs;
        // Raise the minimum binding power to that of the rhs,
        // and start evaluating everything that comes after the operator.
        // Depending on precedence, this may instantly produce the next atomic
        // expression, or it may continue to  expand the right-hand-side
        // until it finds a weaker token. Whatever it ends up producing can be
        // used as the rhs for our expression.
        (tokens, rhs) = expr(tokens, rp)?;

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

    Ok((tokens, lhs))
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

fn peek_next_op(tokens: &[Token]) -> Result<Option<Op>> {
    let op = match peek(tokens) {
        Some(Token::Op(op)) => Some(*op),
        // Reaching EOF or a closing separator means the expression should
        // finish, which we'll communicate by returning None here.
        None | Some(Token::Sep(Sep::RParen)) => None,
        Some(token) => bail!("Invalid token: {:?} (expected binary operator)", token),
    };
    Ok(op)
}

fn expect<F>(tokens: &[Token], m: F) -> PResult<&Token>
where
    F: Fn(&Token) -> bool,
{
    next(tokens).and_then(move |(tokens, t)| {
        if m(t) {
            Ok((tokens, t))
        } else {
            Err(anyhow!("Unexpected token"))
        }
    })
}

fn peek(tokens: &[Token]) -> Option<&Token> {
    if !tokens.is_empty() {
        Some(&tokens[0])
    } else {
        None
    }
}

fn next(tokens: &[Token]) -> PResult<&Token> {
    tokens
        .get(0)
        .ok_or_else(|| anyhow!("Expected another token"))
        .map(|t| (&tokens[1..], t))
}

#[cfg(test)]
mod test {
    use super::*;

    type TResult = Result<()>;

    #[test]
    fn match_rule_unbalanced_closed_parentheses_fails() {
        let tokens = lexer::lex("(test))").unwrap();
        let err = match_rule(tokens).unwrap_err();
        println!("{}", err);
    }

    #[test]
    fn expr_stacked_operators_fails() {
        let tokens = lexer::lex("10 > > 4").unwrap();
        let err = expr(&tokens, 0).unwrap_err();
        println!("{}", err);
    }

    #[test]
    fn expr_unfinished_fails() {
        let tokens = lexer::lex("10 and").unwrap();
        let err = expr(&tokens, 0).unwrap_err();
        println!("{}", err);
    }

    #[test]
    fn expr_unbalanced_open_parentheses_fails() {
        let tokens = lexer::lex("(test").unwrap();
        let err = expr(&tokens, 0).unwrap_err();
        println!("{}", err);
    }

    #[test]
    fn expr_stacked_parentheses() -> TResult {
        let tokens = lexer::lex("((((a))))")?;
        let (tokens, result) = expr(&tokens, 0)?;
        println!("{}", result);

        assert_eq!(tokens, []);
        assert_eq!(result.to_string(), "a");
        Ok(())
    }

    #[test]
    fn expr_parentheses() -> TResult {
        let tokens = lexer::lex("a > (b and c)")?;
        let (tokens, result) = expr(&tokens, 0)?;
        println!("{}", result);

        assert_eq!(tokens, []);
        assert_eq!(result.to_string(), "(a > (b and c))");
        Ok(())
    }

    #[test]
    fn expr_simple_comparison_parses() -> TResult {
        let tokens = lexer::lex("123 > something")?;
        let (tokens, result) = expr(&tokens, 0)?;
        println!("{}", result);

        assert_eq!(tokens, vec![]);
        assert_eq!(result.to_string(), "(123 > something)");
        Ok(())
    }

    #[test]
    fn expr_boolean_gt_correct_precedence() -> TResult {
        let tokens = lexer::lex("left and 123 > something")?;
        let (tokens, result) = expr(&tokens, 0)?;
        println!("{}", result);

        assert_eq!(tokens, vec![]);
        assert_eq!(result.to_string(), "(left and (123 > something))");
        Ok(())
    }

    #[test]
    fn expr_gt_boolean_correct_precedence() -> TResult {
        let tokens = lexer::lex("left > 123 and something")?;
        let (tokens, result) = expr(&tokens, 0)?;
        println!("{}", result);

        assert_eq!(tokens, vec![]);
        assert_eq!(result.to_string(), "((left > 123) and something)");
        Ok(())
    }

    #[test]
    fn expr_unop_comp_parses() -> TResult {
        let tokens = lexer::lex("not 10 > 15")?;
        let (tokens, result) = expr(&tokens, 0)?;
        println!("{}", result);

        assert_eq!(tokens, vec![]);
        assert_eq!(result.to_string(), "(not (10 > 15))");
        Ok(())
    }

    #[test]
    fn expr_unop_and_parses() -> TResult {
        let tokens = lexer::lex("right and not left and right")?;
        let (tokens, result) = expr(&tokens, 0)?;
        println!("{}", result);

        assert_eq!(tokens, vec![]);
        assert_eq!(result.to_string(), "((right and (not left)) and right)");
        Ok(())
    }

    #[test]
    fn expr_literal_parses() -> TResult {
        let tokens = lexer::lex("123")?;
        let (tokens, result) = expr(&tokens, 0)?;

        assert_eq!(tokens, []);
        assert_eq!(result, Expr::Literal(Literal::Number(123)));
        Ok(())
    }
}
