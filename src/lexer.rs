use std::fmt::Display;

use anyhow::{anyhow, Result};
use log::trace;
use nom::{
    branch::*,
    bytes::complete::*,
    character,
    combinator::{all_consuming, map, map_parser},
    error::{context, convert_error, ErrorKind, ParseError, VerboseError},
    multi::many0,
    sequence::terminated,
    Err, Finish, IResult,
};

/// Result type for match parsers.
type MResult<'inp, O> = IResult<&'inp str, O, VerboseError<&'inp str>>;

/// All keywords must consist of only these characters.
const KEYWORD_PATTERN: &str = "abcdefghijklmnopqrstuvwxyz";
/// All variable names must consist of only these characters.
const VARIABLE_PATTERN: &str = "abcdefghijklmnopqrstuvwxyz_";

/// A token, as emitted by the lexer.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Token {
    // TODO: emit position information
    Op(Op),
    Sep(Sep),
    Literal(Literal),
    Ident(String),
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Op(op) => write!(f, "{}", op),
            Token::Sep(sep) => write!(f, "{}", sep),
            Token::Literal(lit) => write!(f, "{}", lit),
            Token::Ident(id) => write!(f, "{}", id),
        }
    }
}

/// A literal, representing a value.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Literal {
    Number(u32),
    Bool(bool),
    // TODO: add string and others
}
impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::Bool(b) => write!(f, "{}", b),
        }
    }
}

/// An operator, either binary or unary.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Op {
    Eq,
    Neq,
    Gt,
    Lt,
    Gte,
    Lte,
    And,
    Or,
    Not,
}
impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Eq => "=",
                Op::Neq => "!=",
                Op::Gt => ">",
                Op::Lt => "<",
                Op::Gte => ">=",
                Op::Lte => "<=",
                Op::And => "and",
                Op::Or => "or",
                Op::Not => "not",
            }
        )
    }
}

/// A separator. Currently, only parentheses are used.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Sep {
    LParen,
    RParen,
}
impl Display for Sep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Sep::LParen => "(",
                Sep::RParen => ")",
            }
        )
    }
}

/// Given an input string, attempts to lex it into a token stream. Produces
/// a generic error on failure.
pub fn lex(input: &str) -> Result<Vec<Token>> {
    let result = token_stream(input).finish();
    match result {
        Ok((_, tokens)) => {
            trace!("Rule token stream is: {:?}", tokens);
            Ok(tokens)
        }
        Err(error) => Err(anyhow!(convert_error(input, error))),
    }
}

/// Given an input string, attempts to lex it into a token stream. Produces
/// an error with a parsing context on failure.
fn token_stream(input: &str) -> MResult<Vec<Token>> {
    // Start by eating as much whitespace as possible
    let (input, _) = opt_whitespace(input)?;
    // Now read as many tokens as possible
    let (input, tokens) = many0(token_and_whitespace)(input)?;

    if !input.is_empty() {
        // Token reading stopped, but we still have input left over.
        // This means we've encountered an invalid token. Report it.
        Err(Err::Failure(VerboseError::from_error_kind(
            input,
            ErrorKind::Eof,
        )))
    } else {
        Ok((input, tokens))
    }
}

/// Reads single token, and consumes as much whitespace following that token
/// as possible.
pub fn token_and_whitespace(input: &str) -> MResult<Token> {
    terminated(token, opt_whitespace)(input)
}

/// Reads a single token.
pub fn token(input: &str) -> MResult<Token> {
    let sep = map(separator, Token::Sep);
    let op = map(op, Token::Op);
    let lit = map(literal, Token::Literal);
    let id = map(variable, Token::Ident);

    // Order matters here! Attempting to parse variables before operators, for
    // instance, would result in all text-only operators being recognised as
    // variables, because they also match the variable parsing rules.
    // Because of this, we start with the most specific token types, and then
    // we gradually widen our search.
    alt((sep, op, lit, id))(input)
}

/// Reads a variable. Returns a string containing the variable name.
pub fn variable(input: &str) -> MResult<String> {
    map(is_a(VARIABLE_PATTERN), str::to_string)(input)
}

/// Reads a literal. Returns the value of the literal.
pub fn literal(input: &str) -> MResult<Literal> {
    let number = map(character::complete::u32, Literal::Number);
    let b_true = keyword("true", Literal::Bool(true));
    let b_false = keyword("false", Literal::Bool(false));

    alt((number, b_true, b_false))(input)
}

/// Reads a separator.
pub fn separator(input: &str) -> MResult<Sep> {
    let lbrace = map_token("(", Sep::LParen);
    let rbrace = map_token(")", Sep::RParen);

    alt((lbrace, rbrace))(input)
}

/// Reads an operator.
pub fn op(input: &str) -> MResult<Op> {
    let gte = map_token(">=", Op::Gte);
    let lte = map_token("<=", Op::Lte);
    let eq = map_token("=", Op::Eq);
    let neq = map_token("!=", Op::Neq);
    let gt = map_token(">", Op::Gt);
    let lt = map_token("<", Op::Lt);
    let and = keyword("and", Op::And);
    let or = keyword("or", Op::Or);
    let not = keyword("not", Op::Not);

    context(
        "binary operator (expected any of: =, >, <, >=, <=, 'and', 'or')",
        alt((gte, lte, eq, neq, gt, lt, and, or, not)),
    )(input)
}

/// Maps a tag string to a token.
pub fn map_token<'i, T>(t: &'i str, token: T) -> impl FnMut(&'i str) -> MResult<T>
where
    T: Copy,
{
    map(tag(t), move |_| token)
}

/// Reads as many valid keyword characters as possible, and compares them with
/// the given keyword. Only succeeds if the read string exactly matches the
/// keyword. This prevents 'anders' from being recognised as the keyword 'and',
/// for instance.
pub fn keyword<'i, Tok>(keyword: &'i str, token: Tok) -> impl FnMut(&'i str) -> MResult<Tok>
where
    Tok: Copy,
{
    map(
        map_parser(keyword_pattern, all_consuming(tag(keyword))),
        move |_| token,
    )
}

/// Returns the longest slice that can be considered a valid keyword.
pub fn keyword_pattern(input: &str) -> MResult<&str> {
    is_a(KEYWORD_PATTERN)(input)
}

/// Reads and discards as much whitespace as possible.
pub fn opt_whitespace(input: &str) -> MResult<()> {
    map(take_while(|x: char| x.is_ascii_whitespace()), |_| ())(input)
}

#[cfg(test)]
mod test {
    use super::*;

    fn ident(str: &str) -> Token {
        Token::Ident(String::from(str))
    }

    // token_stream tests
    // ------------------
    #[test]
    fn token_stream_invalid_error() {
        let value = "note > 4 and \\";
        token_stream(value).unwrap_err();
    }

    #[test]
    fn token_stream_complex() {
        let value = "note > 4 and (not test=andy)";
        let (output, result) = token_stream(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(
            result,
            vec![
                ident("note"),
                Token::Op(Op::Gt),
                Token::Literal(Literal::Number(4)),
                Token::Op(Op::And),
                Token::Sep(Sep::LParen),
                Token::Op(Op::Not),
                ident("test"),
                Token::Op(Op::Eq),
                ident("andy"),
                Token::Sep(Sep::RParen),
            ]
        );
    }

    #[test]
    fn token_stream_and_and() {
        let value = "and and  ";
        let (output, result) = token_stream(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, vec![Token::Op(Op::And), Token::Op(Op::And)]);
    }

    // token tests
    // -----------
    #[test]
    fn token_binop_and() {
        let value = "and";
        let (output, result) = token(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Token::Op(Op::And));
    }

    #[test]
    fn token_identifier() {
        let value = "anders";
        let (output, result) = token(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, ident("anders"));
    }

    #[test]
    fn token_delimited() {
        let value = "anders/";
        let (output, result) = token(value).unwrap();
        assert_eq!(output, "/");
        assert_eq!(result, ident("anders"));
    }

    // variable tests
    // --------------
    #[test]
    fn variable_parses() {
        let value = "example_var";
        let (output, result) = variable(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, "example_var");
    }

    #[test]
    fn variable_whitespace_parses_until_whitespace() {
        let value = "end   ";
        let (output, result) = variable(value).unwrap();
        assert_eq!(output, "   ");
        assert_eq!(result, "end");
    }

    #[test]
    fn variable_bounded_parses_until_bound() {
        let value = "end>test";
        let (output, result) = variable(value).unwrap();
        assert_eq!(output, ">test");
        assert_eq!(result, "end");
    }

    // bin_op tests
    // -------------
    #[test]
    fn lt_parses() {
        let value = "<";
        let (output, result) = op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Op::Lt);
    }

    #[test]
    fn gt_parses() {
        let value = ">";
        let (output, result) = op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Op::Gt);
    }

    #[test]
    fn eq_parses() {
        let value = "=";
        let (output, result) = op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Op::Eq);
    }

    #[test]
    fn gte_parses() {
        let value = ">=";
        let (output, result) = op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Op::Gte);
    }

    #[test]
    fn lte_parses() {
        let value = "<=";
        let (output, result) = op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Op::Lte);
    }

    #[test]
    fn and_parses() {
        let value = "and";
        let (output, result) = op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Op::And);
    }

    #[test]
    fn or_parses() {
        let value = "or";
        let (output, result) = op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Op::Or);
    }

    // op tests
    // --------------
    #[test]
    fn op_parses() {
        let value = "not";
        let (output, result) = op(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Op::Not);
    }

    #[test]
    fn op_starts_with_but_invalid_fails() {
        let value = "note";
        let result = op(value).unwrap_err();
        println!("Result: {}", result);
    }

    #[test]
    fn op_invalid_fails() {
        let value = "any";
        let result = op(value).unwrap_err();
        println!("Result: {}", result);
    }

    // opt-whitespace tests
    // --------------------
    #[test]
    fn opt_whitespace_takes_all() {
        let value = "   >=";
        let (output, _) = opt_whitespace(value).unwrap();
        assert_eq!(output, ">=");
    }
}
