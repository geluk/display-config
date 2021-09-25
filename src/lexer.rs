//! Match rule lexer. Converts the string representation of a match rule into
//! a token stream.

use std::fmt::{self, Display};

use anyhow::*;
use log::*;
use nom::{
    branch::*,
    bytes::complete::*,
    character::{self, complete::one_of},
    combinator::*,
    error::*,
    multi::many0,
    sequence::*,
    Err, Finish, IResult,
};
use sha2::digest::generic_array::typenum::Pow;

/// Result type for match parsers.
type MResult<'inp, O> = IResult<&'inp str, O, VerboseError<&'inp str>>;

/// All keywords must consist of only these characters.
const KEYWORD_PATTERN: &str = "abcdefghijklmnopqrstuvwxyz";
/// All variable names must consist of only these characters.
const VARIABLE_PATTERN: &str = "abcdefghijklmnopqrstuvwxyz_";

/// A token, as emitted by the lexer.
#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    // TODO: emit position information
    Op(Op),
    CmpOp(CmpOp),
    Sep(Sep),
    Literal(Literal),
    Ident(String),
}
impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Op(t) => write!(f, "{}", t),
            Token::CmpOp(t) => write!(f, "{}", t),
            Token::Sep(t) => write!(f, "{}", t),
            Token::Literal(t) => write!(f, "{}", t),
            Token::Ident(t) => write!(f, "{}", t),
        }
    }
}

/// A literal, representing a value.
#[derive(Clone, PartialEq, Debug)]
pub enum Literal {
    Number(Number),
    Bool(bool),
    String(String),
}
impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::String(s) => write!(f, "\"{}\"", s.replace("\"", "\\\"")),
        }
    }
}

pub type Number = f64;

/// A comparison operator, either binary or unary.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CmpOp {
    Eq,
    Neq,
    Gt,
    Lt,
    Gte,
    Lte,
}
impl Display for CmpOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CmpOp::Eq => "=",
                CmpOp::Neq => "!=",
                CmpOp::Gt => ">",
                CmpOp::Lt => "<",
                CmpOp::Gte => ">=",
                CmpOp::Lte => "<=",
            }
        )
    }
}

/// An operator, either binary or unary.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Op {
    And,
    Or,
    Not,
    Add,
    Sub,
    Mul,
    Div,
    Exp,
}
impl Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::And => "and",
                Op::Or => "or",
                Op::Not => "not",
                Op::Add => "+",
                Op::Sub => "-",
                Op::Mul => "*",
                Op::Div => "/",
                Op::Exp => "^",
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
        let ctx = VerboseError {
            errors: vec![(input, VerboseErrorKind::Context("invalid token"))],
        };
        Err(Err::Failure(ctx))
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
    let cmp_op = map(comparison_operator, Token::CmpOp);
    let op = map(operator, Token::Op);
    let lit = map(literal, Token::Literal);
    let id = map(variable, Token::Ident);

    // Order matters here! Attempting to parse variables before operators, for
    // instance, would result in all text-only operators being recognised as
    // variables, because they also match the variable parsing rules.
    // Because of this, we start with the most specific token types, and then
    // we gradually widen our search.
    alt((sep, cmp_op, op, lit, id))(input)
}

/// Reads a variable. Returns a string containing the variable name.
pub fn variable(input: &str) -> MResult<String> {
    map(is_a(VARIABLE_PATTERN), str::to_string)(input)
}

/// Reads a literal. Returns the value of the literal.
pub fn literal(input: &str) -> MResult<Literal> {
    let number = map(number, Literal::Number);
    let b_true = map(keyword("true"), |_| Literal::Bool(true));
    let b_false = map(keyword("false"), |_| Literal::Bool(false));
    let string = map(string, Literal::String);

    alt((number, b_true, b_false, string))(input)
}

/// Reads a string literal.
pub fn string(input: &str) -> MResult<String> {
    fn resolve_escapes(escaped: &str) -> String {
        escaped
            .replace(r#"\\"#, "\\")
            .replace(r#"\""#, "\"")
            .replace(r#"\r"#, "\r")
            .replace(r#"\n"#, "\n")
            .replace(r#"\t"#, "\t")
    }
    map(
        delimited(
            // Start with a quote
            tag(r#"""#),
            escaped(
                // This tag matches the normal (non-control) characters.
                is_not(r#"\""#),
                // This tag matches the control character.
                '\\',
                // This tag matches the escaped characters:
                // \   : because it's already used as a control character
                // "   : because we need to be able to escape quotes
                // rnt : to provide an easy way to insert common whitespace characters
                one_of(r#"\"rnt"#),
            ),
            tag(r#"""#),
        ),
        resolve_escapes,
    )(input)
}

/// Reads a number literal.
pub fn number(input: &str) -> MResult<f64> {
    let (input, mut whole) = map(character::complete::i64, |v| v as f64)(input)?;
    let (input, fraction) = opt(preceded(tag("."), consumed(character::complete::u64)))(input)?;
    let (input, exponent) = opt(preceded(tag("e"), character::complete::i32))(input)?;

    if let Some((text, fraction)) = fraction {
        let char_count = text.len() as i32;
        let fraction = fraction as f64 / 10f64.powi(char_count);
        whole = whole + fraction;
    }
    if let Some(exponent) = exponent {
        whole = whole * 10f64.powi(exponent);
    }
    return Ok((input, whole));
}

/// Reads a separator.
pub fn separator(input: &str) -> MResult<Sep> {
    let lbrace = map_token("(", Sep::LParen);
    let rbrace = map_token(")", Sep::RParen);

    alt((lbrace, rbrace))(input)
}

/// Reads a comparison operator.
pub fn comparison_operator(input: &str) -> MResult<CmpOp> {
    let gte = map_token(">=", CmpOp::Gte);
    let lte = map_token("<=", CmpOp::Lte);
    let neq = map_token("!=", CmpOp::Neq);
    let eq = map_token("=", CmpOp::Eq);
    let gt = map_token(">", CmpOp::Gt);
    let lt = map_token("<", CmpOp::Lt);

    alt((gte, lte, neq, eq, gt, lt))(input)
}

/// Reads an operator.
pub fn operator(input: &str) -> MResult<Op> {
    let and = map(keyword("and"), |_| Op::And);
    let or = map(keyword("or"), |_| Op::Or);
    let not = map(keyword("not"), |_| Op::Not);
    let add = map_token("+", Op::Add);
    let sub = map_token("-", Op::Sub);
    let mul = map_token("*", Op::Mul);
    let div = map_token("/", Op::Div);
    let exp = map_token("^", Op::Exp);

    alt((and, or, not, add, sub, mul, div, exp))(input)
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
pub fn keyword<'i>(keyword: &'i str) -> impl FnMut(&'i str) -> MResult<&'i str> {
    map_parser(keyword_pattern, all_consuming(tag(keyword)))
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
        let error = token_stream(value).unwrap_err();
        println!("{}", error);
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
                Token::CmpOp(CmpOp::Gt),
                Token::Literal(Literal::Number(4.)),
                Token::Op(Op::And),
                Token::Sep(Sep::LParen),
                Token::Op(Op::Not),
                ident("test"),
                Token::CmpOp(CmpOp::Eq),
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

    // literal tests
    // -------------
    #[test]
    fn literal_number_parses() {
        let value = "12320";
        let (output, result) = literal(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Literal::Number(12320.));
    }

    #[test]
    fn literal_bool_true_parses() {
        let value = "true";
        let (output, result) = literal(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Literal::Bool(true));
    }

    #[test]
    fn literal_bool_false_parses() {
        let value = "false";
        let (output, result) = literal(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Literal::Bool(false));
    }

    #[test]
    fn literal_string_parses() {
        let value = r#""a\" test\\ \n\rstring""#;
        let (output, result) = literal(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(
            result,
            Literal::String(String::from("a\" test\\ \n\rstring"))
        );
    }

    // string tests
    // ------------
    #[test]
    fn string_parses() {
        let value = r#""Hello there!""#;
        let (output, result) = string(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, "Hello there!");
    }

    #[test]
    fn string_escaped_backslash_parses() {
        let value = r#""a \\ backslash""#;
        let (output, result) = string(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, "a \\ backslash");
    }

    #[test]
    fn string_escaped_quote_parses() {
        let value = r#""a \" double quote""#;
        let (output, result) = string(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, "a \" double quote");
    }

    #[test]
    fn string_escape_unclosed_fails() {
        let value = r#""\""#;
        let error = string(value).unwrap_err();
        println!("{}", error);
    }

    #[test]
    fn string_invalid_escape_fails() {
        let value = r#""\y""#;
        let error = string(value).unwrap_err();
        println!("{}", error);
    }

    #[test]
    fn string_no_quotes_fails() {
        let value = r#"yes"#;
        let error = string(value).unwrap_err();
        println!("{}", error);
    }

    // comparison operator tests
    // -------------------------
    #[test]
    fn lt_parses() {
        let value = "<";
        let (output, result) = comparison_operator(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, CmpOp::Lt);
    }

    #[test]
    fn gt_parses() {
        let value = ">";
        let (output, result) = comparison_operator(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, CmpOp::Gt);
    }

    #[test]
    fn eq_parses() {
        let value = "=";
        let (output, result) = comparison_operator(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, CmpOp::Eq);
    }

    #[test]
    fn gte_parses() {
        let value = ">=";
        let (output, result) = comparison_operator(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, CmpOp::Gte);
    }

    #[test]
    fn lte_parses() {
        let value = "<=";
        let (output, result) = comparison_operator(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, CmpOp::Lte);
    }

    // operator tests
    // --------------
    #[test]
    fn and_parses() {
        let value = "and";
        let (output, result) = operator(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Op::And);
    }

    #[test]
    fn or_parses() {
        let value = "or";
        let (output, result) = operator(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Op::Or);
    }

    #[test]
    fn not_parses() {
        let value = "not";
        let (output, result) = operator(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Op::Not);
    }

    #[test]
    fn add_parses() {
        let value = "+";
        let (output, result) = operator(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Op::Add);
    }

    #[test]
    fn sub_parses() {
        let value = "-";
        let (output, result) = operator(value).unwrap();
        assert_eq!(output, "");
        assert_eq!(result, Op::Sub);
    }

    #[test]
    fn op_starts_with_but_invalid_fails() {
        let value = "note";
        let result = operator(value).unwrap_err();
        println!("Result: {}", result);
    }

    #[test]
    fn op_invalid_fails() {
        let value = "any";
        let result = operator(value).unwrap_err();
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
