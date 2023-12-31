use nom::{
    IResult,
    bytes::complete::*,
    character::complete::*,
    branch::*,
    sequence::*,
    combinator::*,
    multi::*,
};
use nom_locate::*;

type Span<'a> = LocatedSpan<&'a str>;

use super::tokens::*;
use super::utils::*;
use super::utils::ParserOption::PNone;

#[cfg(test)]
mod tests;

pub fn tokenize(code: &str) -> Result<Vec<LocatedToken>, String> {
    let input = Span::from(code);
    let mut tokens = PNone;
    let mut eof_position = PNone;

    let result = apply((
        skip(space_parser0),
        keep(&mut tokens, many0(
            first(
                next_token,
                space_parser0
            )
        )),
        keep(&mut eof_position, position),
        skip(eof),
    ))(input);

    match result {
        Ok(_) => {
            let mut tokens = tokens.take();
            tokens.push(LocatedToken::from_span(Token::Eof, eof_position.take()));
            Ok(tokens)
        },
        Err(err) => Err(err.to_string()),
    }
}

fn next_token(input: Span) -> IResult<Span, LocatedToken> {
    let mut pos = PNone;
    let mut token = PNone;

    let (out, _) = apply((
        keep(&mut pos, position),
        keep(&mut token, extract_token)
    ))(input)?;

    let token = LocatedToken::from_span(token.take(), pos.take());

    Ok((out, token))
}

fn extract_token(input: Span) -> IResult<Span, Token> {
    alt((
        //unit parser
        value(Token::UnitLiteral, tag("unit")),

        //bool parser
        value(Token::BoolLiteral(true), tag("true")),
        value(Token::BoolLiteral(false), tag("false")),

        //i32 parser
        map(i32, |value| Token::I32Literal(value)),

        //rune parser
        map(parse_rune, |value| Token::RuneLiteral(value)),

        //string parser
        map(parse_string, |value| Token::StringLiteral(value)),

        //symbol parser
        map(parse_symbol, |value| Token::Symbol(value)),

        //keywords
        map(parse_keyword, |value| Token::Keyword(value)),

        //identifiers
        map(parse_identifier, |value| Token::Identifier(value.into_fragment().to_owned())),
    ))(input)
}

fn parse_keyword(input: Span) -> IResult<Span, Keyword> {
    alt((
        value(Keyword::If, tag("if")),
        value(Keyword::Else, tag("else")),
        value(Keyword::While, tag("while")),
        value(Keyword::Fn, tag("fn")),
        value(Keyword::Let, tag("let")),
        value(Keyword::Perform, tag("perform")),
        value(Keyword::With, tag("with")),
        value(Keyword::Handler, tag("handler")),
        value(Keyword::Effect, tag("effect")),
        value(Keyword::Return, tag("return")),
    ))(input)
}

fn parse_symbol(input: Span) -> IResult<Span, Symbol> {
    alt((
        alt((
            value(Symbol::Arrow, tag("->")),
            value(Symbol::DoubleArrow, tag("=>")),
            value(Symbol::LogicalAnd, tag("&&")),
            value(Symbol::LogicalOr, tag("||")),
            value(Symbol::LogicalXor, tag("^^")),
            value(Symbol::DoubleEqual, tag("==")),
            value(Symbol::NotEqual, tag("!=")),
            value(Symbol::GreaterOrEqual, tag(">=")),
            value(Symbol::SmallerOrEqual, tag("<=")),
        )),
        alt((
            value(Symbol::OpenParenthesis, char('(')),
            value(Symbol::CloseParentesis, char(')')),
            value(Symbol::OpenBracket, char('[')),
            value(Symbol::CloseBracket, char(']')),
            value(Symbol::OpenBrace, char('{')),
            value(Symbol::CloseBrace, char('}')),
            value(Symbol::Comma, char(',')),
            value(Symbol::Semicolon, char(';')),
            value(Symbol::Colon, char(':')),
            value(Symbol::Tilde, char('~')),
        )),
        alt((
            value(Symbol::NegatedSet, char('^')),
            value(Symbol::Exclamation, char('!')),
            value(Symbol::Plus, char('+')),
            value(Symbol::Minus, char('-')),
            value(Symbol::Times, char('*')),
            value(Symbol::ForwardSlash, char('/')),
            value(Symbol::Modulo, char('%')),
            value(Symbol::Pipe, char('|')),
            value(Symbol::Equal, char('=')),
            value(Symbol::GreaterThan, char('>')),
            value(Symbol::SmallerThan, char('<')),
        )),
    ))(input)
}

fn parse_identifier(input: Span) -> IResult<Span, Span> {
    recognize(
        pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_"))))
        )
    )(input)
}

fn parse_rune(input: Span) -> IResult<Span, char> {
    delimited(char('\''), anychar, char('\''))(input)
}

fn parse_string(input: Span) -> IResult<Span, String> {
    map(
        delimited(char('"'), many1(none_of("\"")), char('"')),
        |vect| vect.into_iter().collect()
    )(input)
}

fn space_parser0(input: Span) -> IResult<Span, Span> {
    recognize(
        tuple((
            multispace0,
            opt(alt((
                single_line_comment,
                multi_line_comment,
            ))),
            multispace0,
        ))
    )(input)
}

fn space_parser1(input: Span) -> IResult<Span, Span> {
    recognize(
        tuple((
            multispace1,
            opt(alt((
                single_line_comment,
                multi_line_comment,
            ))),
            multispace0,
        ))
    )(input)
}

fn single_line_comment(input: Span) -> IResult<Span, Span> {
    recognize(
        tuple((
            tag("//"),
            is_not("\n\r"),
            line_ending
        ))
    )(input)
}

fn multi_line_comment(input: Span) -> IResult<Span, Span> {
    recognize(
        tuple((
            tag("/*"),
            take_until("*/"),
            tag("*/"),
        ))
    )(input)
}