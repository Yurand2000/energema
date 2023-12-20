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

use crate::ast::BinaryOp;
use crate::ast::UnaryOp;

use super::tokens::*;
use super::utils::*;

pub fn tokenize(code: &str) -> Result<Vec<LocatedToken>, String> {
    let input = Span::from(code);
    
    let result = separated_list0(
        space_parser1,
        next_token
    )(input);
        
    match result {
        Ok((_, tokens)) => Ok(tokens),
        Err(err) => Err(err.to_string()),
    }
}

fn space_parser1(input: Span) -> IResult<Span, ()> {
    todo!()
}

fn next_token(input: Span) -> IResult<Span, LocatedToken> {
    let mut pos = None;
    let mut token = None;

    let (out, _) = apply((
        keep(&mut pos, position),
        keep(&mut token, extract_token)
    ))(input)?;

    let token = LocatedToken::new(token.unwrap(), pos.unwrap());
    
    Ok((out, token))
}

fn extract_token(input: Span) -> IResult<Span, Token> {
    alt((
        //unit parser
        value(Token::UnitLiteral, tag("()")),

        //bool parser
        value(Token::BoolLiteral(true), tag("true")),
        value(Token::BoolLiteral(false), tag("false")),

        //i32 parser
        map(i32, |value| Token::I32Literal(value)),

        //rune parser
        map(parse_rune, |value| Token::RuneLiteral(value)),

        //string parser
        map(parse_string, |value| Token::StringLiteral(value)),

        //keywords
        map(parse_keyword, |value| Token::Keyword(value)),

        //identifiers
        map(parse_identifier, |value| Token::Identifier(value.into_fragment().to_owned())),

        //eof
        value(Token::Eof, eof)
    ))(input)
}

fn parse_keyword(input: Span) -> IResult<Span, Keyword> {
    alt((
        value(Keyword::If, tag("if")),
        value(Keyword::Else, tag("else")),
        value(Keyword::While, tag("while")),
        map(parse_symbol, |sym| Keyword::Symbol(sym)),
    ))(input)
}

fn parse_symbol(input: Span) -> IResult<Span, Symbol> {
    alt((
        value(Symbol::OpenParenthesis, char('(')),
        value(Symbol::CloseParentesis, char('(')),
        value(Symbol::OpenBracket, char('[')),
        value(Symbol::CloseBracket, char(']')),
        value(Symbol::OpenBrace, char('{')),
        value(Symbol::CloseBrace, char('}')),
    ))(input)
}

fn parse_unary_operator(input: Span) -> IResult<Span, UnaryOp> {
    value(UnaryOp::LNot, tag("!"))(input)
}

fn parse_binary_operator(input: Span) -> IResult<Span, BinaryOp> {
    alt((
        value(BinaryOp::Add, tag("+")),
        value(BinaryOp::Sub, tag("-")),
        value(BinaryOp::Mul, tag("*")),
        value(BinaryOp::Div, tag("/")),
        value(BinaryOp::Mod, tag("%")),

        value(BinaryOp::LAnd, tag("&&")),
        value(BinaryOp::LOr, tag("||")),
        value(BinaryOp::LXor, tag("^^")),

        value(BinaryOp::Eq, tag("==")),
        value(BinaryOp::Ne, tag("!=")),
        value(BinaryOp::Le, tag(">=")),
        value(BinaryOp::Lt, tag("<")),
        value(BinaryOp::Gt, tag(">")),
        value(BinaryOp::Ge, tag(">=")),
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