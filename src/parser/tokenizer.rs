use nom::multi::separated_list0;
use nom::{
    IResult,
    bytes::complete::*,
    branch::*,
    sequence::*,
    combinator::*,
};
use nom_locate::*;

type Span<'a> = LocatedSpan<&'a str>;

use super::tokens::*;
use super::utils::*;

pub fn tokenize(code: &str) -> Result<Vec<LocatedToken>, String> {
    let input = Span::from(code);
    let mut tokens = None;
    
    let result = apply((
        keep(&mut tokens, separated_list0(
            space_parser1,
            next_token
        )),
        skip(eof),
    ))(input);
        
    match result {
        Ok(_) => Ok(tokens.unwrap()),
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
        value(Token::UnitLiteral, tag("()")),
    ))(input)
}