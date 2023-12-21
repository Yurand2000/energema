use nom::{
    IResult,
    bytes::complete::*,
    branch::*,
    sequence::*,
    combinator::*,
    multi::*,
    InputIter,
};
use crate::ast::*;
use super::tokens::{TokenStream, LocatedToken, Token, TokenType};

fn value_parser(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Value> {
    alt((
        value(Value::ULiteral, tag(tokens![TokenType::UnitLiteral])),
        map(tag(tokens![TokenType::BoolLiteral]), |tokens: TokenStream<LocatedToken>| {
            let &Token::BoolLiteral(value) = tokens.iter_elements().next().unwrap().get_token() else { panic!() };
            Value::BLiteral(value)
        }),
        map(tag(tokens![TokenType::I32Literal]), |tokens: TokenStream<LocatedToken>| {
            let &Token::I32Literal(value) = tokens.iter_elements().next().unwrap().get_token() else { panic!() };
            Value::I32Literal(value)
        }),
        map(tag(tokens![TokenType::RuneLiteral]), |tokens: TokenStream<LocatedToken>| {
            let &Token::RuneLiteral(value) = tokens.iter_elements().next().unwrap().get_token() else { panic!() };
            Value::RuneLiteral(value)
        }),
        map(tag(tokens![TokenType::StringLiteral]), |tokens: TokenStream<LocatedToken>| {
            let Token::StringLiteral(value) = tokens.iter_elements().next().unwrap().get_token().clone() else { panic!() };
            Value::StringLiteral(value)
        }),
        map(tag(tokens![TokenType::Identifier]), |tokens: TokenStream<LocatedToken>| {
            let Token::Identifier(value) = tokens.iter_elements().next().unwrap().get_token().clone() else { panic!() };
            Value::Var(Identifier(value))
        }),
    ))(input)
}