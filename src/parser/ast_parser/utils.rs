use nom::{
    IResult,
    bytes::complete::*,
    branch::*,
    sequence::*,
    combinator::*,
    multi::*,
    InputIter,
};
use crate::ast::Identifier;
use crate::parser::tokens::{TokenStream, LocatedToken, Token, TokenType, Symbol};

pub fn single_tag<T: Into<TokenType> + Clone>(token: T) -> impl FnMut(TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, TokenStream<LocatedToken>> {
    move |input: TokenStream<LocatedToken>| {
        tag(tokens!(token.clone()))(input)
    }
}

pub fn identifier(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Identifier> {
    map(tag(tokens![TokenType::Identifier]), |tokens: TokenStream<LocatedToken>| {
        let Token::Identifier(value) = tokens.iter_elements().next().unwrap().get_token().clone() else { panic!() };
        Identifier(value)
    })(input)
}

pub fn list_separator(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, TokenStream<LocatedToken>> {
    tag(tokens!(Symbol::Comma))(input)
}