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
use crate::parser::tokens::{TokenStream, LocatedToken, Token, TokenType, Keyword, Symbol};
use crate::parser::utils::*;

mod utils;
use utils::*;

mod function_declaration;
mod effect_declaration;
mod handler_declaration;
mod value_parser;

use function_declaration::*;
use effect_declaration::*;
use handler_declaration::*;
use value_parser::*;

use super::tokenizer::tokenize;

pub fn parse_code(code: &str) -> Result<Vec<Declaration>, String> {
    let mut declarations = None;

    let input = tokenize(code)?;
    let result = apply((
        keep(&mut declarations, many0(parse_declaration)),
        skip(single_tag(TokenType::Eof)),
    ))(TokenStream::new(&input));

    match result {
        Ok(_) => Ok(declarations.unwrap()),
        Err(err) => Err(err.to_string()),
    }
}

fn parse_declaration(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Declaration> {
    alt((
        map(parse_function_declaration, |decl| Declaration::Function(decl)),
        map(parse_handler_declaration, |decl| Declaration::Handler(decl)),
        map(parse_effect_declaration, |decl| Declaration::Effect(decl)),
    ))(input)
}

fn parse_function_body(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    todo!()
}

fn parse_effect(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Effect> {
    todo!()
}

fn type_string(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Type> {
    todo!()
}