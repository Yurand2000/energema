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

mod effect_declaration;
mod handler_declaration;
mod value_parser;

use effect_declaration::*;
use handler_declaration::*;
use value_parser::*;

fn parse_function_body(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    todo!()
}

fn parse_effect(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Effect> {
    todo!()
}

fn type_string(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Type> {
    todo!()
}