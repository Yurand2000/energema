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
mod identifiers;
mod if_expressions;
mod call_expressions;
mod operator_expressions;
mod value_parser;

use function_declaration::*;
use effect_declaration::*;
use handler_declaration::*;
use identifiers::*;
use if_expressions::*;
use call_expressions::*;
use operator_expressions::*;
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
        Err(err) => Err(format!("{:#?}", err)),
    }
}

fn parse_declaration(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Declaration> {
    println!("{:#?}", input);

    alt((
        map(parse_function_declaration, |decl| Declaration::Function(decl)),
        map(parse_handler_declaration, |decl| Declaration::Handler(decl)),
        map(parse_effect_declaration, |decl| Declaration::Effect(decl)),
    ))(input)
}

fn parse_expression(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    parse_sequencing_expression(input)
}

fn parse_sequencing_expression(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    let mut lexpr = None;
    let mut rexpr = None;

    let (stream, _) = apply((
        keep(&mut lexpr, parse_function_call_expression),
        keep(&mut rexpr, opt(second(sequencing_separator, cut(parse_expression)))),
    ))(input)?;

    let (lexpr, rexpr) = (lexpr.unwrap(), rexpr.unwrap());
    if rexpr.is_some() {
        Ok((stream, Expression::Sequencing(Box::new(lexpr), Box::new(rexpr.unwrap()))))
    } else {
        Ok((stream, lexpr))
    }
}

fn parse_expression_no_sequencing(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    alt((
        parenthesis(parse_expression),
        parse_let_expression,
        parse_if_expression,
        parse_while_expression,
        parse_effect_call_expression,
        parse_handler_install_expression,
        parse_unary_op_expression,
        map(parse_value, |value| Expression::Value(Box::new(value))),
    ))(input)
}

fn sequencing_separator(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, TokenStream<LocatedToken>> {
    single_tag(Symbol::Semicolon)(input)
}

fn parse_let_expression(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    let mut id = None;
    let mut expr = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::Let)),
        keep(&mut id, identifier),
        skip(single_tag(Symbol::Equal)),
        keep(&mut expr, parse_expression_no_sequencing),
    ))(input)?;

    Ok((stream, Expression::Let { id: id.unwrap(), expression: Box::new(expr.unwrap()) }))
}