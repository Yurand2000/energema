use nom::Err;
use nom::error::{ParseError, VerboseError, context, ContextError};
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

type Stream<'a> = TokenStream<'a, LocatedToken>;

#[cfg(test)]
type TestError<'a> = nom::error::Error<Stream<'a>>;

pub fn parse_code(code: &str) -> Result<Vec<Declaration>, String> {
    let mut declarations = None;

    let input = tokenize(code)?;
    let input = TokenStream::new(&input);
    let result = apply((
        keep(&mut declarations, many0(parse_declaration::<VerboseError<Stream>>)),
        skip(single_tag(TokenType::Eof)),
    ))(input);

    match result {
        Ok(_) => Ok(declarations.unwrap()),
        Err(Err::Error(err)) => Err(format!("{:#?}", err)),
        Err(Err::Failure(err)) => Err(format!("{:#?}", err)),
        Err(Err::Incomplete(_)) => todo!(),
    }
}

fn parse_declaration<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Declaration, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    alt((
        map(parse_function_declaration, |decl| Declaration::Function(decl)),
        map(parse_handler_declaration, |decl| Declaration::Handler(decl)),
        map(parse_effect_declaration, |decl| Declaration::Effect(decl)),
    ))(input)
}

fn parse_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    context("sequencing", parse_sequencing_expression)(input)
}

fn parse_sequencing_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut lexpr = None;
    let mut rexpr = None;

    let (stream, _) = apply((
        keep(&mut lexpr, context("let", parse_let_expression)),
        keep(&mut rexpr, opt(second(sequencing_separator, cut(parse_expression)))),
    ))(input)?;

    let (lexpr, rexpr) = (lexpr.unwrap(), rexpr.unwrap());
    if rexpr.is_some() {
        Ok((stream, Expression::Sequencing(Box::new(lexpr), Box::new(rexpr.unwrap()))))
    } else {
        Ok((stream, lexpr))
    }
}

fn parse_expression_no_sequencing<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    alt((
        parenthesis(parse_expression),
        parse_if_expression,
        parse_while_expression,
        parse_effect_call_expression,
        parse_handler_install_expression,
        parse_unary_op_expression,
        parse_variable,
        parse_value_expression,
    ))(input)
}

fn sequencing_separator<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Stream<'a>, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    single_tag(Symbol::Semicolon)(input)
}

fn parse_let_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut id = None;
    let mut expr0 = None;
    let mut expr1 = None;

    let (stream, _) = alt((
        apply((
            skip(single_tag(Keyword::Let)),
            cut(apply((
                keep(&mut id, identifier),
                skip(single_tag(Symbol::Equal)),
                keep(&mut expr0, context("fn_call", parse_function_call_expression)),
            )))
        )),
        keep(&mut expr1, context("fn_call", parse_function_call_expression)),
    ))(input)?;

    if expr1.is_some() {
        Ok((stream, expr1.unwrap()))
    } else {
        Ok((stream, Expression::Let { id: id.unwrap(), expression: Box::new(expr0.unwrap()) }))
    }
}