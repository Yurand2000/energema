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
mod types;
mod if_expressions;
mod call_expressions;
mod operator_expressions;
mod value_parser;

use function_declaration::*;
use effect_declaration::*;
use handler_declaration::*;
use identifiers::*;
use types::*;
use if_expressions::*;
use call_expressions::*;
use operator_expressions::*;
use value_parser::*;

use super::tokenizer::tokenize;

type Stream<'a> = TokenStream<'a, LocatedToken>;
mod stream_display;

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
        Err(Err::Error(err)) => Err(format!("{}", err)),
        Err(Err::Failure(err)) => Err(format!("{}", err)),
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
    context("expression", alt((
        parse_block_expression,
        parse_single_line_expression,
    )))(input)
}

fn parse_single_line_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    context("single line", parse_binary_op_expression)(input)
}

pub fn parse_block_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut body = None;

    let (stream, _) = context("block", apply((
        keep(&mut body, braces(parse_sequencing_expression)),
    )))(input)?;

    Ok((stream, body.unwrap()) )
}

pub fn parse_explicit_block_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut body = None;

    let (stream, _) = context("explicit block", apply((
        keep(&mut body, parse_block_expression),
    )))(input)?;

    Ok((stream, Expression::Block( Box::new(body.unwrap()) )))
}

fn parse_sequencing_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut lexpr = None;
    let mut is_sequencing = None;
    let mut rexpr = None;

    let (stream, _) = context("sequencing", apply((
        keep(&mut lexpr, parse_let_expression),
        keep(&mut is_sequencing, opt(apply((
            single_tag(Symbol::Semicolon),
            cut(keep(&mut rexpr, parse_sequencing_expression)))))
        ),
    )))(input)?;

    if is_sequencing.unwrap().is_some() {
        Ok((stream, Expression::Sequencing(Box::new(lexpr.unwrap()), Box::new(rexpr.unwrap()))))
    } else {
        Ok((stream, lexpr.unwrap()))
    }
}

fn parse_let_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut id = None;
    let mut expr0 = None;
    let mut expr1 = None;

    let (stream, _) = context("let", alt((
        apply((
            skip(single_tag(Keyword::Let)),
            cut(apply((
                keep(&mut id, identifier),
                skip(single_tag(Symbol::Equal)),
                keep(&mut expr0, parse_binary_op_expression),
            )))
        )),
        keep(&mut expr1, parse_binary_op_expression),
    )))(input)?;

    if expr1.is_some() {
        Ok((stream, expr1.unwrap()))
    } else {
        Ok((stream, Expression::Let { id: id.unwrap(), expression: Box::new(expr0.unwrap()) }))
    }
}

fn parse_expression_top_precedence<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    context("top precedence", alt((
        parenthesis(parse_single_line_expression),
        parse_explicit_block_expression,
        parse_if_expression,
        parse_while_expression,
        parse_effect_call_expression,
        parse_handler_install_expression,
        parse_closure_create,
        parse_variable,
        parse_value_expression,
    )))(input)
}

pub fn parse_closure_create<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut arguments = None;
    let mut computation = None;

    let (stream, _) = context("closure create", apply((
        alt((
            keep(&mut arguments, pipe_brackets(separated_list0(list_separator, identifier))),
            skip(single_tag(Symbol::LogicalOr)),
        )),
        cut(keep(&mut computation, parse_expression)),
    )))(input)?;

    Ok((stream, Expression::Closure { arguments: arguments.unwrap_or_else(|| Vec::new()), computation: Box::new(computation.unwrap()) }))
}