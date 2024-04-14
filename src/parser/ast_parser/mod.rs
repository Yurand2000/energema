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
use crate::parser::utils::ParserOption::PNone;

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
    let mut declarations = PNone;

    let input = tokenize(code)?;
    let input = TokenStream::new(&input);
    let result = apply((
        keep(&mut declarations, many0(parse_declaration::<VerboseError<Stream>>)),
        skip(single_tag(TokenType::Eof)),
    ))(input);

    match result {
        Ok(_) => Ok(declarations.take()),
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
    let mut body = PNone;

    let (stream, _) = context("block", apply((
        keep(&mut body, braces(parse_sequencing_expression)),
    )))(input)?;

    Ok((stream, body.take()) )
}

pub fn parse_explicit_block_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut body = PNone;

    let (stream, _) = context("explicit block", apply((
        keep(&mut body, parse_block_expression),
    )))(input)?;

    Ok((stream, Expression::Block( Box::new(body.take()) )))
}

fn parse_sequencing_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut lexpr = PNone;
    let mut is_sequencing = PNone;
    let mut rexpr = PNone;

    let (stream, _) = context("sequencing", apply((
        keep(&mut lexpr, parse_let_expression),
        keep(&mut is_sequencing, opt(apply((
            single_tag(Symbol::Semicolon),
            keep(&mut rexpr, opt(parse_sequencing_expression)))))
        ),
    )))(input)?;

    if is_sequencing.take().is_some() {
        Ok((stream, Expression::Sequencing(
            Box::new(lexpr.take()),
            Box::new(rexpr.take().unwrap_or(Expression::Value(Box::new(Value::ULiteral))))
        )))
    } else {
        Ok((stream, lexpr.take()))
    }
}

fn parse_let_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut is_let = false;
    let mut id = PNone;
    let mut expr0 = PNone;
    let mut expr1 = PNone;

    let (stream, _) = context("let", alt((
        has_success(&mut is_let, apply((
            skip(single_tag(Keyword::Let)),
            cut(apply((
                keep(&mut id, identifier),
                skip(single_tag(Symbol::Equal)),
                keep(&mut expr0, parse_binary_op_expression),
            )))
        ))),
        keep(&mut expr1, parse_binary_op_expression),
    )))(input)?;

    if !is_let {
        Ok((stream, expr1.take()))
    } else {
        Ok((stream, Expression::Let { id: id.take(), expression: Box::new(expr0.take()) }))
    }
}

fn parse_expression_top_precedence<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    context("top precedence", alt((
        parenthesis(parse_single_line_expression),
        parse_closure_create,
        parse_explicit_block_expression,
        parse_if_expression,
        parse_while_expression,
        parse_effect_call_expression,
        parse_handler_install_expression,
        parse_variable,
        parse_value_expression,
    )))(input)
}

pub fn parse_closure_create<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut arguments = PNone;
    let mut closure = PNone;

    let (stream, _) = context("closure create", apply((
        alt((
            keep(&mut arguments, pipe_brackets(separated_list0(list_separator, identifier))),
            skip(single_tag(Symbol::LogicalOr)),
        )),
        cut(keep(&mut closure, parse_expression)),
    )))(input)?;

    Ok((stream, Expression::ClosureCreate { arguments: arguments.to_option().unwrap_or(Vec::new()), closure: Box::new(closure.take()) }))
}