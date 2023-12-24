use super::*;

pub fn parse_function_call_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut expr = None;
    let mut arguments = None;

    let (stream, _) = context("function call", apply((
        keep(&mut expr, parse_expression_top_precedence),
        keep(&mut arguments, opt(parenthesis(separated_list0(list_separator, parse_single_line_expression)))),
    )))(input)?;

    let (function, arguments) = (expr.unwrap(), arguments.unwrap());
    if arguments.is_some() {
        Ok((stream, Expression::FunCall { function: Box::new(function), arguments: arguments.unwrap() }))
    } else {
        Ok((stream, function))
    }
}

pub fn parse_effect_call_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut effect = None;
    let mut arguments = None;

    let (stream, _) = context("effect call", apply((
        skip(single_tag(Keyword::Perform)),
        cut(apply((
            keep(&mut effect, parse_effect_name),
            keep(&mut arguments, parenthesis(separated_list0(list_separator, parse_single_line_expression))),
        ))),
    )))(input)?;

    Ok((stream, Expression::EffCall { effect: effect.unwrap(), arguments: arguments.unwrap() }))
}

pub fn parse_handler_install_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut handler = None;
    let mut expr = None;

    let (stream, _) = context("handler install", apply((
        skip(single_tag(Keyword::With)),
        cut(apply((
            keep(&mut handler, identifier),
            skip(single_tag(Symbol::OpenParenthesis)),
            skip(single_tag(Symbol::CloseParentesis)),
            keep(&mut expr, parse_block_expression),
        ))),
    )))(input)?;

    Ok((stream, Expression::Handling { handler: handler.unwrap(), computation: Box::new(expr.unwrap()) }))
}