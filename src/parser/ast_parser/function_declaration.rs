use super::*;

pub fn parse_function_declaration<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, FunDeclaration, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut name = None;
    let mut arguments = None;
    let mut body = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::Fn)),
        keep(&mut name, identifier),
        keep(&mut arguments, parenthesis(separated_list0(list_separator, identifier))),
        keep(&mut body, parse_block),
    ))(input)?;

    Ok((stream, FunDeclaration{ name: name.unwrap(), arguments: arguments.unwrap(), expression: Box::new(body.unwrap()) }))
}

pub fn parse_block<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut body = None;

    let (stream, _) = apply((
        keep(&mut body, braces(parse_expression)),
    ))(input)?;

    Ok((stream, body.unwrap()))
}