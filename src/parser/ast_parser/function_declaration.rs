use super::*;

pub fn parse_function_declaration<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, FunDeclaration, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut name = None;
    let mut arguments = None;
    let mut out_type = None;
    let mut body = None;

    let (stream, _) = context("function declaration", apply((
        skip(single_tag(Keyword::Fn)),
        keep(&mut name, identifier),
        keep(&mut arguments, parse_arguments),
        keep(&mut out_type, opt(parse_out_type)),
        keep(&mut body, parse_block_expression),
    )))(input)?;

    Ok((stream, FunDeclaration{
        name: name.unwrap(),
        arguments: arguments.unwrap(),
        out_type: out_type.unwrap(),
        expression: Box::new(body.unwrap()),
    }))
}

pub fn parse_arguments<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Vec<TypedIdentifier>, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    parenthesis(separated_list0(list_separator, parse_typed_identifier))(input)
}

pub fn parse_out_type<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Type, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut typ = None;

    let (stream, _) = context("out type", apply((
        skip(single_tag(Symbol::Arrow)),
        cut(keep(&mut typ, parse_type))
    )))(input)?;

    Ok((stream, typ.unwrap()))
}