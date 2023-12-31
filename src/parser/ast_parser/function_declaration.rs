use super::*;

pub fn parse_function_declaration<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, FunDeclaration, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut name = PNone;
    let mut arguments = PNone;
    let mut out_type = PNone;
    let mut body = PNone;

    let (stream, _) = context("function declaration", apply((
        skip(single_tag(Keyword::Fn)),
        keep(&mut name, identifier),
        keep(&mut arguments, parse_arguments),
        keep(&mut out_type, opt(parse_out_type)),
        keep(&mut body, parse_block_expression),
    )))(input)?;

    Ok((stream, FunDeclaration{
        name: name.take(),
        arguments: arguments.take(),
        out_type: out_type.take(),
        expression: Box::new(body.take()),
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
    let mut typ = PNone;

    let (stream, _) = context("out type", apply((
        skip(single_tag(Symbol::Arrow)),
        cut(keep(&mut typ, parse_type))
    )))(input)?;

    Ok((stream, typ.take()))
}