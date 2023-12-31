use super::*;

pub fn identifier<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Identifier, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    context("identifier", map(tag(tokens![TokenType::Identifier]), |tokens: Stream<'a>| {
        let Token::Identifier(value) = tokens.iter_elements().next().unwrap().get_token().clone() else { panic!() };
        Identifier(value)
    }))(input)
}

pub fn parse_effect_name<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Effect, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut typ = PNone;

    let (stream, _) = context("effect", apply((
        keep(&mut typ, identifier),
    )))(input)?;

    Ok((stream, Effect{ eff_type: typ.take() }))
}

pub fn parse_typed_identifier<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, TypedIdentifier, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut id = PNone;
    let mut typ = PNone;

    let (stream, _) = context("typed identifier", apply((
        keep(&mut id, identifier),
        cut(apply((
            skip(single_tag(Symbol::Colon)),
            keep(&mut typ, parse_type),
        ))),
    )))(input)?;

    Ok((stream, TypedIdentifier{ id: id.take(), typ: typ.take()  }))
}

pub fn specific_identifier<'a, 'b, E>(id: &'b str) -> impl FnMut(Stream<'a>) -> IResult<Stream<'a>, Identifier, E> + 'b
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    move |input: Stream<'a>| {
        let (stream, identifier) = identifier(input.clone())?;
        
        if identifier.0 == id {
            Ok((stream, identifier))
        } else {
            Err(nom::Err::Error(E::from_error_kind(input, nom::error::ErrorKind::Tag)))
        }
    }
}