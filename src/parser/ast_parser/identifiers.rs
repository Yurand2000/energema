use super::*;

pub fn identifier<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Identifier, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    map(tag(tokens![TokenType::Identifier]), |tokens: Stream<'a>| {
        let Token::Identifier(value) = tokens.iter_elements().next().unwrap().get_token().clone() else { panic!() };
        Identifier(value)
    })(input)
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

pub fn parse_effect_name<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Effect, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut typ = None;

    let (stream, _) = apply((
        keep(&mut typ, identifier),
    ))(input)?;

    Ok((stream, Effect{ eff_type: typ.unwrap(), name: None }))
}

pub fn parse_type<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Type, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    alt((
        value(Type::Void, single_tag(Symbol::Tilde)),
        value(Type::Unit, single_tag(TokenType::UnitLiteral)),
        value(Type::Bool, specific_identifier("bool")),
        value(Type::I32, specific_identifier("i32")),
        value(Type::Rune, specific_identifier("rune")),
        value(Type::String, specific_identifier("string")),
        map(parse_computation_type, |typ| Type::Computation(Box::new(typ))),
        parse_function_type,
        parse_handler_type
    ))(input)
}

pub fn parse_computation_type<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, ComputationType, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut typ = None;
    let mut effects = None;

    let (stream, _) = apply((
        keep(&mut typ, parse_type),
        keep(&mut effects, parse_computation_effects),
    ))(input)?;

    Ok((stream, ComputationType{ typ: typ.unwrap(), effects: effects.unwrap() }))
}

pub fn parse_computation_effects<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, ComputationEffects, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut inverted = None;
    let mut effects = None;

    let (stream, _) = apply((
        skip(single_tag(Symbol::Exclamation)),
        cut(braces(
            apply((
                keep(&mut inverted, opt(single_tag(Symbol::NegatedSet))),
                keep(&mut effects, separated_list0(list_separator, parse_effect_name))
            )),
        ))
    ))(input)?;

    let inverted = inverted.unwrap().is_some();
    let effects = effects.unwrap().into_iter().collect();

    if !inverted {
        Ok((stream, ComputationEffects::AtMost(effects)))
    } else {
        Ok((stream, ComputationEffects::AllBut(effects)))
    }
}

pub fn parse_function_type<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Type, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut in_types = None;
    let mut out_type = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::Fn)),
        cut(apply((
            keep(&mut in_types, parenthesis(separated_list0(list_separator, parse_type))),
            skip(single_tag(Symbol::Arrow)),
            keep(&mut out_type, parse_computation_type),
        ))),
    ))(input)?;

    Ok((stream, Type::Fun { in_types: in_types.unwrap(), out_type: Box::new(out_type.unwrap()) }))
}

pub fn parse_handler_type<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Type, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut in_type = None;
    let mut out_type = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::Handler)),
        cut(apply((
            keep(&mut in_type, parse_computation_type),
            skip(single_tag(Symbol::Arrow)),
            keep(&mut out_type, parse_computation_type),
        ))),
    ))(input)?;

    Ok((stream, Type::Handler { in_type: Box::new(in_type.unwrap()), out_type: Box::new(out_type.unwrap()) }))
}