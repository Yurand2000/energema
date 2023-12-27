use std::collections::HashSet;

use super::*;

pub fn parse_type<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Type, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut typ = None;
    let mut effects = None;

    let (stream, _) = context("type", apply((
        keep(&mut typ, parse_type_no_computation),
        keep(&mut effects, opt(parse_computation_effects)),
    )))(input)?;

    match effects.unwrap() {
        Some(effects) => {
            let computation_type = ComputationType{ typ: typ.unwrap(), effects };
            Ok((stream, Type::Computation(Box::new(computation_type))))
        },
        None => Ok((stream, typ.unwrap())),
    }
}

pub fn parse_type_no_computation<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Type, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    alt((
        parenthesis(parse_type),
        value(Type::Void, single_tag(Symbol::Tilde)),
        value(Type::Unit, single_tag(TokenType::UnitLiteral)),
        value(Type::Bool, specific_identifier("bool")),
        value(Type::I32, specific_identifier("i32")),
        value(Type::Rune, specific_identifier("rune")),
        value(Type::String, specific_identifier("string")),
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
        keep(&mut typ, parse_type_no_computation),
        keep(&mut effects, parse_computation_effects),
    ))(input)?;

    Ok((stream, ComputationType{ typ: typ.unwrap(), effects: effects.unwrap() }))
}

pub fn parse_computation_effects<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, ComputationEffects, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut inverted = None;
    let mut effects = None;

    let (stream, parsed) = opt(apply((
        skip(single_tag(Symbol::Exclamation)),
        cut(braces(
            apply((
                keep(&mut inverted, opt(single_tag(Symbol::NegatedSet))),
                keep(&mut effects, separated_list0(list_separator, parse_effect_name))
            )),
        ))
    )))(input)?;

    if parsed.is_some() {
        let inverted = inverted.unwrap().is_some();
        let effects = effects.unwrap().into_iter().collect();
    
        if !inverted {
            Ok((stream, ComputationEffects::AtMost(effects)))
        } else {
            Ok((stream, ComputationEffects::AllBut(effects)))
        }
    } else {
        Ok((stream, ComputationEffects::AllBut(HashSet::new())))
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
    let mut effects = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::Handler)),
        cut(apply((
            keep(&mut in_type, parse_type),
            skip(single_tag(Symbol::DoubleArrow)),
            keep(&mut out_type, parse_type),
            skip(single_tag(Symbol::Exclamation)),
            brackets(
                keep(&mut effects, separated_list0(list_separator, parse_effect_name))
            ),
        ))),
    ))(input)?;

    let in_type = ComputationType { typ: in_type.unwrap(), effects: ComputationEffects::AllBut(HashSet::new()) };
    let out_type = ComputationType { typ: out_type.unwrap(), effects: ComputationEffects::AllBut(effects.unwrap().into_iter().collect()) };

    Ok((stream, Type::Handler {
        in_type: Box::new(in_type),
        out_type: Box::new(out_type)
    }))
}