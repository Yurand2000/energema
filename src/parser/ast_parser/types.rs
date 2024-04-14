use std::collections::HashSet;

use super::*;

pub fn parse_type<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Type, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut typ = PNone;
    let mut effects = PNone;

    let (stream, _) = context("type", apply((
        keep(&mut typ, parse_type_no_computation),
        keep(&mut effects, opt(parse_computation_effects)),
    )))(input)?;

    match effects.take() {
        Some(effects) => {
            let computation_type = ComputationType{ typ: typ.take(), effects };
            Ok((stream, Type::Computation(Box::new(computation_type))))
        },
        None => Ok((stream, typ.take())),
    }
}

pub fn parse_type_no_computation<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Type, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    context("type basic", alt((
        parenthesis(parse_type),
        value(Type::Void, single_tag(Symbol::Tilde)),
        value(Type::Unit, single_tag(TokenType::UnitLiteral)),
        value(Type::Bool, specific_identifier("bool")),
        value(Type::I32, specific_identifier("i32")),
        value(Type::Rune, specific_identifier("rune")),
        value(Type::String, specific_identifier("string")),
        parse_function_type,
        parse_handler_type
    )))(input)
}

pub fn parse_computation_type<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, ComputationType, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut typ = PNone;
    let mut effects = PNone;

    let (stream, _) = context("computation type", apply((
        keep(&mut typ, parse_type_no_computation),
        keep(&mut effects, parse_computation_effects),
    )))(input)?;

    Ok((stream, ComputationType{ typ: typ.take(), effects: effects.take() }))
}

pub fn parse_computation_effects<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, ComputationEffects, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut success = false;
    let mut inverted = PNone;
    let mut effects = PNone;

    let (stream, _) = opt_success(&mut success, opt(apply((
        skip(single_tag(Symbol::Exclamation)),
        cut(braces(
            apply((
                keep(&mut inverted, opt(single_tag(Symbol::NegatedSet))),
                keep(&mut effects, separated_list0(list_separator, parse_effect_name))
            )),
        ))
    ))))(input)?;

    if success {
        let inverted = inverted.take().is_some();
        let effects = effects.take().into_iter().collect();
    
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
    let mut arguments = PNone;
    let mut out_type = PNone;

    let (stream, _) = context("function type", apply((
        skip(single_tag(Keyword::Fn)),
        cut(apply((
            keep(&mut arguments, parenthesis(separated_list0(list_separator, parse_type))),
            skip(single_tag(Symbol::Arrow)),
            keep(&mut out_type, parse_computation_type),
        ))),
    )))(input)?;

    let typ = FunctionType{ arguments: arguments.take(), out_type: out_type.take() };
    Ok((stream, Type::Fun(Box::new(typ))))
}

pub fn parse_handler_type<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Type, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut arguments = PNone;
    let mut in_type = PNone;
    let mut out_type = PNone;
    let mut effects = PNone;

    let (stream, _) = context("handler type", apply((
        skip(single_tag(Keyword::Handler)),
        cut(apply((
            keep(&mut arguments, parenthesis(separated_list0(list_separator, parse_type))),
            keep(&mut in_type, parse_type),
            skip(single_tag(Symbol::DoubleArrow)),
            keep(&mut out_type, parse_type),
            skip(single_tag(Symbol::Exclamation)),
            keep(&mut effects, brackets(separated_list0(list_separator, parse_effect_name))),
        ))),
    )))(input)?;

    let typ = HandlerType {
        arguments: arguments.take(),
        in_type: in_type.take(),
        out_type: out_type.take(),
        managed_effects: effects.take()
    };

    Ok((stream, Type::Handler(Box::new(typ))))
}