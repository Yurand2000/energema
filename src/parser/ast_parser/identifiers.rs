use nom::error::Error;

use super::*;

pub fn identifier(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Identifier> {
    map(tag(tokens![TokenType::Identifier]), |tokens: TokenStream<LocatedToken>| {
        let Token::Identifier(value) = tokens.iter_elements().next().unwrap().get_token().clone() else { panic!() };
        Identifier(value)
    })(input)
}

pub fn specific_identifier<'a, 'b>(id: &'b str) -> impl FnMut(TokenStream<'a, LocatedToken>) -> IResult<TokenStream<'a, LocatedToken>, Identifier> + 'b {
    move |input: TokenStream<LocatedToken>| {
        let (stream, identifier) = identifier(input.clone())?;
        
        if identifier.0 == id {
            Ok((stream, identifier))
        } else {
            Err(nom::Err::Error(Error::new(input, nom::error::ErrorKind::Tag)))
        }
    }
}

pub fn parse_effect_name(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Effect> {
    let mut typ = None;

    let (stream, _) = apply((
        keep(&mut typ, identifier),
    ))(input)?;

    Ok((stream, Effect{ eff_type: typ.unwrap(), name: None }))
}

pub fn parse_type(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Type> {
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

pub fn parse_computation_type(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, ComputationType> {
    let mut typ = None;
    let mut effects = None;

    let (stream, _) = apply((
        keep(&mut typ, parse_type),
        keep(&mut effects, parse_computation_effects),
    ))(input)?;

    Ok((stream, ComputationType{ typ: typ.unwrap(), effects: effects.unwrap() }))
}

pub fn parse_computation_effects(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, ComputationEffects> {
    let mut inverted = None;
    let mut effects = None;

    let (stream, _) = apply((
        skip(single_tag(Symbol::Exclamation)),
        delimited(
            single_tag(Symbol::OpenBrace),
            apply((
                keep(&mut inverted, opt(single_tag(Symbol::NegatedSet))),
                keep(&mut effects, separated_list0(list_separator, parse_effect_name))
            )),
            single_tag(Symbol::CloseBrace)
        )
    ))(input)?;

    let inverted = inverted.unwrap().is_some();
    let effects = effects.unwrap().into_iter().collect();

    if !inverted {
        Ok((stream, ComputationEffects::AtMost(effects)))
    } else {
        Ok((stream, ComputationEffects::AllBut(effects)))
    }
}

pub fn parse_function_type(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Type> {
    let mut in_types = None;
    let mut out_type = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::Fn)),
        delimited(
            single_tag(Symbol::OpenParenthesis),
            keep(&mut in_types, separated_list0(list_separator, parse_type)),
            single_tag(Symbol::CloseParentesis)
        ),
        skip(single_tag(Symbol::Arrow)),
        keep(&mut out_type, parse_computation_type),
    ))(input)?;

    Ok((stream, Type::Fun { in_types: in_types.unwrap(), out_type: Box::new(out_type.unwrap()) }))
}

pub fn parse_handler_type(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Type> {
    let mut in_type = None;
    let mut out_type = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::Handler)),
        keep(&mut in_type, parse_computation_type),
        skip(single_tag(Symbol::Arrow)),
        keep(&mut out_type, parse_computation_type),
    ))(input)?;

    Ok((stream, Type::Handler { in_type: Box::new(in_type.unwrap()), out_type: Box::new(out_type.unwrap()) }))
}