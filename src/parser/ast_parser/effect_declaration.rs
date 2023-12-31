use super::*;

pub fn parse_effect_declaration<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, EffectDeclaration, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut name = PNone;
    let mut in_types = PNone;
    let mut out_type = PNone;

    let (stream, _) = context("effect declaration", apply((
        skip(single_tag(Keyword::Effect)),
        cut(apply((
            keep(&mut name, parse_effect_name),
            skip(single_tag(Symbol::Colon)),
            keep(&mut in_types, separated_list1(list_separator, parse_type)),
            keep(&mut out_type, opt(second(
                single_tag(Symbol::Arrow),
                cut(parse_type),
            ))),
            skip(single_tag(Symbol::Semicolon)),
        ))),
    )))(input)?;

    Ok((stream, EffectDeclaration{ name: name.take(), in_types: in_types.take(), out_type: out_type.take() }))
}