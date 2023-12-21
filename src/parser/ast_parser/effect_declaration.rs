use super::*;

pub fn parse_effect_declaration(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, EffectDeclaration> {
    let mut name = None;
    let mut in_types = None;
    let mut out_type = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::Effect)),
        keep(&mut name, identifier),
        skip(single_tag(Symbol::Colon)),
        keep(&mut in_types, separated_list1(list_separator, type_string)),
        skip(single_tag(Symbol::Arrow)),
        keep(&mut out_type, type_string),
        skip(single_tag(Symbol::Semicolon)),
    ))(input)?;

    Ok((stream, EffectDeclaration{ name: name.unwrap(), in_types: in_types.unwrap(), out_type: out_type.unwrap() }))
}