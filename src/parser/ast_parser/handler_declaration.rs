use super::*;

pub fn parse_handler_declaration(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, HandlerDeclaration> {
    let mut name = None;
    let mut return_handler = None;
    let mut effect_handlers0 = None;
    let mut effect_handlers1 = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::Handler)),
        keep(&mut name, identifier),
        skip(single_tag(Symbol::OpenParenthesis)),
        skip(single_tag(Symbol::CloseParentesis)),
        delimited(
            single_tag(Symbol::OpenBrace),
            alt((
                apply((
                    keep(&mut return_handler, parse_return_handler),
                    alt((
                        apply((
                            skip(list_separator),
                            keep(&mut effect_handlers0, separated_list1(list_separator, parse_effect_handler)),
                            skip(opt(list_separator)),
                        )),
                        skip(opt(list_separator)),
                    ))
                )),
                apply((
                    keep(&mut effect_handlers1, separated_list0(list_separator, parse_effect_handler)),
                    skip(opt(list_separator)),
                )),
            )),
            single_tag(Symbol::CloseBrace),
        ),
    ))(input)?;

    let effect_handlers = effect_handlers0.unwrap_or( effect_handlers1.unwrap_or(Vec::new()) );
    Ok((stream, HandlerDeclaration{ name: name.unwrap(), return_handler, effect_handlers }))
}

pub fn parse_return_handler(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, (Type, Type, Identifier, Box<Expression>)> {
    let mut in_type = None;
    let mut out_type = None;
    let mut name = None;
    let mut body = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::Return)),
        keep(&mut name, identifier),
        skip(single_tag(Symbol::Colon)),
        keep(&mut in_type, parse_type),
        skip(single_tag(Symbol::Arrow)),
        keep(&mut out_type, parse_type),
        keep(&mut body, parse_function_body),
    ))(input)?;

    Ok( (stream, (in_type.unwrap(), out_type.unwrap(), name.unwrap(), Box::new(body.unwrap()))) )
}

pub fn parse_effect_handler(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, (Effect, Vec<Identifier>, Box<Expression>)> {
    let mut effect = None;
    let mut arguments = None;
    let mut body = None;

    let (stream, _) = apply((
        keep(&mut effect, parse_effect_name),
        delimited(
            single_tag(Symbol::OpenParenthesis),
            keep(&mut arguments, separated_list0(list_separator, identifier)),
            single_tag(Symbol::CloseParentesis)
        ),
        keep(&mut body, parse_function_body),
    ))(input)?;

    Ok( (stream, (effect.unwrap(), arguments.unwrap(), Box::new(body.unwrap()))) )
}