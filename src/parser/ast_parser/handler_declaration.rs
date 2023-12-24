use super::*;

pub fn parse_handler_declaration<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, HandlerDeclaration, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut name = None;
    let mut return_handler = None;
    let mut effect_handlers0 = None;
    let mut effect_handlers1 = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::Handler)),
        cut(apply((
            keep(&mut name, identifier),
            skip(single_tag(Symbol::OpenParenthesis)),
            skip(single_tag(Symbol::CloseParentesis)),
            braces(
                alt((
                    apply((
                        keep(&mut return_handler, context("return_handler", parse_return_handler)),
                        alt((
                            apply((
                                skip(list_separator),
                                keep(&mut effect_handlers0, separated_list1(list_separator, context("effect_handler", parse_effect_handler))),
                                skip(opt(list_separator)),
                            )),
                            skip(opt(list_separator)),
                        ))
                    )),
                    apply((
                        keep(&mut effect_handlers1, separated_list0(list_separator, context("effect_handler", parse_effect_handler))),
                        skip(opt(list_separator)),
                    )),
                )),
            ),
        ))),
    ))(input)?;

    let effect_handlers = effect_handlers0.unwrap_or( effect_handlers1.unwrap_or(Vec::new()) );
    Ok((stream, HandlerDeclaration{ name: name.unwrap(), return_handler, effect_handlers }))
}

pub fn parse_return_handler<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, (Type, Type, Identifier, Box<Expression>), E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut in_type = None;
    let mut out_type = None;
    let mut name = None;
    let mut body = None;

    let (stream, _) = context("return handler", apply((
        skip(single_tag(Keyword::Return)),
        cut(apply((
            keep(&mut name, identifier),
            skip(single_tag(Symbol::Colon)),
            keep(&mut in_type, parse_type),
            skip(single_tag(Symbol::Arrow)),
            keep(&mut out_type, parse_type),
            keep(&mut body, parse_block_expression),
        ))),
    )))(input)?;

    Ok( (stream, (in_type.unwrap(), out_type.unwrap(), name.unwrap(), Box::new(body.unwrap()))) )
}

pub fn parse_effect_handler<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, (Effect, Vec<Identifier>, Box<Expression>), E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut effect = None;
    let mut arguments = None;
    let mut body = None;

    let (stream, _) = context("effect handler", apply((
        keep(&mut effect, parse_effect_name),
        keep(&mut arguments, parenthesis(separated_list0(list_separator, identifier))),
        keep(&mut body, parse_block_expression),
    )))(input)?;

    Ok( (stream, (effect.unwrap(), arguments.unwrap(), Box::new(body.unwrap()))) )
}

#[test]
fn test_effect_handler() {
    let data = tokenize("my_effect(value0, continuation) { test_fn(unit); continuation(value0) } ").unwrap();
    let stream = TokenStream::new(&data);

    let result = parse_effect_handler::<TestError>(stream);
    assert!(result.is_ok(), "{:#?}", result.unwrap_err());
}