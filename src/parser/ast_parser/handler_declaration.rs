use super::*;

pub fn parse_handler_declaration<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, HandlerDeclaration, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut has_return_handler = false;
    let mut name = PNone;
    let mut arguments = PNone;
    let mut out_type = PNone;
    let mut return_handler = PNone;
    let mut effect_handlers0 = PNone;
    let mut effect_handlers1 = PNone;

    let (stream, _) = context("handler declaration", apply((
        skip(single_tag(Keyword::Handler)),
        cut(apply((
            keep(&mut name, identifier),
            keep(&mut arguments, parse_arguments),
            keep(&mut out_type, opt(parse_out_type)),
            braces(
                alt((
                    has_success(&mut has_return_handler, context("with return handler", apply((
                        keep(&mut return_handler, parse_return_handler),
                        alt((
                            apply((
                                skip(list_separator),
                                keep(&mut effect_handlers0, separated_list1(list_separator, parse_effect_handler)),
                                skip(opt(list_separator)),
                            )),
                            skip(opt(list_separator)),
                        ))
                    )))),
                    context("without return handler", apply((
                        keep(&mut effect_handlers1, separated_list0(list_separator, parse_effect_handler)),
                        skip(opt(list_separator)),
                    ))),
                )),
            ),
        ))),
    )))(input)?;

    if has_return_handler {
        Ok((stream, HandlerDeclaration{
            name: name.take(),
            return_handler: Some(return_handler.take()),
            arguments: arguments.take(),
            out_type: out_type.take(),
            effect_handlers: effect_handlers0.take(),
        }))
    } else {
        Ok((stream, HandlerDeclaration{
            name: name.take(),
            return_handler: None,
            arguments: arguments.take(),
            out_type: out_type.take(),
            effect_handlers: effect_handlers1.take(),
        }))
    }
}

pub fn parse_return_handler<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, ReturnHandlerDeclaration, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut argument = PNone;
    let mut body = PNone;

    let (stream, _) = context("return handler", apply((
        skip(single_tag(Keyword::Return)),
        cut(apply((
            keep(&mut argument, parenthesis(parse_typed_identifier)),
            keep(&mut body, parse_block_expression),
        ))),
    )))(input)?;

    Ok((stream, ReturnHandlerDeclaration {
        ret_arg: argument.take(),
        expression: Box::new(body.take()),
    }))
}

pub fn parse_effect_handler<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, EffectHandlerDeclaration, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut effect = PNone;
    let mut arguments = PNone;
    let mut body = PNone;

    let (stream, _) = context("effect handler", apply((
        keep(&mut effect, parse_effect_name),
        keep(&mut arguments, parse_arguments),
        keep(&mut body, parse_block_expression),
    )))(input)?;

    Ok(( stream, EffectHandlerDeclaration {
        effect: effect.take(),
        arguments: arguments.take(),
        expression: Box::new(body.take()),
    }))
}

#[test]
fn test_effect_handler() {
    let data = tokenize("my_effect(value0: i32) { test_fn(unit); continuation(value0) } ").unwrap();
    let stream = TokenStream::new(&data);

    let result = parse_effect_handler::<TestError>(stream);
    assert!(result.is_ok(), "{:#?}", result.unwrap_err());
}