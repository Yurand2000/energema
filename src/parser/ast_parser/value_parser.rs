use super::*;

pub fn parse_value<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Value, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    alt((
        value(Value::ULiteral, tag(tokens![TokenType::UnitLiteral])),
        map(tag(tokens![TokenType::BoolLiteral]), |tokens: Stream<'a>| {
            let &Token::BoolLiteral(value) = tokens.iter_elements().next().unwrap().get_token() else { panic!() };
            Value::BLiteral(value)
        }),
        map(tag(tokens![TokenType::I32Literal]), |tokens: Stream<'a>| {
            let &Token::I32Literal(value) = tokens.iter_elements().next().unwrap().get_token() else { panic!() };
            Value::I32Literal(value)
        }),
        map(tag(tokens![TokenType::RuneLiteral]), |tokens: Stream<'a>| {
            let &Token::RuneLiteral(value) = tokens.iter_elements().next().unwrap().get_token() else { panic!() };
            Value::RuneLiteral(value)
        }),
        map(tag(tokens![TokenType::StringLiteral]), |tokens: Stream<'a>| {
            let Token::StringLiteral(value) = tokens.iter_elements().next().unwrap().get_token().clone() else { panic!() };
            Value::StringLiteral(value)
        }),
        parse_closure,
    ))(input)
}

pub fn parse_closure<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Value, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut arguments = None;
    let mut computation = None;

    let (stream, _) = context("closure", apply((
        alt((
            keep(&mut arguments, pipe_brackets(separated_list0(list_separator, identifier))),
            skip(single_tag(Symbol::LogicalOr)),
        )),
        cut(keep(&mut computation, parse_expression)),
    )))(input)?;

    Ok((stream, Value::Closure { arguments: arguments.unwrap_or_else(|| Vec::new()), computation: Box::new(computation.unwrap()) }))
}

pub fn parse_value_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    map(parse_value, |value| Expression::Value(Box::new(value)))(input)
}

pub fn parse_variable<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    map(tag(tokens![TokenType::Identifier]), |tokens: Stream<'a>| {
        let Token::Identifier(value) = tokens.iter_elements().next().unwrap().get_token().clone() else { panic!() };
        Expression::VarValue(Identifier(value))
    })(input)
}