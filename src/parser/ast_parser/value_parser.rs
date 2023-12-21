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
        map(tag(tokens![TokenType::Identifier]), |tokens: Stream<'a>| {
            let Token::Identifier(value) = tokens.iter_elements().next().unwrap().get_token().clone() else { panic!() };
            Value::Var(Identifier(value))
        }),
    ))(input)
}