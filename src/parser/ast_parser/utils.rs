use super::*;

pub fn single_tag<'a, T, E>(token: T) -> impl FnMut(Stream<'a>) -> IResult<Stream<'a>, Stream<'a>, E>
    where T: Into<TokenType> + Clone, E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    move |input: Stream<'a>| {
        tag(tokens!(token.clone()))(input)
    }
}

pub fn list_separator<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Stream<'a>, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    tag(tokens!(Symbol::Comma))(input)
}

pub fn parenthesis<'a, T, E>(parser: impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>) ->
    impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>
        where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    delimited(
        single_tag(Symbol::OpenParenthesis),
        parser,
        single_tag(Symbol::CloseParentesis)
    )
}

pub fn brackets<'a, T, E>(parser: impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>) ->
    impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>
        where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    delimited(
        single_tag(Symbol::OpenBracket),
        parser,
        single_tag(Symbol::CloseBracket)
    )
}

pub fn braces<'a, T, E>(parser: impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>) ->
    impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>
        where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    delimited(
        single_tag(Symbol::OpenBrace),
        parser,
        single_tag(Symbol::CloseBrace)
    )
}

#[test]
fn test_single_tag() {
    let data = tokenize("effect ->").unwrap();
    let stream = TokenStream::new(&data);

    let (next, _) = single_tag::<Keyword, TestError>(Keyword::Effect)(stream).expect("expected keyword 'effect'");
    single_tag::<Symbol, TestError>(Symbol::Arrow)(next).expect("expected symbol '->'");
}