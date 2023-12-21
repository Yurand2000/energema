use super::*;

pub fn single_tag<T: Into<TokenType> + Clone>(token: T) -> impl FnMut(TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, TokenStream<LocatedToken>> {
    move |input: TokenStream<LocatedToken>| {
        tag(tokens!(token.clone()))(input)
    }
}

pub fn list_separator(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, TokenStream<LocatedToken>> {
    tag(tokens!(Symbol::Comma))(input)
}

pub fn parenthesis<'a, T>(parser: impl FnMut(TokenStream<'a, LocatedToken>) -> IResult<TokenStream<'a, LocatedToken>, T>) ->
    impl FnMut(TokenStream<'a, LocatedToken>) -> IResult<TokenStream<'a, LocatedToken>, T>
{
    delimited(
        single_tag(Symbol::OpenParenthesis),
        parser,
        single_tag(Symbol::CloseParentesis)
    )
}

pub fn brackets<'a, T>(parser: impl FnMut(TokenStream<'a, LocatedToken>) -> IResult<TokenStream<'a, LocatedToken>, T>) ->
    impl FnMut(TokenStream<'a, LocatedToken>) -> IResult<TokenStream<'a, LocatedToken>, T>
{
    delimited(
        single_tag(Symbol::OpenBracket),
        parser,
        single_tag(Symbol::CloseBracket)
    )
}

pub fn braces<'a, T>(parser: impl FnMut(TokenStream<'a, LocatedToken>) -> IResult<TokenStream<'a, LocatedToken>, T>) ->
    impl FnMut(TokenStream<'a, LocatedToken>) -> IResult<TokenStream<'a, LocatedToken>, T>
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

    let (next, _) = single_tag(Keyword::Effect)(stream).expect("expected keyword 'effect'");
    single_tag(Symbol::Arrow)(next).expect("expected symbol '->'");
}