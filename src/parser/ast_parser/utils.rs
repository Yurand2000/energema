use super::*;

pub fn single_tag<T: Into<TokenType> + Clone>(token: T) -> impl FnMut(TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, TokenStream<LocatedToken>> {
    move |input: TokenStream<LocatedToken>| {
        tag(tokens!(token.clone()))(input)
    }
}

pub fn list_separator(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, TokenStream<LocatedToken>> {
    tag(tokens!(Symbol::Comma))(input)
}

#[test]
fn test_single_tag() {
    let data = tokenize("effect ->").unwrap();
    let stream = TokenStream::new(&data);

    let (next, _) = single_tag(Keyword::Effect)(stream).expect("expected keyword 'effect'");
    single_tag(Symbol::Arrow)(next).expect("expected symbol '->'");
}