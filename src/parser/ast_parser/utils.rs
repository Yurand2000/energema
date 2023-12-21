use super::*;

pub fn single_tag<T: Into<TokenType> + Clone>(token: T) -> impl FnMut(TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, TokenStream<LocatedToken>> {
    move |input: TokenStream<LocatedToken>| {
        tag(tokens!(token.clone()))(input)
    }
}

pub fn list_separator(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, TokenStream<LocatedToken>> {
    tag(tokens!(Symbol::Comma))(input)
}