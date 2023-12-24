use nom::InputTake;

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
    delimited_split(parser, Symbol::OpenParenthesis, Symbol::CloseParentesis)
}

pub fn brackets<'a, T, E>(parser: impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>) ->
    impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>
        where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    delimited_split(parser, Symbol::OpenBracket, Symbol::CloseBracket)
}

pub fn braces<'a, T, E>(parser: impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>) ->
    impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>
        where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    delimited_split(parser, Symbol::OpenBrace, Symbol::CloseBrace)
}

pub fn angle_brackets<'a, T, E>(parser: impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>) ->
    impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>
        where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    delimited_split(parser, Symbol::SmallerThan, Symbol::GreaterThan)
}

pub fn pipe_brackets<'a, T, E>(parser: impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>) ->
    impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>
        where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    delimited(
        single_tag(Symbol::Pipe),
        parser,
        single_tag(Symbol::Pipe)
    )
}

fn get_parenthesized_generic<'a, E>(stream: Stream<'a>, open_symbol: Symbol, close_symbol: Symbol) -> IResult<Stream<'a>, Stream<'a>, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let open_token: TokenType = open_symbol.clone().into();
    let close_token: TokenType = close_symbol.clone().into();

    let (next, _) = single_tag(open_symbol)(stream.clone())?;
    let mut counter = 1;
    
    for (i, token) in next.iter_indices() {
        if token == close_token {
            counter -= 1; 
        } else if token == open_token {
            counter += 1;
        }

        if counter == 0 {
            let (next, content) = next.take_split(i);
            let (next, _) = single_tag(close_symbol)(next)?;
            return Ok((next, content))
        }
    }

    Err(Err::Error(E::from_error_kind(stream, nom::error::ErrorKind::RegexpMatch)))
}

fn delimited_split<'a, T, E>(mut parser: impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>, open_symbol: Symbol, close_symbol: Symbol) ->
    impl FnMut(Stream<'a>) -> IResult<Stream<'a>, T, E>
        where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    move |input| {
        let (next, inner) = get_parenthesized_generic(input, open_symbol.clone(), close_symbol.clone())?;
        let (_, result) = parser(inner)?;
        Ok((next, result))
    }
}

#[test]
fn test_single_tag() {
    let data = tokenize("effect ->").unwrap();
    let stream = TokenStream::new(&data);

    let (next, _) = single_tag::<Keyword, TestError>(Keyword::Effect)(stream).expect("expected keyword 'effect'");
    single_tag::<Symbol, TestError>(Symbol::Arrow)(next).expect("expected symbol '->'");
}

#[test]
fn test_parenthesized_generic() {
    let data = tokenize("{ if a { 1 } else { false } effect } ").unwrap();
    let stream = TokenStream::new(&data);

    let (next, parenthesized) = get_parenthesized_generic::<'_, TestError>(stream, Symbol::OpenBrace, Symbol::CloseBrace).expect("expected braces pair");
    let (_, _) = single_tag::<TokenType, TestError>(TokenType::Eof)(next).expect("expected return after braces");

    let (next, _) = single_tag::<Keyword, TestError>(Keyword::If)(parenthesized).expect("expected if inside first braces");
    let (next, _) = single_tag::<TokenType, TestError>(TokenType::Identifier)(next).expect("expected identifier");

    let (next, then_b) = get_parenthesized_generic::<'_, TestError>(next, Symbol::OpenBrace, Symbol::CloseBrace).expect("expected braces pair after if clause");
    let (_, _) = single_tag::<TokenType, TestError>(TokenType::I32Literal)(then_b).expect("expected number in then branch");

    let (next, _) = single_tag::<Keyword, TestError>(Keyword::Else)(next).expect("expected else after then branch");
    let (next, else_b) = get_parenthesized_generic::<'_, TestError>(next, Symbol::OpenBrace, Symbol::CloseBrace).expect("expected braces pair after else");
    let (_, _) = single_tag::<TokenType, TestError>(TokenType::BoolLiteral)(else_b).expect("expected number in else branch");

    let (_, _) = single_tag::<Keyword, TestError>(Keyword::Effect)(next).expect("expected effect after else branch");
}