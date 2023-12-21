use super::*;

pub fn parse_function_declaration(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, FunDeclaration> {
    let mut name = None;
    let mut arguments = None;
    let mut body = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::Fn)),
        keep(&mut name, identifier),
        delimited(
            single_tag(Symbol::OpenParenthesis),
            keep(&mut arguments, separated_list1(list_separator, identifier)),
            single_tag(Symbol::CloseParentesis)
        ),
        keep(&mut body, parse_block),
    ))(input)?;

    Ok((stream, FunDeclaration{ name: name.unwrap(), arguments: arguments.unwrap(), expression: Box::new(body.unwrap()) }))
}

pub fn parse_block(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    let mut body = None;

    let (stream, _) = apply((
        delimited(
            single_tag(Symbol::OpenBrace),
            keep(&mut body, parse_expression), 
            single_tag(Symbol::CloseBrace)
        ),
    ))(input)?;

    Ok((stream, body.unwrap()))
}