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
            keep(&mut arguments, separated_list0(list_separator, identifier)),
            single_tag(Symbol::CloseParentesis)
        ),
        keep(&mut body, parse_function_body),
    ))(input)?;

    Ok((stream, FunDeclaration{ name: name.unwrap(), arguments: arguments.unwrap(), expression: Box::new(body.unwrap()) }))
}