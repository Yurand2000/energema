use super::*;

pub fn parse_if_expression(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    let mut guard = None;
    let mut then_b = None;
    let mut else_b = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::If)),
        keep(&mut guard, parse_expression_no_sequencing),
        delimited(
            single_tag(Symbol::OpenBrace),
            keep(&mut then_b, parse_expression),
            single_tag(Symbol::CloseBrace)
        ),
        skip(single_tag(Keyword::Else)),
        delimited(
            single_tag(Symbol::OpenBrace),
            keep(&mut else_b, parse_expression),
            single_tag(Symbol::CloseBrace)
        ),
    ))(input)?;

    Ok((stream, Expression::If { guard: Box::new(guard.unwrap()), then_b: Box::new(then_b.unwrap()), else_b: Box::new(else_b.unwrap()) }))
}

pub fn parse_while_expression(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    let mut guard = None;
    let mut block = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::While)),
        keep(&mut guard, parse_expression),
        delimited(
            single_tag(Symbol::OpenBrace),
            keep(&mut block, parse_expression),
            single_tag(Symbol::CloseBrace)
        ),
    ))(input)?;

    Ok((stream, Expression::While { guard: Box::new(guard.unwrap()), block: Box::new(block.unwrap()) }))
}