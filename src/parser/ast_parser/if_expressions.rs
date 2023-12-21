use super::*;

pub fn parse_if_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut guard = None;
    let mut then_b = None;
    let mut else_b = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::If)),
        cut(apply((
            keep(&mut guard, parse_expression_no_sequencing),
            keep(&mut then_b, parse_block),
            skip(single_tag(Keyword::Else)),
            keep(&mut else_b, parse_block),
        ))),
    ))(input)?;

    Ok((stream, Expression::If { guard: Box::new(guard.unwrap()), then_b: Box::new(then_b.unwrap()), else_b: Box::new(else_b.unwrap()) }))
}

pub fn parse_while_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut guard = None;
    let mut block = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::While)),
        cut(apply((
            keep(&mut guard, parse_expression),
            keep(&mut block, parse_block),
        ))),
    ))(input)?;

    Ok((stream, Expression::While { guard: Box::new(guard.unwrap()), block: Box::new(block.unwrap()) }))
}