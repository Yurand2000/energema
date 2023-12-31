use super::*;

pub fn parse_if_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut guard = PNone;
    let mut then_b = PNone;
    let mut else_b = PNone;

    let (stream, _) = apply((
        skip(single_tag(Keyword::If)),
        cut(apply((
            keep(&mut guard, parse_single_line_expression),
            keep(&mut then_b, parse_block_expression),
            skip(single_tag(Keyword::Else)),
            keep(&mut else_b, parse_block_expression),
        ))),
    ))(input)?;

    Ok((stream, Expression::If { guard: Box::new(guard.take()), then_b: Box::new(then_b.take()), else_b: Box::new(else_b.take()) }))
}

pub fn parse_while_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut guard = PNone;
    let mut block = PNone;

    let (stream, _) = apply((
        skip(single_tag(Keyword::While)),
        cut(apply((
            keep(&mut guard, parse_single_line_expression),
            keep(&mut block, parse_block_expression),
        ))),
    ))(input)?;

    Ok((stream, Expression::While { guard: Box::new(guard.take()), block: Box::new(block.take()) }))
}