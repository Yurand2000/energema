use super::*;

pub fn parse_unary_op_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut operator = None;
    let mut expr0 = None;
    let mut expr1 = None;

    let (stream, _) = context("unary expression", alt((
        apply((
            keep(&mut operator, parse_unary_operator),
            cut(keep(&mut expr0, parse_function_call_expression)),
        )),
        keep(&mut expr1, parse_function_call_expression),
    )))(input)?;

    if expr1.is_some() {
        Ok((stream, expr1.unwrap()))
    } else {
        Ok((stream, Expression::UnaryOp(operator.unwrap(), Box::new(expr0.unwrap()))))
    }
}

pub fn parse_binary_op_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut operator = None;
    let mut is_binary = None;
    let mut lexpr = None;
    let mut rexpr = None;

    let (stream, _) = context("binary expression", apply((
        keep(&mut lexpr, parse_unary_op_expression),
        keep(&mut is_binary,
            opt(apply((
                keep(&mut operator, parse_binary_operator),
                cut(keep(&mut rexpr, parse_binary_op_expression)),
            )))
        ),
    )))(input)?;

    let (lexpr, is_binary) = (lexpr.unwrap(), is_binary.unwrap());
    if is_binary.is_some() {
        Ok((stream, Expression::BinaryOp(Box::new(lexpr), operator.unwrap(), Box::new(rexpr.unwrap()))))
    } else {
        Ok((stream, lexpr))
    }
}

pub fn parse_unary_operator<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, UnaryOp, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    context("unary operator",alt((
        value(UnaryOp::LNot, single_tag(Symbol::Exclamation)),
        value(UnaryOp::Negate, single_tag(Symbol::Minus)),
    )))(input)
}

pub fn parse_binary_operator<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, BinaryOp, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    context("binary operator", alt((
        value(BinaryOp::Add, single_tag(Symbol::Plus)),
        value(BinaryOp::Sub, single_tag(Symbol::Minus)),
        value(BinaryOp::Mul, single_tag(Symbol::Times)),
        value(BinaryOp::Div, single_tag(Symbol::ForwardSlash)),
        value(BinaryOp::Mod, single_tag(Symbol::Modulo)),

        value(BinaryOp::LAnd, single_tag(Symbol::LogicalAnd)),
        value(BinaryOp::LOr, single_tag(Symbol::LogicalOr)),
        value(BinaryOp::LXor, single_tag(Symbol::LogicalXor)),

        value(BinaryOp::Eq, single_tag(Symbol::DoubleEqual)),
        value(BinaryOp::Ne, single_tag(Symbol::NotEqual)),
        value(BinaryOp::Le, single_tag(Symbol::SmallerOrEqual)),
        value(BinaryOp::Lt, single_tag(Symbol::SmallerThan)),
        value(BinaryOp::Gt, single_tag(Symbol::GreaterThan)),
        value(BinaryOp::Ge, single_tag(Symbol::GreaterOrEqual)),
    )))(input)
}