use super::*;

pub fn parse_unary_op_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut is_unary = false;
    let mut operator = PNone;
    let mut expr0 = PNone;
    let mut expr1 = PNone;

    let (stream, _) = context("unary expression", alt((
        has_success(&mut is_unary, apply((
            keep(&mut operator, parse_unary_operator),
            cut(keep(&mut expr0, parse_function_call_expression)),
        ))),
        keep(&mut expr1, parse_function_call_expression),
    )))(input)?;

    if !is_unary {
        Ok((stream, expr1.take()))
    } else {
        Ok((stream, Expression::UnaryOp(operator.take(), Box::new(expr0.take()))))
    }
}

pub fn parse_binary_op_expression<'a, E>(input: Stream<'a>) -> IResult<Stream<'a>, Expression, E>
    where E: ParseError<Stream<'a>> + ContextError<Stream<'a>>
{
    let mut operator = PNone;
    let mut is_binary = false;
    let mut lexpr = PNone;
    let mut rexpr = PNone;

    let (stream, _) = context("binary expression", apply((
        keep(&mut lexpr, parse_unary_op_expression),
        opt_success(&mut is_binary,
            opt(apply((
                keep(&mut operator, parse_binary_operator),
                cut(keep(&mut rexpr, parse_binary_op_expression)),
            )))
        ),
    )))(input)?;

    if is_binary {
        Ok((stream, Expression::BinaryOp(Box::new(lexpr.take()), operator.take(), Box::new(rexpr.take()))))
    } else {
        Ok((stream, lexpr.take()))
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