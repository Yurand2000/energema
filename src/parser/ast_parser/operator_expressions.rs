use super::*;

pub fn parse_unary_op_expression(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    let mut operator = None;
    let mut expr = None;

    let (stream, _) = apply((
        keep(&mut operator, parse_unary_operator),
        cut(keep(&mut expr, parse_expression_no_sequencing)),
    ))(input)?;

    Ok((stream, Expression::UnaryOp(operator.unwrap(), Box::new(expr.unwrap()))))
}

pub fn parse_binary_op_expression(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    let mut operator = None;
    let mut is_binary = None;
    let mut lexpr = None;
    let mut rexpr = None;

    let (stream, _) = apply((
        keep(&mut lexpr, parse_expression_no_sequencing),
        keep(&mut is_binary,
            opt(apply((
                keep(&mut operator, parse_binary_operator),
                cut(keep(&mut rexpr, parse_expression_no_sequencing)),
            )))
        ),
    ))(input)?;

    let (lexpr, is_binary) = (lexpr.unwrap(), is_binary.unwrap());
    if is_binary.is_some() {
        Ok((stream, Expression::BinaryOp(Box::new(lexpr), operator.unwrap(), Box::new(rexpr.unwrap()))))
    } else {
        Ok((stream, lexpr))
    }
}

pub fn parse_unary_operator(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, UnaryOp> {
    value(UnaryOp::LNot, single_tag(Symbol::Exclamation))(input)
}

pub fn parse_binary_operator(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, BinaryOp> {
    alt((
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
    ))(input)
}