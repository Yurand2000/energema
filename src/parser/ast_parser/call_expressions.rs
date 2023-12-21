use super::*;

pub fn parse_value_call_expression(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    let mut expr = None;

    let (stream, _) = apply((
        keep(&mut expr, parse_expression_no_sequencing),
        skip(single_tag(Symbol::OpenParenthesis)),
        skip(single_tag(Symbol::CloseParentesis)),
    ))(input)?;

    Ok((stream, Expression::ValueCall { expression: Box::new(expr.unwrap()) }))
}

pub fn parse_function_call_expression(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    let mut function = None;
    let mut arguments = None;

    let (stream, _) = apply((
        keep(&mut function, identifier),
        keep(&mut arguments, parenthesis(separated_list1(list_separator, parse_expression_no_sequencing))),
    ))(input)?;

    Ok((stream, Expression::FunCall { function: function.unwrap(), arguments: arguments.unwrap() }))
}

pub fn parse_effect_call_expression(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    let mut effect = None;
    let mut arguments = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::Perform)),
        cut(apply((
            keep(&mut effect, parse_effect_name),
            keep(&mut arguments, separated_list1(list_separator, parse_expression_no_sequencing)),
        ))),
    ))(input)?;

    Ok((stream, Expression::EffCall { effect: effect.unwrap(), arguments: arguments.unwrap() }))
}

pub fn parse_handler_install_expression(input: TokenStream<LocatedToken>) -> IResult<TokenStream<LocatedToken>, Expression> {
    let mut handler = None;
    let mut expr = None;

    let (stream, _) = apply((
        skip(single_tag(Keyword::With)),
        cut(apply((
            keep(&mut handler, identifier),
            skip(single_tag(Symbol::OpenParenthesis)),
            skip(single_tag(Symbol::CloseParentesis)),
            keep(&mut expr, parse_block),
        ))),
    ))(input)?;

    Ok((stream, Expression::Handling { handler: handler.unwrap(), computation: Box::new(expr.unwrap()) }))
}