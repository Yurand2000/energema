use nom::{
    IResult,
    character::complete::*,
    bytes::complete::*,
    sequence::*,
    branch::*,
    combinator::*, 
    multi::*,
};

use super::ast::*;

pub fn parse_code(code: &str) -> Result<Vec<Declaration>, String> {
    match tuple((many0(declaration), space_parser0, eof))(code) {
        Ok((_, (declarations, _, _))) => Ok(declarations),
        Err(err) => Err(err.to_string()),
    }
}

fn declaration(input: &str) -> IResult<&str, Declaration> {
    alt((
        map(function_declaration, |decl| Declaration::Function(decl)),
        map(handler_declaration, |decl| Declaration::Handler(decl)),
        map(effect_declaration, |decl| Declaration::Effect(decl)),
    ))(input)
}

fn function_declaration(input: &str) -> IResult<&str, FunDeclaration> {
    let (next, _) = space_parser0(input)?;
    let (next, _) = tag("fn")(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, name) = identifier(next)?;
    let (next, _) = space_parser0(next)?;
    let (next, _) = char('(')(next)?;
    let (next, arguments) = separated_list0(list_separator, identifier)(next)?;
    let (next, _) = char(')')(next)?;
    let (out, body) = function_body(next)?;

    Ok((out, FunDeclaration{ name, arguments, expression: Box::new(body) }))
}

fn function_body(input: &str) -> IResult<&str, Expression> {
    let (next, _) = space_parser0(input)?;
    let (next, _) = char('{')(next)?;
    let (next, body) = expression_parser(next)?;
    let (next, _) = space_parser0(next)?;
    let (out, _) = char('}')(next)?;

    Ok((out, body))
}

fn value_parser(input: &str) -> IResult<&str, Value> {
    let (next, _) = space_parser0(input)?;

    alt((
        value(Value::ULiteral, tag("()")),
        value(Value::BLiteral(true), tag("true")),
        value(Value::BLiteral(false), tag("false")),
        map_res(digit1, |str| i32::from_str_radix(str, 10).map(|value| Value::I32Literal(value))),
        map(identifier, |id| Value::Var(id)),
        //missing runes and strings
    ))(next)
}

fn expression_parser(input: &str) -> IResult<&str, Expression> {
    let (next, _) = space_parser0(input)?;
    let (next, lexpr) = expression_parser_no_sequencing(next)?;
    let (out, rexpr) = opt(map(
        tuple((
            sequencing_separator,
            expression_parser
        )), |(_, rexpr)| rexpr))(next)?;

    match rexpr {
        Some(rexpr) => Ok((out, Expression::Sequencing(Box::new(lexpr), Box::new(rexpr)))),
        None => Ok((out, lexpr)),
    }
}

fn expression_parser_no_sequencing(input: &str) -> IResult<&str, Expression> {
    alt((
        let_expression,
        if_expression,
        //while expression,
        function_call_expression,
        effect_call_expression,
        handler_install_expression,        
        unary_op_expression,
        binary_op_expression,
        map(value_parser, |value| Expression::Value(Box::new(value))),
    ))(input)
}

fn let_expression(input: &str) -> IResult<&str, Expression> {
    let (next, _) = space_parser0(input)?;
    let (next, _) = tag("let")(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, id) = identifier(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, _) = char('=')(next)?;
    let (next, _) = space_parser1(next)?;
    let (out, expression) = expression_parser_no_sequencing(next)?;

    Ok((out, Expression::Let { id, expression: Box::new(expression) }))
}

fn if_expression(input: &str) -> IResult<&str, Expression> {
    let (next, _) = space_parser0(input)?;
    let (next, _) = tag("if")(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, guard) = expression_parser_no_sequencing(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, _) = char('{')(next)?;
    let (next, then_b) = expression_parser_no_sequencing(next)?;
    let (next, _) = char('}')(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, _) = tag("else")(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, _) = char('{')(next)?;
    let (next, else_b) = expression_parser_no_sequencing(next)?;
    let (out, _) = char('}')(next)?;

    Ok((out, Expression::If { guard: Box::new(guard), then_b: Box::new(then_b), else_b: Box::new(else_b) }))
}

fn function_call_expression(input: &str) -> IResult<&str, Expression> {
    let (next, _) = space_parser0(input)?;
    let (next, function) = identifier(next)?;
    let (next, _) = space_parser0(next)?;
    let (next, _) = char('(')(next)?;
    let (next, arguments) = separated_list1(list_separator, expression_parser_no_sequencing)(next)?;
    let (out, _) = char(')')(next)?;

    Ok((out, Expression::FunCall { function, arguments }))
}

fn effect_call_expression(input: &str) -> IResult<&str, Expression> {
    let (next, _) = space_parser0(input)?;
    let (next, _) = tag("perform")(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, effect) = effect(next)?;
    let (next, _) = space_parser0(next)?;
    let (next, _) = char('(')(next)?;
    let (next, arguments) = separated_list1(list_separator, expression_parser_no_sequencing)(next)?;
    let (out, _) = char(')')(next)?;

    Ok((out, Expression::EffCall { effect, arguments }))
}

fn handler_install_expression(input: &str) -> IResult<&str, Expression> {
    let (next, _) = space_parser0(input)?;
    let (next, _) = tag("with")(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, handler) = identifier(next)?;
    let (next, _) = space_parser0(next)?;
    let (next, _) = tag("()")(next)?;
    let (out, expression) = function_body(next)?;

    Ok((out, Expression::Handling { handler, computation: Box::new(expression) }))
}

fn unary_op_expression(input: &str) -> IResult<&str, Expression> {
    let (next, _) = space_parser0(input)?;
    let (next, op) = unary_operator(next)?;
    let (next, _) = space_parser0(next)?;
    let (out, expr) = expression_parser_no_sequencing(next)?;

    Ok((out, Expression::UnaryOp(op, Box::new(expr))))
}

fn binary_op_expression(input: &str) -> IResult<&str, Expression> {
    let (next, _) = space_parser0(input)?;
    let (next, _) = char('(')(next)?;
    let (next, _) = space_parser0(next)?;
    let (next, lexpr) = expression_parser_no_sequencing(next)?;
    let (next, _) = space_parser0(next)?;
    let (next, op) = binary_operator(next)?;
    let (next, _) = space_parser0(next)?;
    let (next, rexpr) = expression_parser_no_sequencing(next)?;
    let (next, _) = space_parser0(next)?;
    let (out, _) = char(')')(next)?;

    Ok((out, Expression::BinaryOp(Box::new(lexpr), op, Box::new(rexpr))))
}

fn handler_declaration(input: &str) -> IResult<&str, HandlerDeclaration> {
    let (next, _) = space_parser0(input)?;
    let (next, _) = tag("handler")(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, name) = identifier(next)?;
    let (next, _) = space_parser0(next)?;
    let (next, _) = tag("()")(next)?;
    let (next, _) = space_parser0(next)?;
    let (next, _) = char('{')(next)?;
    let (next, return_handler) = opt(return_handler)(next)?;
    if return_handler.is_some() {
        let (next, effect_handlers) = 
            opt(second(pair(
                list_separator,
                separated_list0(list_separator, effect_handler)
            )))(next)?;
        let (next, _) = opt(list_separator)(next)?;
        let (next, _) = space_parser0(next)?;
        let (out, _) = char('}')(next)?;
        
        Ok((out, HandlerDeclaration{ name, return_handler, effect_handlers: effect_handlers.unwrap_or(Vec::new()) }))
    } else {
        let (next, effect_handlers) = separated_list0(list_separator, effect_handler)(next)?;
        let (next, _) = opt(list_separator)(next)?;
        let (next, _) = space_parser0(next)?;
        let (out, _) = char('}')(next)?;
    
        Ok((out, HandlerDeclaration{ name, return_handler, effect_handlers }))
    }

    //let (next, effect_handlers) = separated_list0(list_separator, effect_handler)(next)?;
    //Ok((out, HandlerDeclaration{ name, return_handler, effect_handlers }))
}

fn return_handler(input: &str) -> IResult<&str, (Type, Type, Box<Expression>)> {
    let (next, _) = space_parser0(input)?;
    let (next, _) = tag("return")(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, in_type) = type_string(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, out_type) = type_string(next)?;
    let (next, _) = space_parser1(next)?;
    let (out, body) = function_body(next)?;

    Ok( (out, (in_type, out_type, Box::new(body))) )
}

fn effect_handler(input: &str) -> IResult<&str, (Effect, Vec<Identifier>, Box<Expression>)> {
    let (next, _) = space_parser0(input)?;
    let (next, effect) = effect(next)?;
    let (next, _) = space_parser0(next)?;
    let (next, _) = char('(')(next)?;
    let (next, arguments) = separated_list0(list_separator, identifier)(next)?;
    let (next, _) = char(')')(next)?;
    let (out, body) = function_body(next)?;

    Ok( (out, (effect, arguments, Box::new(body))) )
}

fn effect_declaration(input: &str) -> IResult<&str, EffectDeclaration> {
    let (next, _) = space_parser0(input)?;
    let (next, _) = tag("effect")(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, name) = identifier(next)?;
    let (next, _) = char(':')(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, in_types) = separated_list1(list_separator, type_string)(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, _) = tag("->")(next)?;
    let (next, _) = space_parser1(next)?;
    let (next, out_type) = type_string(next)?;
    let (out, _) = char(';')(next)?;

    Ok((out, EffectDeclaration{ name, in_types, out_type }))
}

fn effect(input: &str) -> IResult<&str, Effect> {
    let (next, _) = space_parser0(input)?;
    let (out, eff_type) = identifier(next)?;

    Ok((out, Effect{ eff_type, name: None }))
}

fn type_string(input: &str) -> IResult<&str, Type> {
    alt((
        value(Type::Void, tag("~")),
        value(Type::Unit, tag("()")),
        value(Type::Bool, tag("bool")),
        value(Type::I32, tag("i32")),
        value(Type::Rune, tag("rune")),
        value(Type::String, tag("string")),
        map(computation_type_string, |typ| Type::Computation(Box::new(typ))),
        function_type,
        handler_type,
    ))(input)
}

fn function_type(input: &str) -> IResult<&str, Type> {
    let (next, _) = tag("fn")(input)?;
    let (next, _) = space_parser0(next)?;
    let (next, in_types) = delimited(
        char('('),
        separated_list0(list_separator, type_string),
        char(')')
    )(next)?;
    let (next, _) = space_parser0(next)?;
    let (next, _) = tag("->")(next)?;
    let (next, _) = space_parser0(next)?;
    let (out, out_type) = computation_type_string(next)?;

    Ok((out, Type::Fun { in_types, out_type: Box::new(out_type) }))
}

fn handler_type(input: &str) -> IResult<&str, Type> {
    let (next, _) = tag("handler")(input)?;
    let (next, _) = space_parser1(next)?;
    let (next, in_type) = computation_type_string(next)?;
    let (next, _) = space_parser1(next)?;
    let (out, out_type) = computation_type_string(next)?;

    Ok((out, Type::Handler { in_type: Box::new(in_type), out_type: Box::new(out_type) }))
}

fn computation_type_string(input: &str) -> IResult<&str, ComputationType> {
    let (next, typ) = type_string(input)?;
    let (out, effects) = computation_effects(next)?;

    Ok((out, ComputationType{ typ, effects }))
}

fn computation_effects(input: &str) -> IResult<&str, ComputationEffects> {
    let (next, _) = tag("!{")(input)?;
    let (next, inverted) = map(opt(tag("^")), |o| o.is_some())(next)?;
    let (next, effects) = separated_list0(list_separator, effect)(next)?;
    let (out, _) = char('}')(next)?;

    if !inverted {
        Ok((out, ComputationEffects::AtMost(effects.into_iter().collect())))
    } else {
        Ok((out, ComputationEffects::AllBut(effects.into_iter().collect())))
    }
}

fn unary_operator(input: &str) -> IResult<&str, UnaryOp> {
    value(UnaryOp::LNot, tag("!"))(input)
}

fn binary_operator(input: &str) -> IResult<&str, BinaryOp> {
    alt((
        value(BinaryOp::Add, tag("+")),
        value(BinaryOp::Sub, tag("-")),
        value(BinaryOp::Mul, tag("*")),
        value(BinaryOp::Div, tag("/")),
        value(BinaryOp::Mod, tag("%")),

        value(BinaryOp::LAnd, tag("&&")),
        value(BinaryOp::LOr, tag("||")),
        value(BinaryOp::LXor, tag("^^")),

        value(BinaryOp::Eq, tag("==")),
        value(BinaryOp::Ne, tag("!=")),
        value(BinaryOp::Le, tag(">=")),
        value(BinaryOp::Lt, tag("<")),
        value(BinaryOp::Gt, tag(">")),
        value(BinaryOp::Ge, tag(">=")),
    ))(input)
}

fn identifier(input: &str) -> IResult<&str, Identifier> {
    let (next, string) = recognize(
        pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_"))))
        )
    )(input)?;

    Ok( (next, Identifier(string.to_owned())) )
}

fn list_separator(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(
            char(','),
            space_parser0
        )
    )(input)
}

fn sequencing_separator(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(
            char(';'),
            space_parser0
        )
    )(input)
}

pub fn first<I, O1, O2, E, F>(mut parser: F) -> impl FnMut(I) -> IResult<I, O1, E>
    where F: nom::Parser<I, (O1, O2), E>,
{
  move |input: I| {
    let (input, (o1, _)) = parser.parse(input)?;
    Ok((input, o1))
  }
}

pub fn second<I, O1, O2, E, F>(mut parser: F) -> impl FnMut(I) -> IResult<I, O2, E>
    where F: nom::Parser<I, (O1, O2), E>,
{
  move |input: I| {
    let (input, (_, o2)) = parser.parse(input)?;
    Ok((input, o2))
  }
}

fn space_parser0(input: &str) -> IResult<&str, &str> {
    recognize(
        tuple((
            multispace0,
            opt(alt((
                single_line_comment,
                multi_line_comment,
            ))),
            multispace0,
        ))
    )(input)
}

fn space_parser1(input: &str) -> IResult<&str, &str> {
    recognize(
        tuple((
            multispace1,
            opt(alt((
                single_line_comment,
                multi_line_comment,
            ))),
            multispace0,
        ))
    )(input)
}

fn single_line_comment(input: &str) -> IResult<&str, &str> {
    recognize(
        tuple((
            tag("//"),
            is_not("\n\r"),
            line_ending
        ))
    )(input)
}

fn multi_line_comment(input: &str) -> IResult<&str, &str> {
    recognize(
        tuple((
            tag("/*"),
            take_until("*/"),
            tag("*/"),
        ))
    )(input)
}