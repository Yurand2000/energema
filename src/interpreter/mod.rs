use std::collections::HashMap;

use super::ast::*;

mod interpreter_data;
mod declarations;
mod environment;
mod standard_library;
mod utils;
mod get_types;
mod interpret_value;
mod sequencing_expressions;
mod if_expressions;
mod fn_call_expression;
mod effect_call_expression;
mod handler_expressions;
mod operator_expressions;
mod closure_creation;

use interpreter_data::*;

#[derive(Clone)]
pub struct Declarations {
    functions: HashMap<Identifier, IFunDeclaration>,
    handlers: HashMap<Identifier, IHandlerDeclaration>,
    effects: HashMap<Identifier, EffectDeclaration>,

    native_functions: HashMap<Identifier, NativeFun>
}

struct Environment {
    declarations: Declarations,
    call_stack: Vec<EnvBlock>,
}

pub struct Interpreter {
    expression: IExpression,
    environment: Environment,
}

impl Interpreter {
    pub fn new(declarations: Declarations) -> Result<Self, String> {
        let mut interpreter = Self {
            environment: Environment::new(declarations),
            expression: IExpression::Value(Box::new(IValue::ULiteral))
        };

        interpreter.restart()?;
        Ok(interpreter)
    }

    fn interpret_next(expr: IExpression, env: &mut Environment) -> Result<IExpression, String> {
        match expr {
            IExpression::Value(_) |
            IExpression::EffectHandling { .. } =>
                Ok(expr),
            IExpression::VarValue(id) => 
                Self::interpret_variable(id, env),
            IExpression::Sequencing(expr, succ) =>
                Self::interpret_sequencing((expr, succ), env),
            IExpression::Let { id, expression } =>
                Self::interpret_let((id, expression), env),
            IExpression::If { guard, then_b, else_b } =>
                Self::interpret_if((guard, then_b, else_b), env),
            IExpression::While { guard, block } =>
                Self::interpret_while((guard, block), env),
            IExpression::FunctionCall { function, arguments } =>
                Self::interpret_fn_call((function, arguments), env),
            IExpression::EffectCall { effect, arguments } =>
                Self::interpret_effect_call((effect, arguments), env),
            IExpression::HandlingInstall { handler, computation } =>
                Self::interpret_handler_install((handler, computation), env),
            IExpression::UnaryOp(op, expr) =>
                Self::interpret_unary_op((op, expr), env),
            IExpression::BinaryOp(lexpr, op, rexpr) =>
                Self::interpret_binary_op((lexpr, op, rexpr), env),
            IExpression::Block(expr) =>
                Self::interpret_block(expr, env),
            IExpression::Handling(expr) =>
                Self::interpret_handling(expr, env),
            IExpression::ClosureCreate { arguments, computation } =>
                Self::interpret_closure_create((arguments, computation), env),
            IExpression::Continuation { expression, previous_environment } =>
                Self::interpret_continuation((expression, previous_environment), env),
        }
    }
}