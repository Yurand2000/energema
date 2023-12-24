use super::*;

impl Interpreter {
    pub(super) fn get_type_of_value(value: &IValue, env: &Environment) -> Option<Type> {
        Some(match value {
            IValue::ULiteral => Type::Unit,
            IValue::BLiteral(_) => Type::Bool,
            IValue::I32Literal(_) => Type::I32,
            IValue::RuneLiteral(_) => Type::Rune,
            IValue::StringLiteral(_) => Type::String,
            IValue::Continuation{ expression: expr, .. } => Type::Computation(Box::new(
                Self::get_type_of_expression(expr.as_ref(), env)
            )),
            IValue::Function(_) => todo!(),
            IValue::NativeFunction(_) => todo!(),
            IValue::Closure { .. } => todo!(),
        })
    }

    pub(super) fn get_type_of_expression(_expr: &IExpression, _env: &Environment) -> ComputationType {
        todo!()
    }
}