use super::*;

impl Interpreter {
    pub fn interpret_unary_op((op, expr): (UnaryOp, Box<IExpression>), env: &mut Environment) -> Result<IExpression, String> {
        match *expr {
            IExpression::Value(value) =>
                Self::execute_unary_op(op, *value, env),
            IExpression::EffectHandling { effect, arguments, computation, environment } => {
                Ok(IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::UnaryOp(op, computation))
                })
            },
            _ => {
                let res = Self::interpret_next(*expr, env)?;
                Ok(IExpression::UnaryOp(op, Box::new(res)))
            }
        }
    }

    fn execute_unary_op(op: UnaryOp, value: IValue, env: &mut Environment) -> Result<IExpression, String> {
        match (op, value) {
            (UnaryOp::LNot, IValue::BLiteral(value)) => Ok(IExpression::Value( Box::new(IValue::BLiteral(!value)) )),
            (op, value) => Err(format!("Unmatched unary operator {:?} to expression of type {:?}", op, Self::get_type_of_value(&value, env)))
        }
    }

    pub fn interpret_binary_op((lexpr, op, rexpr): (Box<IExpression>, BinaryOp, Box<IExpression>), env: &mut Environment) -> Result<IExpression, String> {
        match (*lexpr, *rexpr) {
            (IExpression::Value(lvalue), IExpression::Value(rvalue)) => {
                Self::execute_binary_op(*lvalue, op, *rvalue, env)
            },
            (lexpr @ IExpression::Value(_), IExpression::EffectHandling { effect, arguments, computation, environment }) => {
                Ok(IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::BinaryOp(Box::new(lexpr), op, computation))
                })
            },
            (lexpr @ IExpression::Value(_), rexpr) => {
                let rres = Self::interpret_next(rexpr, env)?;
                Ok(IExpression::BinaryOp(Box::new(lexpr), op, Box::new(rres)))
            },
            (IExpression::EffectHandling { effect, arguments, computation, environment }, rexpr) => {
                Ok(IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::BinaryOp(computation, op, Box::new(rexpr)))
                })
            },
            (lexpr, rexpr) => {
                let lres = Self::interpret_next(lexpr, env)?;
                Ok(IExpression::BinaryOp(Box::new(lres), op, Box::new(rexpr)))
            },
        }
    }

    fn execute_binary_op(lexpr: IValue, op: BinaryOp, rexpr: IValue, env: &mut Environment) -> Result<IExpression, String> {
        (match (lexpr, op, rexpr) {
            (IValue::BLiteral(lvalue), BinaryOp::LAnd, IValue::BLiteral(rvalue)) =>
                Ok(IValue::BLiteral(lvalue && rvalue)),
            (lexpr, op, rexpr) => 
                Err(format!("Unmatched binary operator {:?} to operands of type {:?} and {:?}",
                    op, Self::get_type_of_value(&lexpr, env), Self::get_type_of_value(&rexpr, env)))
        }).map(|value| {
            IExpression::Value( Box::new(value) )
        })
    }
}