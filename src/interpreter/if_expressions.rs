use super::*;

impl Interpreter {
    pub(super) fn interpret_if((guard, then_b, else_b): (Box<IExpression>, Box<IExpression>, Box<IExpression>), env: &mut Environment) -> Result<IExpression, String> {
        Ok(match *guard {
            IExpression::Value(value) => {
                match *value {
                    IValue::BLiteral(true) => {
                        env.push_block();
                        IExpression::Block(then_b)
                    },
                    IValue::BLiteral(false) => {
                        env.push_block();
                        IExpression::Block(else_b)
                    },
                    value => Err(format!("If expression guard, of type {:?}, is not of boolean type",
                        Self::get_type_of_value(&value, env)))?
                }
            },
            IExpression::EffectHandling { effect, arguments, computation, environment } => {
                IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::If { guard: computation, then_b, else_b })
                }
            },
            _ => {
                let res = Self::interpret_next(*guard, env)?;
                IExpression::If{ guard: Box::new(res), then_b, else_b }
            }
        })
    }

    pub(super) fn interpret_while((guard, block): (Box<IExpression>, Box<IExpression>), _env: &mut Environment) -> Result<IExpression, String> {
        let then_b = Box::new(
            IExpression::Sequencing(
                block.clone(),
                Box::new(IExpression::While { guard: guard.clone(), block })
            )
        );

        let else_b = Box::new(
            IExpression::Value(Box::new(
                IValue::ULiteral
            ))
        );

        Ok(IExpression::If { guard, then_b, else_b })
    }
}