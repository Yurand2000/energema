use super::*;

impl Interpreter {
    pub fn interpret_sequencing((expr, succ): (Box<IExpression>, Box<IExpression>), env: &mut Environment) -> Result<IExpression, String> {
        Ok(match *expr {
            IExpression::Value(_) => *succ,
            IExpression::EffectHandling { effect, arguments, computation, environment } =>
                IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::Sequencing(computation, succ))
                },
            _ => {
                let res = Self::interpret_next(*expr, env)?;
                IExpression::Sequencing(Box::new(res), succ)
            },
        })
    }

    pub fn interpret_block(expr: Box<IExpression>, env: &mut Environment) -> Result<IExpression, String> {
        match *expr {
            IExpression::Value(_) => {
                env.pop_block();
                Ok(*expr)
            },
            IExpression::EffectHandling { effect, arguments, computation, environment } => {
                Ok(IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::Block(computation))
                })
            },
            _ => {
                let res = Self::interpret_next(*expr, env)?;
                Ok(IExpression::Block(Box::new(res)))
            }
        }
    }
    
    pub fn interpret_let((id, expr): (Identifier, Box<IExpression>), env: &mut Environment) -> Result<IExpression, String> {
        Ok(match *expr {
            IExpression::Value(value) => {
                env.new_identifier(&id, *value);
                IExpression::Value(Box::new(IValue::ULiteral))
            },
            IExpression::EffectHandling { effect, arguments, computation, environment } => {
                IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::Let { id, expression: computation })
                }
            },
            _ => {
                let res = Self::interpret_next(*expr, env)?;
                IExpression::Let{ id, expression: Box::new(res) }
            },
        })
    }
}