use super::*;

impl Interpreter {
    pub fn interpret_handler_install((handler, computation): (Identifier, Box<IExpression>), env: &mut Environment) -> Result<IExpression, String> {
        env.push_handler(handler);
        Ok(IExpression::Handling(computation))
    }

    pub fn interpret_handling(expr: Box<IExpression>, env: &mut Environment) -> Result<IExpression, String> {
        match *expr {
            IExpression::Value(value) => {
                let handler = env.get_handler()
                    .ok_or(format!("Definition for handler {} not found!", env.get_handler_name()))?;

                if let Some((_, _, id, expr)) = &handler.return_handler {
                    let iexpr: IExpression = (*expr.clone()).into();
                    env.pop_handler();
                    env.push_block();
                    env.new_identifier(id, *value);
                    Ok(iexpr)
                } else {
                    Ok(IExpression::Value(value))
                }
            },
            IExpression::EffectHandling { effect, arguments, computation, mut environment } => {
                let handler = env.get_handler()
                    .ok_or(format!("Definition for handler {} not found!", env.get_handler_name()))?;

                let handler_effect = handler.effect_handlers.iter()
                    .find(|&(handler_effect, _, _)| {
                        handler_effect == &effect
                    }).cloned();

                if let Some((_, handler_args, handler_body)) = handler_effect {
                    let call_stack = env.detach_blocks();
                    for (argument_name, argument) in handler_args.iter().zip(arguments.into_iter()) {
                        env.new_identifier(argument_name, argument);
                    }

                    let continuation = IValue::Continuation {
                        expression: computation,
                        previous_environment: environment,
                        call_stack
                    };
                    env.new_identifier(&"continuation".into(), continuation);

                    Ok(IExpression::Block(handler_body.into()))
                } else {
                    environment.push( env.pop_handler() );
                    Ok(IExpression::EffectHandling { effect, arguments, environment,
                        computation: Box::new(IExpression::Handling(computation))
                    })
                }
            },
            _ => {
                let res = Self::interpret_next(*expr, env)?;
                Ok(IExpression::Handling(Box::new(res)))
            }
        }
    }
}