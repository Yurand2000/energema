use super::*;

impl Interpreter {
    pub(super) fn interpret_handler_install((handler, computation): (Identifier, Box<IExpression>), env: &mut Environment) -> Result<IExpression, String> {
        env.push_handler(handler);
        Ok(IExpression::Handling(Box::new(IExpression::Block(computation))))
    }

    pub(super) fn interpret_handling(expr: Box<IExpression>, env: &mut Environment) -> Result<IExpression, String> {
        match *expr {
            IExpression::Value(value) => {
                let handler = env.get_handler().cloned()
                    .ok_or(format!("Definition for handler {} not found!", env.get_handler_name()))?;

                env.pop_handler();
                if let Some(IReturnHandlerDeclaration { ret_arg, expression }) = handler.return_handler {
                    env.push_block();
                    env.new_identifier(&ret_arg.id, *value);
                    Ok(IExpression::Block(expression))
                } else {
                    Ok(IExpression::Value(value))
                }
            },
            IExpression::EffectHandling { effect, arguments, computation, mut environment } => {
                let handler = env.get_handler()
                    .ok_or(format!("Definition for handler {} not found!", env.get_handler_name()))?;

                let handler_effect = handler.effect_handlers.iter()
                    .find(|&handler| {
                        handler.effect == effect
                    }).cloned();

                environment.push( env.pop_handler() );
                if let Some(IEffectHandlerDeclaration { effect: heffect, arguments: harguments, expression: hexpression }) = handler_effect {
                    let handler_effect = env.search_effect(&heffect).cloned()
                        .ok_or_else(|| format!("Definition for effect \"{:?}\" not found.", heffect))?;

                    env.push_block();
                    for (argument_name, argument) in harguments.iter().zip(arguments.into_iter()) {
                        env.new_identifier(&argument_name.id, argument);
                    }

                    let continuation = IExpression::Continuation {
                        expression: Box::new(IExpression::Handling(computation)),
                        previous_environment: environment,
                    };

                    let closure = IValue::Closure {
                        arguments: handler_effect.out_type.iter().map(|_| "$effret".into()).collect(),
                        computation: Box::new(continuation),
                        environment: ActivationRecord::default()
                    };

                    env.new_identifier_str("continuation", closure);

                    Ok(IExpression::Block(hexpression))
                } else {
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