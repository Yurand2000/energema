use super::*;

impl Interpreter {
    pub(super) fn interpret_fn_call((function, arguments): (Box<IExpression>, Vec<IExpression>), env: &mut Environment) -> Result<IExpression, String> {
        match *function {
            IExpression::Value(_) => (),
            IExpression::EffectHandling { effect, arguments: eff_arguments, computation, environment } => {
                return Ok(IExpression::EffectHandling { effect, arguments: eff_arguments, environment,
                    computation: Box::new(IExpression::FunctionCall { function: computation, arguments })
                });
            },
            _ => {
                let res = Self::interpret_next(*function, env)?;
                return Ok(IExpression::FunctionCall { function: Box::new(res), arguments });
            }
        };
        
        let (effect_data, arguments) = Self::export_fn_args_effect(arguments);
        if let Some((effect, eff_arguments, eff_environment)) = effect_data {
            Ok(IExpression::EffectHandling { effect, arguments: eff_arguments, environment: eff_environment,
                computation: Box::new(IExpression::FunctionCall { function, arguments })
            })
        } else {
            let (arguments, reduced) = Self::interpret_fn_args(arguments, env)?;
            if reduced {
                let IExpression::Value(fn_obj) = *function else { return Err(format!("Function expression is not a value")); };
                match *fn_obj {
                    IValue::Function(function) =>
                        Self::execute_function((function, arguments), env),
                    IValue::NativeFunction(native_fun) =>
                        Self::execute_native_function((native_fun, arguments), env),
                    IValue::Continuation { expression, previous_environment } =>
                        Self::execute_continuation((expression, previous_environment, arguments), env),
                    _ => {
                        Err(format!("Function expression must either be an identifier or a continuation"))
                    }
                }                
            } else {
                Ok(IExpression::FunctionCall { function, arguments })
            }
        }
    }

    fn execute_function((function, arguments): (IFunDeclaration, Vec<IExpression>), env: &mut Environment) -> Result<IExpression, String> {
        if function.arguments.len() != arguments.len() {
            Err(
                format!("Argument number mismatch for function: {}. Expected {} arguments, but found {}.",
                function.name, function.arguments.len(), arguments.len())
            )?;
        }
            
        let arguments = Self::fn_args_to_values(arguments);
            
        env.push_block();
        for (id, value) in function.arguments.iter().zip(arguments.into_iter()) {
            env.new_identifier(id, value);
        }
            
        Ok(IExpression::Block(function.expression.clone()))
    }

    fn execute_native_function((function, arguments): (NativeFun, Vec<IExpression>), _env: &mut Environment) -> Result<IExpression, String> {
        let arguments = Self::fn_args_to_values(arguments);
            
        function.0(arguments)
            .map(|value| IExpression::Value(Box::new(value)))
    }

    fn execute_continuation((expr, previous_environment, arguments): (Box<IExpression>, Vec<EnvBlock>, Vec<IExpression>), env: &mut Environment) -> Result<IExpression, String> {
        if arguments.len() > 1 {
            return Err(format!("Argument number mismatch for continuation: Expected 0 or 1 arguments, but found {}.", arguments.len()));
        }
        let arguments = Self::fn_args_to_values(arguments);
        
        env.restore_environment(previous_environment);
        env.push_block();

        if !arguments.is_empty() {
            env.new_identifier_str("$effret", arguments.into_iter().next().unwrap());
        }

        Ok(*expr)
    }

    pub(super) fn interpret_fn_args(arguments: Vec<IExpression>, env: &mut Environment) -> Result<(Vec<IExpression>, bool), String> {
        let mut reduced = true;
        arguments.into_iter()
            .fold(Ok(Vec::new()), |vect, expr| {
                let mut vect = vect?;

                if !reduced {
                    vect.push(expr);
                    return Ok(vect);
                }

                match expr {
                    IExpression::Value(_) => {
                        vect.push(expr);
                    },
                    _ => {
                        let res = Self::interpret_next(expr, env)?;
                        vect.push(res);
                        reduced = false;
                    }
                }

                Ok(vect)
            }).map(|vec| (vec, reduced))
    }

    pub(super) fn export_fn_args_effect(arguments: Vec<IExpression>) -> (Option<(Effect, Vec<IValue>, Vec<EnvBlock>)>, Vec<IExpression>) {
        let mut reduced = true;
        let mut out_effect = None;
        let arguments = arguments.into_iter()
            .fold(Vec::new(), |mut vect, expr| {
                match (reduced, expr) {
                    (true, IExpression::EffectHandling { effect, arguments, computation, environment }) => {
                        vect.push(*computation);
                        out_effect = Some((effect, arguments, environment));
                        reduced = false;
                    },
                    (_, expr) => {
                        vect.push(expr);
                    },
                };

                vect
            });

        (out_effect, arguments)
    }

    pub fn fn_args_to_values(arguments: Vec<IExpression>) -> Vec<IValue> {
        arguments.into_iter().map(|expr| {
            let IExpression::Value(value) = expr else { panic!(); };
            *value
        }).collect()
    }
}