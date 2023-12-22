use super::*;

impl Interpreter {
    pub fn interpret_effect_call((effect, arguments): (Effect, Vec<IExpression>), env: &mut Environment) -> Result<IExpression, String> {
        let (effect_data, arguments) = Self::export_fn_args_effect(arguments);
        if let Some((out_effect, out_arguments, out_environment)) = effect_data {
            Ok(IExpression::EffectHandling { effect: out_effect, arguments: out_arguments, environment: out_environment,
                computation: Box::new(IExpression::EffectCall { effect, arguments })
            })
        } else {
            let (arguments, reduced) = Self::interpret_fn_args(arguments, env)?;

            if reduced {
                let arguments = Self::fn_args_to_values(arguments);
                let computation = Box::new(IExpression::VarValue("$effret".into()));

                Ok(IExpression::EffectHandling { effect, arguments, computation, environment: Vec::new() })
            } else {
                Ok(IExpression::EffectCall { effect, arguments })
            }
        }
    }
}