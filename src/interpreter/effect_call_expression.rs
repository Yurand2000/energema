use super::*;

impl Interpreter {
    pub(super) fn interpret_effect_call((effect, arguments): (Effect, Vec<IExpression>), env: &mut Environment) -> Result<IExpression, String> {
        let (effect_data, arguments) = Self::export_fn_args_effect(arguments);
        if let Some((out_effect, out_arguments, out_environment)) = effect_data {
            Ok(IExpression::EffectHandling { effect: out_effect, arguments: out_arguments, environment: out_environment,
                computation: Box::new(IExpression::EffectCall { effect, arguments })
            })
        } else {
            let (arguments, reduced) = Self::interpret_fn_args(arguments, env)?;

            if reduced {
                let arguments = Self::fn_args_to_values(arguments);
                let effect_def = env.search_effect(&effect)
                    .ok_or_else(|| format!("Definition for effect \"{:?}\" not found.", effect))?;

                let computation = if effect_def.out_type.is_some() {
                    IExpression::VarValue("$effret".into())
                } else {
                    IExpression::Value(Box::new(IValue::ULiteral))
                };

                Ok(IExpression::EffectHandling { effect, arguments, computation: Box::new(computation), environment: Vec::new() })
            } else {
                Ok(IExpression::EffectCall { effect, arguments })
            }
        }
    }
}