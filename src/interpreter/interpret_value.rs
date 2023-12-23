use super::*;

impl Interpreter {
    pub(super) fn interpret_variable(id: Identifier, env: &mut Environment) -> Result<IExpression, String> {
        let value = env.search_identifier(&id)
            .ok_or(format!("Cannot find value for variable {}", id))?;

        Ok(IExpression::Value(Box::new(value)))
    }
}