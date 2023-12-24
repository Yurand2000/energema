use super::*;

impl Interpreter {
    pub(super) fn interpret_variable(id: Identifier, env: &mut Environment) -> Result<IExpression, String> {
        let value =
            if id.0 == "$effret" {
                env.pop_identifier(&id)
            } else {
                env.search_identifier(&id)  
            }
            .ok_or(format!("Cannot find value for variable {}", id))?;


        Ok(IExpression::Value(Box::new(value)))
    }
}