use super::*;

impl Interpreter {
    pub(super) fn interpret_closure_create((arguments, expr): (Vec<Identifier>, Box<IExpression>), env: &mut Environment) -> Result<IExpression, String> {
        let captured = Self::get_variables_in_expression(&expr);
        let mut closure_environment = ActivationRecord::default();
        for capture in captured.into_iter() {
            let Some(value) = env.search_identifier(capture) else {
                return Err(format!("Closure tried to capture variable \"{}\", but it was not found in the current environment.", capture));
            };
            closure_environment.new_identifier(capture, value);
        }

        Ok(IExpression::Value(Box::new(IValue::Closure { arguments, computation: expr, environment: closure_environment })))
    }

    fn get_variables_in_expression(expr: &IExpression) -> Vec<&Identifier> {
        match expr {
            IExpression::Value(_) => vec![],
            IExpression::VarValue(var) => vec![var],
            IExpression::Sequencing(left, right) => {
                let mut left = Self::get_variables_in_expression(left);
                let mut right = Self::get_variables_in_expression(right);
                left.extend(right.drain(..));
                left
            },
            IExpression::Let { expression, .. } => Self::get_variables_in_expression(expression),
            IExpression::If { guard, then_b, else_b } => {
                let mut guard = Self::get_variables_in_expression(guard);
                let mut then_b = Self::get_variables_in_expression(then_b);
                let mut else_b = Self::get_variables_in_expression(else_b);
                guard.extend(then_b.drain(..));
                guard.extend(else_b.drain(..));
                guard
            },
            IExpression::While { guard, block } => {
                let mut guard = Self::get_variables_in_expression(guard);
                let mut block = Self::get_variables_in_expression(block);
                guard.extend(block.drain(..));
                guard
            },
            IExpression::FunctionCall { function, arguments } => {
                let ids = Self::get_variables_in_expression(function);
                arguments.iter().fold(ids, |mut accum, expr| {
                    let mut ids = Self::get_variables_in_expression(expr);
                    accum.extend(ids.drain(..));
                    accum
                })
            },
            IExpression::EffectCall { arguments, .. } => {
                arguments.iter().fold(Vec::new(), |mut accum, expr| {
                    let mut ids = Self::get_variables_in_expression(expr);
                    accum.extend(ids.drain(..));
                    accum
                })
            },
            IExpression::HandlingInstall { computation, .. } => Self::get_variables_in_expression(computation),
            IExpression::ClosureCreate { arguments, computation } => {
                let ids = Self::get_variables_in_expression(computation);
                ids.into_iter().filter(|id| !arguments.contains(id)).collect()
            },
            IExpression::UnaryOp(_, expr) => Self::get_variables_in_expression(expr),
            IExpression::BinaryOp(left, _, right) => {
                let mut left = Self::get_variables_in_expression(left);
                let mut right = Self::get_variables_in_expression(right);
                left.extend(right.drain(..));
                left
            },
            IExpression::Block(expr) => Self::get_variables_in_expression(expr),
            IExpression::Handling(expr) => Self::get_variables_in_expression(expr),
            IExpression::EffectHandling { computation, environment, .. } => {
                let ids = Self::get_variables_in_expression(computation);
                ids.into_iter().filter(|id| {
                    environment.iter().all(|env_block| {
                        env_block.search_identifier(id).is_none()
                    })
                }).collect()
            },
            IExpression::Continuation { expression, previous_environment } => {
                let ids = Self::get_variables_in_expression(expression);
                ids.into_iter().filter(|id| {
                    previous_environment.iter().all(|env_block| {
                        env_block.search_identifier(id).is_none()
                    })
                }).collect()
            },
        }
    }
}