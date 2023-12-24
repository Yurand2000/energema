use super::*;

impl Interpreter {
    pub fn next(&mut self) -> Result<(), String> {
        let mut old_expression = IExpression::Value(Box::new(IValue::ULiteral));
        std::mem::swap(&mut self.expression, &mut old_expression);
        self.expression = Self::interpret_next(old_expression, &mut self.environment)?;
        Ok(())
    }

    pub fn print_state(&self) -> String {
        format!(
            "EXPRESSION:\n{}\n\nHandler Stack size: {}\nCall Stack size: {}",
            self.expression,
            self.environment.call_stack.len() - 1,
            self.environment.call_stack.last().unwrap().get_stack_size(),
        )
    }

    pub fn has_next(&self) -> bool {
        if let IExpression::Value(_) = &self.expression { false } else { true }
    }

    pub fn restart(&mut self) -> Result<(), String> {
        let Some(main) = self.environment.get_main_function() else { return Err(format!("Main function not found!")); };
        self.expression = IExpression::Handling(Box::new(IExpression::Block(main.expression)));
        self.environment.reset();
        Ok(())
    }

    pub fn run_to_completition(&mut self) -> Result<String, String> {
        while self.has_next() {
            self.next()?;
        }

        let mut old_expression = IExpression::Value(Box::new(IValue::ULiteral));
        std::mem::swap(&mut self.expression, &mut old_expression);
        let IExpression::Value(val) = old_expression else { panic!("unexpected panic. Unwrapping a non-value expression!") };

        let result = format!("Program returned {}", self.value_to_string(*val));
        if self.environment.call_stack.is_empty() {
            Ok(result)
        } else {
            Err(format!("{}, but call stack was not emptied!\nHandler Stack size: {}\nCall Stack size: {}",
                result,
                self.environment.call_stack.len() - 1,
                self.environment.call_stack.last().unwrap().get_stack_size()
            ))
        }
    }    

    fn value_to_string(&self, value: IValue) -> String {
        match value {
            IValue::ULiteral => format!("(): unit"),
            IValue::BLiteral(value) => format!("{}: bool", value),
            IValue::I32Literal(value) => format!("{}: i32", value),
            IValue::RuneLiteral(value) => format!("{}: rune", value),
            IValue::StringLiteral(value) => format!("{}: string", value),
            IValue::Function(_) => todo!(),
            IValue::NativeFunction(_) => todo!(),
            IValue::Closure{ .. } => todo!(),
        }
    }
}