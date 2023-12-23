use super::*;

impl Interpreter {
    pub fn next(&mut self) -> Result<(), String> {
        let mut old_expression = IExpression::Value(Box::new(IValue::ULiteral));
        std::mem::swap(&mut self.expression, &mut old_expression);
        self.expression = Self::interpret_next(old_expression, &mut self.environment)?;
        Ok(())
    }

    pub fn print_expression(&self) {
        println!("{}", self.expression);
    }

    pub fn has_next(&self) -> bool {
        if let IExpression::Value(_) = &self.expression { false } else { true }
    }

    pub fn restart(&mut self) -> Result<(), String> {
        let Some(main) = self.environment.get_main_function() else { return Err(format!("Main function not found!")); };
        self.expression = (*main.expression).into();
        Ok(())
    }

    pub fn run_to_completition(&mut self) -> Result<String, String> {
        while self.has_next() {
            self.next()?;
        }

        let mut old_expression = IExpression::Value(Box::new(IValue::ULiteral));
        std::mem::swap(&mut self.expression, &mut old_expression);
        let IExpression::Value(val) = old_expression else { panic!("unexpected panic. Unwrapping a non-value expression!") };
        Ok(format!("Program returned {}", self.value_to_string(*val)))
    }    

    fn value_to_string(&self, value: IValue) -> String {
        match value {
            IValue::ULiteral => format!("(): unit"),
            IValue::BLiteral(value) => format!("{}: bool", value),
            IValue::I32Literal(value) => format!("{}: i32", value),
            IValue::RuneLiteral(_) => todo!(),
            IValue::StringLiteral(_) => todo!(),
            IValue::Continuation { .. } => format!("!: continuation"),
            IValue::Function(_) => todo!(),
            IValue::NativeFunction(_) => todo!(),
        }
    }
}