use super::*;

impl Declarations {
    pub fn standard_library() -> Vec<(Identifier, NativeFun)> {
        vec![
            ("nativePrint".into(), NativeFun(&|values| {
                for value in values.into_iter() {
                    match value {
                        IValue::ULiteral => print!("{}", "unit"),
                        IValue::BLiteral(value) => print!("{}", value),
                        IValue::I32Literal(value) => print!("{}", value),
                        IValue::RuneLiteral(value) => print!("{}", value),
                        IValue::StringLiteral(value) => print!("{}", value),
                        IValue::Continuation { .. } => print!("{}", "computation"),
                        IValue::Function { .. } => print!("{}", "function"),
                        IValue::NativeFunction { .. } => print!("{}", "native function"),
                        IValue::Closure { .. } => print!("{}", "closure"),
                    };
                }
                println!("");
                Ok(IValue::ULiteral)
            }))
        ]
    }
}