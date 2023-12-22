use super::*;

impl Declarations {
    pub fn standard_library() -> Vec<(Identifier, NativeFun)> {
        vec![
            ("nativePrint".into(), NativeFun(&|values| {
                if values.len() != 1 {
                    Err(format!("nativePrint funcion accepts only one value!"))
                } else {
                    match values.into_iter().next().unwrap() {
                        IValue::ULiteral => println!("()"),
                        IValue::BLiteral(value) => println!("{}", value),
                        IValue::I32Literal(value) => println!("{}", value),
                        IValue::RuneLiteral(value) => println!("{}", value),
                        IValue::StringLiteral(value) => println!("{}", value),
                        IValue::Continuation { .. } => println!("computation"),
                        IValue::Function { .. } => println!("function"),
                        IValue::NativeFunction { .. } => println!("native function"),
                    };

                    Ok(IValue::ULiteral)
                }
            }))
        ]
    }
}