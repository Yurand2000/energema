use std::io::{Read, Write};

use super::*;

impl Declarations {
    pub fn standard_library() -> Vec<(Identifier, NativeFun)> {
        vec![
            ("nativePrint".into(), NativeFun(&|args| {
                for arg in args.into_iter() {
                    match arg {
                        IValue::ULiteral => print!("{}", "unit"),
                        IValue::BLiteral(value) => print!("{}", value),
                        IValue::I32Literal(value) => print!("{}", value),
                        IValue::RuneLiteral(value) => print!("{}", value),
                        IValue::StringLiteral(value) => print!("{}", value),
                        IValue::Function { .. } => print!("{}", "function"),
                        IValue::NativeFunction { .. } => print!("{}", "native function"),
                        IValue::Closure { .. } => print!("{}", "closure"),
                    };
                }
                println!("");
                Ok(IValue::ULiteral)
            })),
            ("nativeRead".into(), NativeFun(&|args| {
                if args.len() != 0 {
                    Err(format!("The read function requires no arguments"))
                } else {
                    let mut buf = [0u8; 1];
                    match std::io::stdin().read_exact(&mut buf) {
                        Ok(_) => { Ok(IValue::I32Literal(buf[0] as i32)) },
                        Err(_) => Err(format!("Couldn't read from standard input"))
                    }
                }
            })),
            ("nativeWrite".into(), NativeFun(&|args| {
                if args.len() != 1 {
                    Err(format!("The write function requires just one integer argument [0]"))
                } else {
                    match args[0] {
                        IValue::I32Literal(value) if value >= 0 && value < 256 => {
                            let buf = [value as u8; 1];
                            match std::io::stdout().write(&buf) {
                                Ok(_) => { std::io::stdout().flush().unwrap(); Ok(IValue::ULiteral) },
                                Err(_) => Err(format!("Couldn't write to standard output"))
                            }
                        },
                        IValue::I32Literal(_) => Err(format!("nativeWrite: integer out of range")),
                        _ => Err(format!("The write function requires just one integer argument [2]"))
                    }
                }
            })),
            ("nativeFlushStdin".into(), NativeFun(&|args| {
                if args.len() != 0 {
                    Err(format!("The stdin flush function requires no arguments"))
                } else {
                    let mut buf = String::new();
                    std::io::stdin().read_line(&mut buf).unwrap();
                    Ok(IValue::ULiteral)
                }
            })),
        ]
    }

    pub fn default_handler() -> IHandlerDeclaration {
        IHandlerDeclaration{
            name: "$default_handler".into(),
            return_handler: None,
            effect_handlers: Vec::new()
        }
    }
}