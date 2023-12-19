pub mod ast;
pub mod interpreter;
pub mod parser;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 {
        match execute_code(&args[1]) {
            Ok(msg) => println!("{}", msg),
            Err(err) => println!("{}", err),
        }
    } else {
        println!("No file supplied");
    }
}

fn execute_code(file: &str) -> Result<String, String> {
    std::fs::read_to_string(file)
        .map_err(|err| format!("{:#?}", err))
        .and_then(|code| {
            let parsed = parser::parse_code(&code)?;
            let interpreter = interpreter::Declarations::new(parsed, interpreter::Declarations::standard_library());
            interpreter.run()
        })
}
