pub mod ast;
pub mod interpreter;
pub mod parser;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 {
        match execute_code_interactive(&args[1]) {
            Ok(_) => (),
            Err(err) => println!("{}", err),
        }
    } else {
        println!("No file supplied");
    }
}

fn parse_code(file: &str) -> Result<interpreter::Interpreter, String> {
    std::fs::read_to_string(file)
        .map_err(|err| format!("{:#?}", err))
        .and_then(|code| {
            let parsed = parser::parse_code(&code)?;
            let declarations = interpreter::Declarations::new(parsed, interpreter::Declarations::standard_library());
            Ok(interpreter::Interpreter::new(declarations))
        })
}

fn execute_code(file: &str) -> Result<String, String> {
    let mut interpreter = parse_code(file)?;

    interpreter.restart()?;
    interpreter.run_to_completition()
}

fn execute_code_interactive(file: &str) -> Result<(), String> {
    let mut read_buffer = String::new();
    let stdin = std::io::stdin();
    let mut interpreter = parse_code(file)?;

    interpreter.restart()?;
    loop {
        while interpreter.has_next() {
            read_line(&stdin, &mut read_buffer)?;
            if read_buffer == "next" {
                interpreter.next()?;
            } else if read_buffer == "restart" {
                interpreter.restart()?;
            }
        }
    
        println!("{}", interpreter.run_to_completition()?);
        println!("terminate? [no]");
        read_line(&stdin, &mut read_buffer)?;
        if read_buffer != "no" {
            return Ok(());
        }
    }
}

fn read_line(stdin: &std::io::Stdin, buffer: &mut String) -> Result<(), String> {
    println!("> ");
    buffer.clear();
    stdin.read_line(buffer)
        .map(|_| ())
        .map_err(|err| err.to_string())
}
