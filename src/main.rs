pub mod ast;
pub mod interpreter;
pub mod parser;

use std::{env, io::Write};

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
            let declarations = interpreter::Declarations::new(parsed);
            Ok(interpreter::Interpreter::new(declarations)?)
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

    loop {
        while interpreter.has_next() {
            read_line(&stdin, &mut read_buffer)?;
            if read_buffer.contains("next") {
                interpreter.next()?;
                println!("{}", interpreter.print_state());
            } else if read_buffer.contains("restart") {
                interpreter.restart()?;
            } else if read_buffer.contains("exit") {
                return Ok(());
            } else if read_buffer.contains("current") {
                println!("{}", interpreter.print_state());
            } else if read_buffer.contains("continue") {
                break;
            }
        }
    
        println!("{}", interpreter.run_to_completition()?);
        println!("terminate? [no]");
        read_line(&stdin, &mut read_buffer)?;
        if !read_buffer.contains("no") {
            return Ok(());
        }

        interpreter.restart()?;
    }
}

fn read_line(stdin: &std::io::Stdin, buffer: &mut String) -> Result<(), String> {
    print!("> ");
    std::io::stdout().flush().unwrap();
    buffer.clear();
    stdin.read_line(buffer)
        .map(|_| ())
        .map_err(|err| err.to_string())
}
