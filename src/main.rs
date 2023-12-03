pub mod ast;
pub mod interpreter;
pub mod parser;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 {
        match std::fs::read_to_string(&args[1]) {
            Ok(code) => println!("{:#?}", parser::parse_code(&code)),
            Err(err) => println!("{:#?}", err),
        }
    } else {
        println!("No file supplied");
    }
}
