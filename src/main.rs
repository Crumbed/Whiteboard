

mod lexer;
mod runtime;
mod parser;
mod interpreter;


//pub trait Scope {
//    fn declare_var(&mut self, var_id: &str, var_data: &RuntimeValue);
//    fn assign_var(&mut self, var_id: &str, var_data: &RuntimeValue);
//    fn get_var(&mut self, var_id: &str) -> RuntimeValue;
//    fn contains_var(&self, var_id: &str) -> bool;
//    fn get_type(&self) -> ScopeType;
//}

use std::{io::{self, Write }, process};

use crate::parser::ProgramAST;







fn main() -> io::Result<()>{
    let mut input = String::new();
    let args: Vec<String> = std::env::args().collect();



    // If the user provides a file to run
    if args.len() > 1 {
        let file_path = args.get(1).unwrap();
        //let dir = std::fs::read_dir(file_path)?;
        //let name = dir.last().unwrap();
        //println!("{:?}", name);
        let src = std::fs::read_to_string(file_path)
            .expect("Unable to read file");
        
        return Ok(());
    }

    println!("Repl v0.1");
    loop {
        print!("> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut input)?;
        if input == ".exit\n" { break; }

        let tokens = lexer::tokenize(&input);
        //println!("{:#?}", tokens);
        let ast = ProgramAST::parse(tokens);
        println!("{:#?}", ast.nodes);

        input.clear();
    }

    Ok(())
}




pub fn error(msg: &str) {
    eprintln!("Error: {msg}");
    process::exit(0);
}
















