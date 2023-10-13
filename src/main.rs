use std::{fs::File, io::Read};

pub mod datalog;
pub mod parser;

const TITLE: &str = "Datalog";
const PROMPT: &str = ">";

fn run_repl(stmts: Vec<datalog::Statement>) {
    println!("{TITLE}\n");
    if !stmts.is_empty() {
        println!("info{PROMPT} Loaded Statements:");
        for stmt in stmts.iter() {
            println!("defn{PROMPT} {stmt}");
        }
        println!("\n{PROMPT}");
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let statements = match args.len() {
        1 => {
            vec![]
        }
        2 => {
            let path = &args[1];
            match File::open(path) {
                Ok(mut file) => {
                    let mut contents = String::new();
                    if file.read_to_string(&mut contents).is_ok() {
                        let maybe_statements = parser::peg_parser::program(&contents);
                        match maybe_statements {
                            Ok(stmts) => stmts,
                            Err(e) => {
                                eprintln!("Failed to parse file: {path}!\nError:\n{e}");
                                std::process::exit(1);
                            }
                        }
                    } else {
                        eprintln!("Failed to read file: {path}!");
                        std::process::exit(1);
                    }
                }
                Err(e) => {
                    eprintln!("Failed to open file: {path}!\nReason: {e}");
                    std::process::exit(1);
                }
            }
        }
        _ => {
            println!("Usage: {} [<PATH>]", args[0]);
            std::process::exit(1);
        }
    };
    run_repl(statements);
}
