use std::{fs::File, io::Read};

pub mod datalog;
pub mod engine;
pub mod parser;

use engine::{evaluate_program, format_query_result, Database, QueryEngine};

const TITLE: &str = "Datalog";

fn run_program(stmts: Vec<datalog::Statement>) {
    println!("{TITLE}\n");

    // Separate assertions and queries
    let mut db = Database::new();
    let mut queries = Vec::new();

    for stmt in stmts {
        match stmt {
            datalog::Statement::Assertion(clause) => {
                println!("  {clause}.");
                db.add_clause(&clause);
            }
            datalog::Statement::Query(lit) => {
                queries.push(lit);
            }
        }
    }

    // Evaluate the program (derive all facts)
    if db.rules().is_empty() && db.fact_count() == 0 {
        println!("No facts or rules loaded.\n");
    } else {
        println!("\nEvaluating...");
        match evaluate_program(&mut db) {
            Ok(result) => {
                println!(
                    "Derived {} new facts in {} iteration(s).",
                    result.facts_derived, result.iterations
                );
                println!("Total facts: {}\n", db.fact_count());
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
    }

    // Answer queries
    if !queries.is_empty() {
        let query_engine = QueryEngine::new(&db);

        for query in queries {
            println!("?- {}", query);
            let result = query_engine.query(&query);
            println!("{}\n", format_query_result(&query, &result));
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let statements = match args.len() {
        1 => vec![],
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
    run_program(statements);
}
