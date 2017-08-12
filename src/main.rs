#![recursion_limit = "1024"]

// #[macro_use] extern crate chomp;
//#[macro_use] extern crate lazy_static;
#[macro_use] extern crate pest;
#[macro_use] extern crate clap;

mod macros;
mod parser;
mod types;
mod evaluator;
mod list;
mod outputter;
mod prelude;

use clap::App;
// this imports and evaluates a FUSS file:
use prelude::import::import_root;
use std::convert::From;
use std::path::PathBuf;
use types::*;
use std::thread;

fn main() {
    run();
}

fn run() {

    let child = thread::Builder::new().stack_size(64 * 1024 * 1024).spawn(move || {

        /// parse args from CLI based on our cli.yml,
        /// and get matched commands:
        let yaml = load_yaml!("cli.yml");
        let matches = App::from_yaml(yaml).get_matches();

        // import the FUSS file, compiling and evaluating it.
        let input = matches.value_of("input").unwrap();
        let res = import_root( &PathBuf::from(input) );

        match res {
            Err(e) => {
                eprintln!("Error: {:?}", e);
            },
            Ok(Expr::EvaluatedBlock(block)) => {
                outputter::print_css(block);
            },
            Ok(_) => {
                eprintln!("Fuss file needs to evaluate to a css block");
            }
        }

    }).unwrap();

    child.join().unwrap()

}






