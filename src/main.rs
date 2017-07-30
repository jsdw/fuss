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

fn main() {

    /// parse args from CLI based on our cli.yml,
    /// and get matched commands:
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();

    let input = matches.value_of("input").unwrap();
    let output = matches.value_of("output").unwrap();

    // import the FUSS file, compiling and evaluating it.
    let res = import_root( &PathBuf::from(input) );

    match res {
        Err(e) => {
            println!("AAH! {:?}", e);
        },
        Ok(Expr::NestedSimpleBlock(block)) => {
            let css = outputter::to_css(&block);
            println!("block: {:?}", block);
            println!("OK! {}", css);
        },
        Ok(_) => {
            println!("Fuss file needs to evaluate to a css block")
        }
    }

}






