#![recursion_limit = "1024"]

// #[macro_use] extern crate chomp;
//#[macro_use] extern crate lazy_static;
extern crate pest;
#[macro_use] extern crate pest_derive;
#[macro_use] extern crate clap;

mod macros;
mod parser;
mod types;
mod evaluator;
mod cache;
mod outputter;
mod prelude;
mod errors;

use clap::App;
// this imports and evaluates a FUSS file:
use prelude::import::{import_root,import_string};
use std::convert::From;
use std::path::PathBuf;
use types::*;
use errors::display;
use std::thread;

use std::io::{self, Read};

fn main() {
    run();
}

fn run() {

    let child = thread::Builder::new().stack_size(64 * 1024 * 1024).spawn(move || {

        // parse args from CLI based on our cli.yml,
        // and get matched commands:
        let yaml = load_yaml!("cli.yml");
        let matches = App::from_yaml(yaml).get_matches();
        let maybe_path = matches.value_of("input").map(PathBuf::from);

        let input_string = if maybe_path.is_none() {
            let mut buffer = String::new();
            let stdin = io::stdin();
            let mut handle = stdin.lock();
            handle.read_to_string(&mut buffer).unwrap();
            buffer
        } else {
            String::new()
        };

        // import the FUSS file, compiling and evaluating it.
        // if path provided, use that, else pull from stdin.
        let res = match maybe_path {
            Some(path) => {
                import_root(&path)
            },
            None => {
                import_string(&input_string)
            }
        };

        match res {
            Err(e) => {
                display::display_import_error(e, display::Options::with_stdin(&input_string));
            },
            Ok(EvaluatedExpr::Block(block)) => {
                let warnings = outputter::print_css(block);
                let opts = display::Options::with_stdin(&input_string);
                for warning in warnings {
                    display::display_warning(warning, &opts);
                }
            },
            Ok(e) => {
                eprintln!("Fuss file needs to evaluate to a css block, but instead evaluates to: {}", e.kind());
            }
        }

    }).unwrap();

    child.join().unwrap()

}






