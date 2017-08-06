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
use std::io::{self, Write};
use std::thread;

fn main() {
    match run() {
        Err(e) => {
            eprintln!("Error: {:?}", e);
        },
        Ok(_) => {
            // all done!
        }
    }
}

fn run() -> io::Result<()> {

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
                Ok(())
            },
            Ok(Expr::Block(block)) => {
                let css = outputter::to_css(&block);
                let stdout = io::stdout();
                let mut handle = stdout.lock();

                // these could error:
                handle.write_all(css.as_bytes())?;
                handle.flush()
            },
            Ok(_) => {
                eprintln!("Fuss file needs to evaluate to a css block");
                Ok(())
            }
        }

    }).unwrap();

    child.join().unwrap()

}






