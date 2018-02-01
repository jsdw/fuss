use errors::errors::*;

pub fn display_error(e: ImportError) -> String {
    format!("{}", e)
}