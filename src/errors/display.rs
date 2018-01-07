use errors::errors::*;

pub fn display_error(e: Error) -> String {
    format!("{}", e)
}