use types::{Expression,InputPosition,Position};
use chomp;
use parser;

pub type Res = Result<Expression,Error>;
pub struct Error {
    ty: ErrorType,
    file: String,
    position: Position
}
pub enum ErrorType {
    ParseError(parser::Error)
}

fn eval_str(text: &str, name: &str) -> Res {

    let pos = Position::new();
    let input = InputPosition::new(text, pos);
    let (rest, res) = chomp::run_parser(input, parser::parse);

    match res {
        Ok(expr) => simplify(expr),
        Err(err) => Err(Error{
            ty: ErrorType::ParseError(err),
            file: name.to_owned(),
            position: rest.position()
        })
    }

}

fn simplify(expr: Expression) -> Res {
    unimplemented!();
}

#[cfg(test)]
pub mod tests {

    use super::*;

}