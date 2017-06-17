use types::{Expression,InputPosition,Position};
use std::collections::HashMap;
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

struct Scope(Vec<HashMap<String,Expression>>);
impl Scope {
    fn new() -> Self {
        Scope(vec![HashMap::new()])
    }
    fn find<'a>(&'a self, name: &str) -> Option<&'a Expression> {
        for map in self.0.iter().rev() {
            if let Some(expr) = map.get(name) {
                return Some(expr)
            }
        }
        None
    }
}

fn eval_str(text: &str, name: &str) -> Res {

    let pos = Position::new();
    let input = InputPosition::new(text, pos);
    let (rest, res) = chomp::run_parser(input, parser::parse);

    match res {
        Ok(expr) => simplify(expr, Scope::new()),
        Err(err) => Err(Error{
            ty: ErrorType::ParseError(err),
            file: name.to_owned(),
            position: rest.position()
        })
    }

}

fn simplify(expr: Expression, scopes: Scope) -> Res {



    unimplemented!();
}

#[cfg(test)]
pub mod tests {

    use super::*;

}