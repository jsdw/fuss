use types::{Expression,InputPosition,Position};
use std::collections::HashMap;
use list::List;
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

struct Scope(List<HashMap<String,Expression>>);
impl Scope {

    /// create a new, empty scope:
    fn new() -> Self {
        Scope(List::new().push(HashMap::new()))
    }

    /// lookup a value in the scope:
    fn find<'a>(&'a self, name: &str) -> Option<&'a Expression> {
        for map in self.0.iter() {
            if let Some(expr) = map.get(name) {
                return Some(expr)
            }
        }
        None
    }

    /// push some new values onto the scope, returning a new one and
    /// leaving the original unchanged:
    fn push(&self, values: HashMap<String,Expression>) -> Scope {
        Scope(self.0.push(values))
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

fn simplify(expr: Expression, scope: Scope) -> Res {



    unimplemented!();
}

#[cfg(test)]
pub mod tests {

    use super::*;

}