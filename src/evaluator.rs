use types::{Expression,InputPosition,Position};
use std::collections::HashMap;
use list::List;
use std::borrow::Cow;
use chomp;
use parser;

pub type Res = Result<Expression,Error>;
pub struct Error {
    ty: ErrorType,
    file: String,
    start: Position,
    end: Position
}
pub enum ErrorType {
    ParseError(parser::Error),
    CantFindVariable(String),
    NotAFunction,
    WrongNumberOfArguments{expected: usize, got: usize}
}

#[derive(Clone,Debug)]
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
        Ok(expr) => simplify(expr, Scope::new()).map_err(|mut e| {
            e.file = name.to_owned();
            e
        }),
        Err(err) => Err(Error{
            ty: ErrorType::ParseError(err),
            file: name.to_owned(),
            start: rest.position(),
            end: rest.position()
        })
    }

}

fn simplify(e: Expression, scope: Scope) -> Res {

    use types::Expr::*;
    use types::Primitive::*;
    use self::ErrorType::*;

    match e.expr {

        /// Func declarations can't simplify, so leave as is
        Func{..} => Ok(e),

        /// We don't need to simplify primitives; they don't get any simpler!
        Prim(_) => Ok(e),

        /// Variables, if found in scope, are already simplified, so just complain
        /// if they don't exist.
        Var(ref name) => scope.find(name).map_or(
            Err(e.err(CantFindVariable(name.to_owned()))),
            |var| Ok(var.clone())
        ),

        /// If simplifies based on the boolean-ness of the condition!
        If{ cond: boxed_cond, then: boxed_then, otherwise: boxed_else } => {

            let cond = simplify(*boxed_cond, scope.clone())?;

            let is_true = match cond.expr {
                Prim(Str(s)) => s.len() > 0,
                Prim(Bool(b)) => b,
                Prim(Unit(n,_)) => n != ::std::f64::NAN && n != 0f64,
                Func{..} => true,
                _ => false
            };

            if is_true {
                simplify(*boxed_then, scope)
            } else {
                simplify(*boxed_else, scope)
            }
        },

        /// Function applications lead to a new scope being created to stick the
        /// function inputs into, and then simplifying the body against that.
        App{ expr: ref boxed_expr, args: ref args } => {

            let func = simplify(*boxed_expr.clone(), scope.clone())?;

            let (arg_names, func_e) = match func.expr {
                Func{ inputs: arg_names, output: func_e} => Ok( (arg_names, func_e) ),
                _ => Err(func.err(NotAFunction))
            }?;

            if arg_names.len() != args.len() {
                return Err(e.err(WrongNumberOfArguments{
                    expected: arg_names.len(),
                    got: args.len()
                }));
            }

            let mut function_scope = HashMap::new();
            for (name, expr) in arg_names.into_iter().zip(args) {
                function_scope.insert(name, expr.clone());
            }

            simplify(*func_e.clone(), scope.push(function_scope))

        }

        /// For Blocks, we simplify the CSSEntry Expressions in the context of
        /// the block scope, and complain if they do not themselves resolve to blocks.
        /// We make use of a NestedSimpleBlock type to ensure that we have valid output.
        Block(block) => {
            unimplemented!()
        }

    }

}

impl Expression {
    fn err(&self, e:ErrorType) -> Error {
        Error{
            ty: e,
            file: String::new(),
            start: self.start,
            end: self.end
        }
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;

}