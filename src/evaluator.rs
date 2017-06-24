use types::{Expression,Expr,InputPosition,Position,CSSEntry,NestedCSSEntry,NestedSimpleBlock};
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
    WrongNumberOfArguments{expected: usize, got: usize},
    NotACSSBlock
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

/// generate an Err from an Expression and an ErrorType. This is a macro
/// so that it can pluck out only what it needs from the struct, rather than
/// trying to move the whole thing
macro_rules! err {
    ($e:ident, $err:expr) => ({
        let start = $e.start;
        let end = $e.end;
        Err(Error{
            ty: $err,
            file: String::new(),
            start: start,
            end: end
        })
    })
}

fn simplify(e: Expression, scope: Scope) -> Res {

    use types::Primitive::*;
    use self::ErrorType::*;

    match e.expr {

        /// Func declarations: we need to substitute any variables declared within
        /// (with the exception of those coming in through the args) with expressions
        /// on scope at present.
        Expr::Func{ inputs, output } => {

            use std::collections::hash_set::{HashSet};

            // make a scope wherein the function args are preserved as variables,
            // so that simplifyin only replaces other stuff.
            let mut inputVars = HashMap::new();
            for arg in inputs.iter() {
                let var = Expression{
                    start: e.start,
                    end: e.end,
                    expr: Expr::Var(arg.clone())
                };
                inputVars.insert(arg.clone(), var);
            }

            let simplified_output = simplify(*output, scope.push(inputVars))?;

            Ok(Expression{
                start: e.start,
                end: e.end,
                expr: Expr::Func{
                    inputs: inputs,
                    output: Box::new(simplified_output)
                }
            })

        },

        /// We don't need to simplify primitives; they don't get any simpler!
        Expr::Prim(_) => Ok(e),

        /// This is our target output, so we can't simplify it further
        Expr::NestedSimpleBlock(_) => Ok(e),

        /// Variables: replace these with the Expresssion on scope that the
        /// variable points to. Assume anything on scope is already simplified
        /// as much as needed (this is important for Funcs, which use a scope
        /// of vars to avoid replacing the func arg uses with other expressions)
        Expr::Var(name) => scope.find(&name).map_or(
            err!(e,CantFindVariable(name)),
            |var| Ok(var.clone())
        ),

        /// If simplifies based on the boolean-ness of the condition!
        Expr::If{ cond: boxed_cond, then: boxed_then, otherwise: boxed_else } => {

            let cond = simplify(*boxed_cond, scope.clone())?;

            let is_true = match cond.expr {
                Expr::Prim(Str(s)) => s.len() > 0,
                Expr::Prim(Bool(b)) => b,
                Expr::Prim(Unit(n,_)) => n != ::std::f64::NAN && n != 0f64,
                Expr::Func{..} => true,
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
        Expr::App{ expr: boxed_expr, args } => {

            let func = simplify(*boxed_expr, scope.clone())?;

            let (arg_names, func_e) = match func.expr {
                Expr::Func{ inputs: arg_names, output: func_e} => Ok( (arg_names, func_e) ),
                _ => err!(func, NotAFunction)
            }?;

            if arg_names.len() != args.len() {
                return err!(e,WrongNumberOfArguments{
                    expected: arg_names.len(),
                    got: args.len()
                });
            }

            let mut function_scope = HashMap::new();
            for (name, expr) in arg_names.into_iter().zip(args) {
                function_scope.insert(name, expr);
            }

            simplify(*func_e, scope.push(function_scope))

        },

        /// For Blocks, we simplify the CSSEntry Expressions in the context of
        /// the block scope, and complain if they do not themselves resolve to blocks.
        /// We make use of a NestedSimpleBlock type to ensure that we have valid output.
        Expr::Block(block) => {

            let new_scope = scope.push(block.scope);
            let mut blocks = vec![];
            for val in block.css {
                match val {
                    CSSEntry::Expr(expr) => {
                        let css_expr = simplify(expr, new_scope.clone())?;
                        let block = match css_expr.expr {
                            Expr::NestedSimpleBlock(block) => block,
                            _ => return err!(css_expr, NotACSSBlock)
                        };
                        blocks.push( NestedCSSEntry::Block(Box::new(block)) );
                    },
                    CSSEntry::KeyVal{ key, val } => {
                        blocks.push( NestedCSSEntry::KeyVal{ key:key, val:val } );
                    }
                }
            }

            Ok(Expression{
                start: e.start,
                end: e.end,
                expr: Expr::NestedSimpleBlock(NestedSimpleBlock{
                    selector: block.selector,
                    css: blocks
                })
            })

        },


    }

}

#[cfg(test)]
pub mod tests {

    use super::*;

}