use types::*;
use types::ErrorType::*;
use std::collections::HashMap;
use list::List;
use chomp;
use parser;

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

    /// push one key and value onto the scope, returning a new one and
    /// leaving the original unchanged:
    fn push_one(&self, key: String, value: Expression) -> Scope {
        let mut map = HashMap::with_capacity(1);
        map.insert(key, value);
        self.push(map)
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

    use types::Primitive::*;

    match e.expr {

        /// We don't need to simplify primitives; they don't get any simpler!
        Expr::Prim(_) => Ok(e),

        /// primitive functions are essentailly black boxes that we pass exprs into
        /// and get some result out; we can't simplify them.
        Expr::PrimFunc(..) => Ok(e),

        /// This is our target output, so we can't simplify it further
        Expr::NestedSimpleBlock(_) => Ok(e),

        /// Variables: replace these with the Expresssion on scope that the
        /// variable points to. Assume anything on scope is already simplified
        /// as much as needed (this is important for Funcs, which use a scope
        /// of vars to avoid replacing the func arg uses with other expressions)
        Expr::Var(name, rest) => {

            // locate the initial variable on scope somewhere:
            let mut var = scope.find(&name).map_or(
                err!(e,CantFindVariable(name.clone())),
                |var| {
                    // We found a var but it is a RecursiveValue, meaning we are trying
                    // to define a value in terms of itself.
                    if let Expr::Prim(Primitive::RecursiveValue) = var.expr {
                        err!(var, LoopDetected)
                    } else {
                        Ok(var)
                    }
                }
            )?;

            // if asked to, try and dig into the variable, failing
            // as soon as we don't have a scope to dig into any more.
            for key in rest {
                if let Expr::NestedSimpleBlock(ref block) = var.expr {
                    match block.scope.get(&key) {
                        Some(val) => {
                            var = val;
                        },
                        None => {
                            return err!(e, PropertyDoesNotExist(name.clone(), key));
                        }
                    }
                }
            }

            // if we have found something from our digging, copy and return it:
            Ok(var.clone())

        },

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

        /// Func declarations: we need to substitute any variables declared within
        /// (with the exception of those coming in through the args) with expressions
        /// on scope at present.
        Expr::Func{ inputs, output } => {

            // make a scope wherein the function args are preserved as variables,
            // so that simplifyin only replaces other stuff.
            let mut input_vars = HashMap::new();
            for arg in inputs.iter() {
                let var = Expression{
                    start: e.start,
                    end: e.end,
                    expr: Expr::Var(arg.clone(),vec![])
                };
                input_vars.insert(arg.clone(), var);
            }

            let simplified_output = simplify(*output, scope.push(input_vars))?;

            Ok(expression_from!{e,
                Expr::Func{
                    inputs: inputs,
                    output: Box::new(simplified_output)
                }
            })

        },

        /// Function applications lead to a new scope being created to stick the
        /// function inputs into, and then simplifying the body against that.
        Expr::App{ expr: boxed_expr, args } => {

            // simplify the func expr, which might at this point be a variable
            // or something. Then, simplify the args.
            let func = simplify(*boxed_expr, scope.clone())?;
            let mut couldnt_simplify_all = false;
            let mut simplified_args = vec![];
            for arg in args.into_iter() {

                let simplified_arg = simplify(arg, scope.clone())?;

                // simplify didn't fail, but the arg is still a variable,
                // which means we are simplifying inside a Func, and this
                // is a variable that doesn't yet have a known value. this
                // meamns we can't yet simplify further.
                if let Expr::Var(..) = simplified_arg.expr {
                    couldnt_simplify_all = true;
                }
                simplified_args.push(simplified_arg);

            }

            // simplifying not good enough, so reconstruct and return; at least
            // we'll have potentially done some work simplifing in this pass.
            if couldnt_simplify_all {
                return Ok(expression_from!{e,
                    Expr::App{
                        expr: Box::new(func),
                        args: simplified_args
                    }
                });
            }

            // now we've simplified the args and the func, see what type of
            // func we are dealing with so that we can apply the args to it
            // and get back a result.
            match func.expr {
                Expr::Func{ inputs: arg_names, output: func_e} => {

                    if arg_names.len() != simplified_args.len() {
                        return err!(e,WrongNumberOfArguments{
                            expected: arg_names.len(),
                            got: simplified_args.len()
                        });
                    }

                    // create scope containing simplified args to make use of in function body expr:
                    let mut function_scope = HashMap::new();
                    for (name, arg) in arg_names.into_iter().zip(simplified_args) {
                        function_scope.insert(name,arg);
                    }

                    simplify(*func_e, scope.push(function_scope))

                },
                Expr::PrimFunc(func) => {

                    // primitive func? just run it on the args then!
                    match func(simplified_args) {
                        Ok(res) => Ok(expression_from!{e, res}),
                        Err(err) => err!{e, err}
                    }

                }
                _ => err!(func, NotAFunction)
            }

        },

        /// For Blocks, we simplify the CSSEntry Expressions in the context of
        /// the block scope, and complain if they do not themselves resolve to blocks.
        /// We make use of a NestedSimpleBlock type to ensure that we have valid output.
        Expr::Block(block) => {

            // simplify things in the block scope against a scope including the unsimplified
            // versions of themselves. This allows variables to reference other variables defined here.
            //
            // For each variable we try and simplify, we push onto the
            // scope a RecursiveValue marker for that variable, so that if at any point while
            // simplifying the expression bound to the variable, we encounter that same variable,
            // an error is thrown. This prevents self referential expressions.
            //
            let block_scope = scope.push(block.scope.clone());
            let mut simplified_block_scope = HashMap::new();
            for (name,expr) in block.scope {
                let s = block_scope.push_one(name.clone(), expression_from!{ expr, Expr::Prim(RecursiveValue) });
                let simplified_expr = simplify(expr,s)?;
                simplified_block_scope.insert(name, simplified_expr);
            }

            let new_scope = scope.push(simplified_block_scope.clone());
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
                        let mut strings = vec![];
                        for bit in val {
                            match bit {
                                CSSValueBit::Str(s) => strings.push(s),
                                CSSValueBit::Expr(e) => {
                                    let simplified = simplify(e, scope.clone())?;
                                    let s = css_expr_to_string(simplified)?;
                                    strings.push(s);
                                }
                            }
                        }
                        blocks.push( NestedCSSEntry::KeyVal{ key:key, val:strings.concat() } );
                    }
                }
            }

            Ok(expression_from!{e,
                Expr::NestedSimpleBlock(NestedSimpleBlock{
                    scope: simplified_block_scope,
                    selector: block.selector,
                    css: blocks
                })
            })

        },


    }

}

// convert expressions embedded in CSS values to string pieces where possible.
fn css_expr_to_string(e: Expression) -> Result<String,Error> {
    use types::Primitive::*;
    match e.expr {
        Expr::Prim(Str(s)) => Ok(s),
        Expr::Prim(Bool(b)) => Ok(if b { "true".to_owned() } else { "false".to_owned() }),
        Expr::Prim(Unit(num,suffix)) => Ok(format!{"{}{}", format!{"{:.5}",num}.trim_right_matches('0'),suffix}),
        _ => err!(e,InvalidExpressionInCssValue)
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;

}