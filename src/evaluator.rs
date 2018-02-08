use types::*;
use errors::*;
use std::collections::HashMap;
use std::collections::HashSet;

pub fn eval(e: &Expression, scope: Scope, context: &Context) -> Result<EvaluatedExpression,Error> {

    match e.expr {

        // String eg "hello"
        Expr::Str(ref s) => Ok(EvaluatedExpression::with_position(e.start, e.end, context.path.clone(), EvaluatedExpr::Str(s.clone()))),

        // boolean eg true or false
        Expr::Bool(b) => Ok(EvaluatedExpression::with_position(e.start, e.end, context.path.clone(), EvaluatedExpr::Bool(b))),

        // unit eg 12px, 100%, 30
        Expr::Unit(n, ref unit) => Ok(EvaluatedExpression::with_position(e.start, e.end, context.path.clone(), EvaluatedExpr::Unit(n,unit.clone()))),

        // colour
        Expr::Colour(ref col) => Ok(EvaluatedExpression::with_position(e.start, e.end, context.path.clone(), EvaluatedExpr::Colour(col.clone()))),

        // undefined
        Expr::Undefined => Ok(EvaluatedExpression::with_position(e.start, e.end, context.path.clone(), EvaluatedExpr::Undefined)),

        // Variables: replace these with the Expresssion on scope that the
        // variable points to. Assume anything on scope is already simplified
        // as much as needed (this is important for Funcs, which use a scope
        // of vars to avoid replacing the func arg uses with other expressions)
        Expr::Var(ref name, ty) => {

            scope.find(name, ty).map_or(
                Err(err(ApplicationError::CantFindVariable(name.clone(), ty), At::position(&context.path, e.start,e.end))),
                |var| { Ok(var.clone()) }
            )

        },

        // If simplifies based on the boolean-ness of the condition!
        Expr::If{ ref cond, then: ref then_e, otherwise: ref else_e } => {

            let cond = eval(cond, scope.clone(), context)?;

            use prelude::casting::raw_boolean;
            let is_true = match raw_boolean(&cond.expr){
                Ok(b) => Ok(b),
                Err(e) => Err(err(e, At::position(&context.path, cond.start, cond.end)))
            }?;

            if is_true {
                eval(then_e, scope, context)
            } else {
                eval(else_e, scope, context)
            }
        },

        // Func declarations: we want to store the scope that they are seen in
        // against the function so that it can be applied against the right things.
        // otherwise, leave as is.
        Expr::Func{ ref inputs, ref output } => {

            Ok(EvaluatedExpression::with_position(
                e.start,
                e.end,
                context.path.clone(),
                EvaluatedExpr::Func{
                    inputs: inputs.clone(),
                    output: output.clone(),
                    scope: scope.clone()
                }
            ))

        },

        // Access in the form of property access like $a.hello, or function application like $a(2,4)
        // access can be chained.
        Expr::Accessed{ ref expression, ref access } => {

            let mut curr: EvaluatedExpression = eval(expression, scope.clone(), context)?;

            for arg in access {
                match *arg {

                    Accessor::Property{ ref name, ref location } => {

                        if let EvaluatedExpr::Block(ref block) = curr.clone().expr {
                            match block.scope.get(name) {
                                Some(val) => {
                                    curr = val.clone();
                                },
                                None => {
                                    curr = EvaluatedExpression::with_position(location.start(), location.end(), context.path.clone(), EvaluatedExpr::Undefined);
                                }
                            }
                        } else {
                            return Err(err(ApplicationError::PropertyDoesNotExist(name.to_owned()), At::position(&context.path, location.start(), location.end())));
                        };

                    },
                    Accessor::Function{ ref args, ref location } => {

                        let mut simplified_args = Vec::with_capacity(args.len());
                        for arg in args.into_iter() {
                            let simplified_arg = eval(arg, scope.clone(), context)?;
                            simplified_args.push(simplified_arg);
                        }

                        match curr.clone().expr {
                            EvaluatedExpr::Func{ inputs: ref arg_names, output: ref func_e, ref scope } => {

                                // if too few args provided, set rest to undefined:
                                while arg_names.len() > simplified_args.len() {
                                    simplified_args.push( EvaluatedExpression::with_position(location.start(), location.end(), context.path.clone(), EvaluatedExpr::Undefined) );
                                }

                                // complain if too many args are provided:
                                if arg_names.len() != simplified_args.len() {
                                    return Err(err(ApplicationError::WrongNumberOfArguments{
                                        expected: arg_names.len(),
                                        got: simplified_args.len()
                                    }, At::position(&context.path, e.start, e.end)));
                                }

                                // create scope containing simplified args to make use of in function body expr:
                                let mut function_scope = HashMap::new();
                                for (name, arg) in arg_names.into_iter().zip(simplified_args) {
                                    function_scope.insert(name.to_owned(),arg);
                                }

                                // update our current expr to be the evaluated result:
                                curr = eval(func_e, scope.push(function_scope), context).map_err(|e| {
                                    err(ApplicationError::FunctionError(e), At::location(&context.path, location.clone()))
                                })?;

                            },
                            EvaluatedExpr::PrimFunc(ref func) => {

                                // primitive func? just run it on the args then!
                                curr = match func.0(&simplified_args, context) {
                                    Ok(res) => Ok(EvaluatedExpression::with_position(e.start,e.end,context.path.clone(),res)),
                                    Err(error) => Err(err(error, At::position(&context.path, e.start, e.end)))
                                }?;

                            }
                            _ => {
                                return Err(err(ApplicationError::NotAFunction, At::position(&context.path, e.start, e.end)));
                            }
                        }
                    }

                }
            }

            Ok(curr)

        },

        // For Blocks, we do our best to simplify the block contents, complaining
        // if there is something invalid somewhere.
        Expr::Block(ref block) => {

            let block_scope = simplify_block_scope(&block.scope, &scope, context)?;
            let new_scope = scope.push(block_scope.clone());

            let css = try_eval_cssentries(&block.css, &new_scope, context)?;
            let selector = try_cssbits_to_string(&block.selector, &new_scope, context)?;
            let mut ty = BlockType::Generic;

            // trim unnecessary spacing and newlines from within selector:
            let mut selector = selector
                .split(|c| c == ' ' || c == '\n')
                .filter(|s| s.len() > 0)
                .collect::<Vec<_>>().join(" ");

            let media_str = "@media ";
            let keyframes_str = "@keyframes ";
            let fontface_str = "@font-face";

            if selector.starts_with(media_str) { ty = BlockType::Media; selector = selector.replacen(media_str,"",1); }
            else if selector.starts_with(keyframes_str) { ty = BlockType::Keyframes; selector = selector.replacen(keyframes_str,"",1); }
            else if selector.starts_with(fontface_str) { ty = BlockType::FontFace; selector = selector.replacen(fontface_str,"",1); }

            Ok(EvaluatedExpression::with_position(
                e.start,
                e.end,
                context.path.clone(),
                EvaluatedExpr::Block(EvaluatedBlock{
                    ty: ty,
                    at: At::position(&context.path, e.start, e.end),
                    scope: block_scope,
                    selector: selector,
                    css: css
                })
            ))

        },


    }

}

// Scan through an expression, searching for variables provided in `search`, and adding any found
// to `out`. Anything that introduces variables (function declaration and blocks) removes those from
// search (since they shadow the names we actually care about finding).
fn dependencies(e: &Expression, search: &HashSet<String>) -> HashSet<String> {

    fn get_dependencies_of(e: &Expression, search: &HashSet<String>, out: &mut HashSet<String>) {
        match e.expr {
            Expr::Str(..) => {},
            Expr::Bool(..) => {},
            Expr::Unit(..) => {},
            Expr::Colour(..) => {},
            Expr::Undefined => {},
            Expr::If{ ref cond, ref then, ref otherwise } => {
                get_dependencies_of(cond, search, out);
                get_dependencies_of(then, search, out);
                get_dependencies_of(otherwise, search, out);
            },
            Expr::Func{ ref inputs, ref output, .. } => {
                let mut search = search.clone();
                for i in inputs { search.remove(i); }
                get_dependencies_of(output, &search, out);
            },
            Expr::Var(ref name, ty) => {
                if ty != VarType::Builtin && search.contains(name) {
                    out.insert(name.clone());
                }
            },
            Expr::Accessed{ ref expression, ref access } => {
                for accessor in access {
                    if let Accessor::Function{ref args, ..} = *accessor {
                        for arg in args {
                            get_dependencies_of(arg, search, out);
                        }
                    }
                }
                get_dependencies_of(expression, search, out);
            },
            Expr::Block(ref block) => {
                let search = get_dependencies_of_scope(&block.scope, &search.clone(), out);
                get_dependencies_of_cssbits(&block.selector, &search, out);
                get_dependencies_of_cssentries(&block.css, &search, out);
            }
        };
    }
    fn get_dependencies_of_scope(scope: &HashMap<String,Expression>, search: &HashSet<String>, out: &mut HashSet<String>) -> HashSet<String> {
        let mut search = search.clone();
        for k in scope.keys() {
            search.remove(k);
        }
        for e in scope.values() {
            get_dependencies_of(e, &search, out);
        }
        search
    }
    fn get_dependencies_of_cssbits(bits: &Vec<CSSBit>, search: &HashSet<String>, out: &mut HashSet<String>) {
        for bit in bits {
            if let &CSSBit::Expr(ref e) = bit {
                get_dependencies_of(e, &search, out);
            }
        }
    }
    fn get_dependencies_of_cssentries(entries: &Vec<CSSEntry>, search: &HashSet<String>, out: &mut HashSet<String>) {
        for entry in entries {
            match *entry {
                CSSEntry::Expr(ref e) => {
                    get_dependencies_of(e, search, out);
                },
                CSSEntry::KeyVal{ ref key, ref val } => {
                    get_dependencies_of_cssbits(key, search, out);
                    get_dependencies_of_cssbits(val, search, out);
                }
            }
        }
    }

    let mut out = HashSet::new();
    get_dependencies_of(e, search, &mut out);
    out

}

// Given a map of dependencies (var name -> Expression + var deps), return a map of evaluated expressions,
// or if a cycle is detected which prevents proper evaluation, an error.
type Dependencies<'a> = HashMap<String,(&'a Expression,HashSet<String>)>;
fn simplify_dependencies(deps: &Dependencies, scope: &Scope, context: &Context) -> Result<HashMap<String,EvaluatedExpression>,Error> {

    fn do_simplify(key: &String, last: &Vec<String>, deps: &Dependencies, scope: &Scope, context: &Context, out: &mut HashMap<String,EvaluatedExpression>) -> Result<(),Error> {

        if out.contains_key(key) { return Ok(()); }

        let &(expr,ref expr_deps) = deps.get(key).expect("Trying to simplify an expression but can't find it on scope");

        if last.iter().any(|k| k == key) {
            return Err(err(ApplicationError::CycleDetected(last.clone(), key.clone()), At::position(&context.path,expr.start,expr.end)));
        }

        if expr_deps.len() > 0 {
            let mut new_last = last.clone();
            new_last.push(key.clone());
            for dep in expr_deps {
                do_simplify(dep, &new_last, deps, scope, context, out)?;
            }
        }

        let new_scope = scope.push(out.clone());
        let new_expr = eval(expr, new_scope, context)?;
        out.insert(key.clone(), new_expr);
        Ok(())

    };

    let mut out = HashMap::new();
    let last = Vec::new();
    for (key,_) in deps {
        do_simplify(key, &last, deps, scope, context, &mut out)?;
    }
    Ok(out)

}

fn simplify_block_scope(block_scope: &HashMap<String,Expression>, scope: &Scope, context: &Context) -> Result<HashMap<String,EvaluatedExpression>,Error> {

    // work out what each variable on scope depends on, so that we know which order to simplify them in in order
    // to ensure that each variable has in its scope a simplified version of everything it depends on.
    let vars: HashSet<String> = block_scope.keys().cloned().collect();
    let deps: Dependencies = block_scope.iter().map(|(k,v)| (k.clone(),(v,dependencies(v, &vars)))).collect();

    simplify_dependencies(&deps, scope, context)
}
fn try_cssbits_to_string(bits: &Vec<CSSBit>, scope: &Scope, context: &Context) -> Result<String,Error> {
    let mut string = vec![];
    for bit in bits {
        match *bit {
            CSSBit::Str(ref s) => string.push(s.to_owned()),
            CSSBit::Expr(ref e) => {
                let e = eval(e, scope.clone(),context)?;
                use prelude::casting::raw_string;
                let s = match raw_string(&e.expr) {
                    Ok(s) => Ok(s),
                    Err(error) => Err(err(error, At::position(&context.path, e.start, e.end)))
                }?;
                string.push(s);
            }
        }
    }

    // trim unnecessary spacing and newlines from CSS value:
    let string = string
        .concat()
        .trim()
        .split(|c| c == ' ' || c == '\n')
        .filter(|s| s.len() > 0)
        .collect::<Vec<_>>().join(" ");

    Ok(string)
}
fn try_eval_cssentries(entries: &Vec<CSSEntry>, scope: &Scope, context: &Context) -> Result<Vec<EvaluatedCSSEntry>,Error> {
    let mut out = vec![];
    for val in entries {
        match *val {
            CSSEntry::Expr(ref expr) => {
                let css_expr = eval(expr, scope.clone(),context)?;
                match css_expr.expr {
                    EvaluatedExpr::Block(ref block) => out.push(EvaluatedCSSEntry::Block(block.clone())),
                    _ => return Err(err(ShapeError::NotACSSBlock(css_expr.expr.kind()), At::position(&context.path,css_expr.start,css_expr.end)))
                };
            },
            CSSEntry::KeyVal{ref key, ref val} => {
                let key = try_cssbits_to_string(key, scope, context)?;
                let val = try_cssbits_to_string(val, scope, context)?;
                out.push(EvaluatedCSSEntry::KeyVal{
                    key: key,
                    val: val
                });
            }
        }
    }
    Ok(out)
}


#[cfg(test)]
pub mod tests {

    use super::*;

}