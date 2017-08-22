use types::*;
use types::ErrorType::*;
use std::collections::HashMap;

pub fn eval(e: Expression, scope: Scope, context: &Context) -> Res {

    match e.expr {

        /// We don't need to simplify primitives; they don't get any simpler!
        Expr::Prim(_) => Ok(e),

        /// primitive functions are essentailly black boxes that we pass exprs into
        /// and get some result out; we can't simplify them.
        Expr::PrimFunc(..) => Ok(e),

        /// EvaluatedBlocks have already been evaluated, so no need to do more:
        Expr::EvaluatedBlock(..) => Ok(e),

        /// Variables: replace these with the Expresssion on scope that the
        /// variable points to. Assume anything on scope is already simplified
        /// as much as needed (this is important for Funcs, which use a scope
        /// of vars to avoid replacing the func arg uses with other expressions)
        Expr::Var(ref name, ref rest) => {

            // locate the initial variable on scope somewhere:
            let mut var = scope.find(name).map_or(
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
                if let Expr::EvaluatedBlock(ref b) = var.expr {
                    match b.block.scope().and_then(|s| s.get(key)) {
                        Some(val) => {
                            var = val;
                        },
                        None => {
                            return err!(e, PropertyDoesNotExist(name.clone(), key.to_owned()));
                        }
                    }
                } else {
                    return err!(e, PropertyDoesNotExist(name.clone(), key.to_owned()));
                }
            }

            // if we have found something from our digging, copy and return it:
            Ok(var.clone())

        },

        /// If simplifies based on the boolean-ness of the condition!
        Expr::If{ cond: ref cond, then: ref then_e, otherwise: ref else_e } => {

            let cond = eval(cond.clone(), scope.clone(), context)?;

            use prelude::casting::raw_boolean;
            let is_true = match raw_boolean(&cond.expr){
                Ok(b) => Ok(b),
                Err(err) => err!{cond,err}
            }?;

            if is_true {
                eval(then_e.clone(), scope, context)
            } else {
                eval(else_e.clone(), scope, context)
            }
        },

        /// Func declarations: we want to store the scope that they are seen in
        /// against the function so that it can be applied against the right things.
        /// otherwise, leave as is.
        Expr::Func{ ref inputs, ref output, .. } => {

            Ok(expression_from!{e,
                Expr::Func{
                    inputs: inputs.clone(),
                    output: output.clone(),
                    scope: scope.clone()
                }
            })

        },

        /// Function applications lead to a new scope being created to stick the
        /// function inputs into, and then simplifying the body against that.
        Expr::App{ expr: ref expr, ref args } => {

            // simplify the func expr, which might at this point be a variable
            // or something. Then, simplify the args.
            let func = eval(expr.clone(), scope.clone(), context)?;
            let mut simplified_args = Vec::with_capacity(args.len());
            for arg in args.into_iter() {
                let simplified_arg = eval(arg.clone(), scope.clone(), context)?;
                simplified_args.push(simplified_arg);
            }

            // now we've simplified the args and the func, see what type of
            // func we are dealing with so that we can apply the args to it
            // and get back a result.
            match func.expr {
                Expr::Func{ inputs: ref arg_names, output: ref func_e, ref scope } => {

                    if arg_names.len() != simplified_args.len() {
                        return err!(e,WrongNumberOfArguments{
                            expected: arg_names.len(),
                            got: simplified_args.len()
                        });
                    }

                    // create scope containing simplified args to make use of in function body expr:
                    let mut function_scope = HashMap::new();
                    for (name, arg) in arg_names.into_iter().zip(simplified_args) {
                        function_scope.insert(name.to_owned(),arg);
                    }

                    eval(func_e.clone(), scope.push(function_scope), context)

                },
                Expr::PrimFunc(ref func) => {

                    // primitive func? just run it on the args then!
                    match func.0(simplified_args, context) {
                        Ok(res) => Ok(expression_from!{e, res}),
                        Err(err) => err!{e, err}
                    }

                }
                _ => {
                    err!(func, NotAFunction)
                }
            }

        },

        /// For Blocks, we do our best to simplify the block contents, complaining
        /// if there is something invalid somewhere.
        Expr::Block(ref block_enum) => {

            /// wrap the output appropriately:
            let start = e.start;
            let end = e.end;
            let out = |b| EvaluatedBlock{start:start,end:end,block:b};

            /// simplify the block as best as we can, throwing up any errors in doing so.
            let simplified_block = match *block_enum {
                Block::KeyframesBlock(ref block) => {
                    let block_scope = simplify_block_scope(&block.scope, &scope, context)?;
                    let new_scope = scope.push(block_scope.clone());

                    out(Block::KeyframesBlock(KeyframesBlock{
                        scope: block_scope,
                        name: try_cssbits_to_string(&block.name, &new_scope, context)?,
                        inner: try_eval_cssentries(&block.inner, &new_scope, context)?
                    }))
                },
                Block::MediaBlock(ref block) => {
                    let block_scope = simplify_block_scope(&block.scope, &scope, context)?;
                    let new_scope = scope.push(block_scope.clone());

                    out(Block::MediaBlock(MediaBlock{
                        scope: block_scope,
                        query: try_cssbits_to_string(&block.query, &new_scope, context)?,
                        css: try_eval_cssentries(&block.css, &new_scope, context)?
                    }))
                },
                Block::FontFaceBlock(ref block) => {
                    let block_scope = simplify_block_scope(&block.scope, &scope, context)?;
                    let new_scope = scope.push(block_scope.clone());

                    out(Block::FontFaceBlock(FontFaceBlock{
                        scope: block_scope,
                        css: try_eval_cssentries(&block.css, &new_scope, context)?
                    }))
                },
                Block::CSSBlock(ref block) => {
                    let block_scope = simplify_block_scope(&block.scope, &scope, context)?;
                    let new_scope = scope.push(block_scope.clone());

                    out(Block::CSSBlock(CSSBlock{
                        scope: block_scope,
                        selector: try_cssbits_to_string(&block.selector, &new_scope, context)?,
                        css: try_eval_cssentries(&block.css, &new_scope, context)?
                    }))
                }
            };

            Ok(expression_from!{e, Expr::EvaluatedBlock(simplified_block)})

        },


    }

}

fn simplify_block_scope(block_scope: &HashMap<String,Expression>, scope: &Scope, context: &Context) -> Result<HashMap<String,Expression>,Error> {
    let new_scope = scope.push(block_scope.clone());
    let mut simplified_block_scope = HashMap::new();
    for (name,expr) in block_scope {
        let s = new_scope.push_one(name.clone(), expression_from!{ expr, Expr::Prim(Primitive::RecursiveValue) });
        let simplified_expr = eval(expr.clone(),s,context)?;
        simplified_block_scope.insert(name.to_owned(), simplified_expr);
    }
    Ok(simplified_block_scope)
}
fn try_cssbits_to_string(bits: &Vec<CSSBit>, scope: &Scope, context: &Context) -> Result<String,Error> {
    let mut string = vec![];
    for bit in bits {
        match *bit {
            CSSBit::Str(ref s) => string.push(s.to_owned()),
            CSSBit::Expr(ref e) => {
                let e = eval(e.clone(), scope.clone(),context)?;
                use prelude::casting::raw_string;
                let s = match raw_string(&e.expr) {
                    Ok(s) => Ok(s),
                    Err(err) => err!{e,err}
                }?;
                string.push(s);
            }
        }
    }
    Ok(string.concat())
}
fn try_eval_cssentries(entries: &Vec<CSSEntry>, scope: &Scope, context: &Context) -> Result<Vec<EvaluatedCSSEntry>,Error> {
    let mut out = vec![];
    for val in entries {
        match *val {
            CSSEntry::Expr(ref expr) => {
                let css_expr = eval(expr.clone(), scope.clone(),context)?;
                match css_expr.expr {
                    Expr::EvaluatedBlock(block) => out.push(EvaluatedCSSEntry::Block(block)),
                    _ => return err!(css_expr, NotACSSBlock)
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