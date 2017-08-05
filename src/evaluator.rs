use types::*;
use types::ErrorType::*;
use std::collections::HashMap;

pub fn eval(e: Expression, scope: Scope, context: &Context) -> Res {

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
                } else {
                    return err!(e, PropertyDoesNotExist(name.clone(), key));
                }
            }

            // if we have found something from our digging, copy and return it:
            Ok(var.clone())

        },

        /// If simplifies based on the boolean-ness of the condition!
        Expr::If{ cond: boxed_cond, then: boxed_then, otherwise: boxed_else } => {

            let cond = eval(*boxed_cond, scope.clone(), context)?;

            use prelude::casting::raw_boolean;
            let is_true = match raw_boolean(cond.expr){
                Ok(b) => Ok(b),
                Err(err) => err!{cond,err}
            }?;

            if is_true {
                eval(*boxed_then, scope, context)
            } else {
                eval(*boxed_else, scope, context)
            }
        },

        /// Func declarations: we want to store the scope that they are seen in
        /// against the function so that it can be applied against the right things.
        /// otherwise, leave as is.
        Expr::Func{ inputs, output, .. } => {

            Ok(expression_from!{e,
                Expr::Func{
                    inputs: inputs,
                    output: output,
                    scope: scope.clone()
                }
            })

        },

        /// Function applications lead to a new scope being created to stick the
        /// function inputs into, and then simplifying the body against that.
        Expr::App{ expr: boxed_expr, args } => {

            // simplify the func expr, which might at this point be a variable
            // or something. Then, simplify the args.
            let func = eval(*boxed_expr, scope.clone(), context)?;
            let mut simplified_args = Vec::with_capacity(args.len());
            for arg in args.into_iter() {
                let simplified_arg = eval(arg, scope.clone(), context)?;
                simplified_args.push(simplified_arg);
            }

            // now we've simplified the args and the func, see what type of
            // func we are dealing with so that we can apply the args to it
            // and get back a result.
            match func.expr {
                Expr::Func{ inputs:arg_names, output:func_e, scope } => {

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

                    eval(*func_e, scope.push(function_scope), context)

                },
                Expr::PrimFunc(func) => {

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

        /// For Blocks, we simplify the CSSEntry Expressions in the context of
        /// the block scope, and complain if they do not themselves resolve to blocks.
        /// We make use of a NestedSimpleBlock type to ensure that we have valid output.
        Expr::Block(block_enum) => {

            match block_enum {
                Block::KeyframesBlock(block) => {

                },
                Block::MediaBlock(block) => {

                },
                Block::FontFaceBlock(block) => {

                },
                Block::CSSBlock(block) => {

                },
            }


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
                let simplified_expr = eval(expr,s,context)?;
                simplified_block_scope.insert(name, simplified_expr);
            }
            let new_scope = scope.push(simplified_block_scope.clone());

            /// This function takes a vector of CSSBits and returns a String.
            /// We use this to decode the strings+expressions in css keys, values
            /// and selector.
            let build_from = |bits: Vec<CSSBit>| -> Result<String,Error> {
                let mut string = vec![];
                for bit in bits {
                    match bit {
                        CSSBit::Str(s) => string.push(s),
                        CSSBit::Expr(e) => {
                            let e = eval(e, new_scope.clone(),context)?;
                            use prelude::casting::raw_string;
                            let s = match raw_string(e.expr) {
                                Ok(s) => Ok(s),
                                Err(err) => err!{e,err}
                            }?;
                            string.push(s);
                        }
                    }
                }
                Ok(string.concat())
            };

            /// build up our simplified block now given the above.
            let selector_string = build_from(block.selector)?;
            let mut blocks = vec![];
            for val in block.css {
                match val {
                    CSSEntry::Expr(expr) => {
                        let css_expr = eval(expr, new_scope.clone(),context)?;
                        let block = match css_expr.expr {
                            Expr::NestedSimpleBlock(block) => block,
                            _ => return err!(css_expr, NotACSSBlock)
                        };
                        blocks.push( NestedCSSEntry::Block(Box::new(block)) );
                    },
                    CSSEntry::KeyVal{ key, val } => {
                        let key_strings = build_from(key)?;
                        let val_strings = build_from(val)?;
                        blocks.push( NestedCSSEntry::KeyVal{ key:key_strings, val:val_strings } );
                    }
                }
            }

            Ok(expression_from!{e,
                Expr::NestedSimpleBlock(NestedSimpleBlock{
                    scope: simplified_block_scope,
                    selector: selector_string,
                    css: blocks
                })
            })

        },


    }

}

fn eval_block_scope(block_scope: Scope, scope: Scope, context: &Context){
    let block_scope = scope.push(block.scope.clone());
    let mut simplified_block_scope = HashMap::new();
    for (name,expr) in block.scope {
        let s = block_scope.push_one(name.clone(), expression_from!{ expr, Expr::Prim(RecursiveValue) });
        let simplified_expr = eval(expr,s,context)?;
        simplified_block_scope.insert(name, simplified_expr);
    }
    let new_scope = scope.push(simplified_block_scope.clone());
}

#[cfg(test)]
pub mod tests {

    use super::*;

}