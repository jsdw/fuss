use types::*;
use std::collections::HashMap;

/// merge variables declared in blocks, outputting a single block. Ignore Undefined variables.
pub fn merge(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    let mut out = HashMap::new();

    for e in args {

        match e.expr {
            Expr::Undefined => {},
            Expr::EvaluatedBlock(ref b) => {
                if let Some(scope) = b.block.scope() {
                    for (key,val) in scope {
                        if val.expr == Expr::Undefined { continue }
                        out.insert(key.clone(), val.clone());
                    }
                }
            },
            _ => {
                return Err(ErrorType::WrongTypeOfArguments{
                    message: "merge_variables only accepts block or undefined variables".to_owned()
                });
            }
        }

    }

    Ok(Expr::EvaluatedBlock(EvaluatedBlock{
        start: Position::new(),
        end: Position::new(),
        block: Block::CSSBlock(CSSBlock{
            scope: out,
            selector: String::new(),
            css: Vec::new()
        })
    }))

}