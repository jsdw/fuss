use types::*;
use std::collections::HashMap;

/// merge variables declared in blocks, outputting a single block. Ignore Undefined variables.
pub fn merge(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    let mut ty = BlockType::Generic;
    let mut scope = HashMap::new();
    let mut css = Vec::new();
    let mut selector = String::new();
    let mut seen_first = false;

    for e in args {
        match e.expr {
            EvaluatedExpr::Undefined => {
                if !seen_first {
                    return Err(ErrorType::WrongTypeOfArguments{
                        message: "the first argument should not be undefined".to_owned()
                    });
                }
            },
            EvaluatedExpr::Block(ref block) => {
                for (key,val) in &block.scope {
                    if val.expr == EvaluatedExpr::Undefined { continue }
                    scope.insert(key.clone(), val.clone());
                }
                for item in &block.css {
                    css.push(item.clone());
                }
                if !seen_first {
                    selector = block.selector.clone();
                    ty = block.ty;
                }
            },
            _ => {
                return Err(ErrorType::WrongTypeOfArguments{
                    message: "only accepts css blocks or undefined variables".to_owned()
                });
            }
        }
        seen_first = true;
    }

    Ok(EvaluatedExpr::Block(EvaluatedBlock{
        ty: ty,
        start: Position::new(),
        end: Position::new(),
        scope: scope,
        selector: selector,
        css: css
    }))

}