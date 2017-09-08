use types::*;
use std::collections::HashMap;

/// merge variables declared in blocks, outputting a single block. Ignore Undefined variables.
pub fn merge(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    let mut scope = HashMap::new();
    let mut css = Vec::new();
    let mut selector = String::new();
    let mut seen_first = false;

    for e in args {
        match e.expr {
            Expr::Undefined => {
                if !seen_first {
                    return Err(ErrorType::WrongTypeOfArguments{
                        message: "the first argument should not be undefined".to_owned()
                    });
                }
            },
            Expr::EvaluatedBlock(ref b) => {
                if let Block::CSSBlock(ref c) = b.block {
                    for (key,val) in &c.scope {
                        if val.expr == Expr::Undefined { continue }
                        scope.insert(key.clone(), val.clone());
                    }
                    for item in &c.css {
                        css.push(item.clone());
                    }
                    if !seen_first {
                        selector = c.selector.clone();
                    }
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

    Ok(Expr::EvaluatedBlock(EvaluatedBlock{
        start: Position::new(),
        end: Position::new(),
        block: Block::CSSBlock(CSSBlock{
            scope: scope,
            selector: selector,
            css: css
        })
    }))

}