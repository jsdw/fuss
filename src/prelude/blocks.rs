use types::*;
use errors::*;
use std::collections::HashMap;

/// merge variables declared in blocks, outputting a single block. Ignore Undefined variables.
pub fn merge(args: &Vec<EvaluatedExpression>, context: &Context) -> PrimRes {

    let mut ty = BlockType::Generic;
    let mut scope = HashMap::new();
    let mut css = Vec::new();
    let mut selector = String::new();
    let mut seen_first = false;

    for (idx,e) in args.iter().enumerate() {
        match *e.expr() {
            EvaluatedExpr::Undefined => {
                if !seen_first {
                    return Err(ApplicationError::WrongKindOfArguments{
                        index: idx,
                        expected: vec![Kind::Block],
                        got: Kind::Undefined
                    }.into());
                }
            },
            EvaluatedExpr::Block(ref block) => {
                for (key,val) in &block.scope {
                    if val.expr() == &EvaluatedExpr::Undefined { continue }
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
                return Err(ApplicationError::WrongKindOfArguments{
                    index: idx,
                    expected: vec![Kind::Block],
                    got: e.expr().kind()
                }.into());
            }
        }
        seen_first = true;
    }

    Ok(EvaluatedExpr::Block(EvaluatedBlock{
        ty: ty,
        //@todo improve this:
        at: At::position(&context.path, 0, 0),
        scope: scope,
        selector: selector,
        css: css
    }))

}