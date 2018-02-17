use types::*;
use errors::*;

/// cast an expression to a boolean as best we can
pub fn boolean(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 1 {
        return ApplicationError::WrongNumberOfArguments{ expected: 1, got: args.len() }.into();
    }

    let raw = raw_boolean(args[0].expr())?;
    Ok(EvaluatedExpr::Bool(raw))

}

/// cast an expression to a string where possible
pub fn string(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 1 {
        return ApplicationError::WrongNumberOfArguments{ expected: 1, got: args.len() }.into();
    }

    let raw = raw_string(args[0].expr())?;
    Ok(EvaluatedExpr::Str(raw))

}

pub fn raw_string(e: &EvaluatedExpr) -> Result<String,ErrorKind> {
    match *e {
        EvaluatedExpr::Str(ref s) =>
            Ok(s.to_owned()),
        EvaluatedExpr::Bool(ref b) =>
            Ok(if *b { "true".to_owned() } else { "false".to_owned() }),
        EvaluatedExpr::Unit(ref num, ref suffix) =>
            Ok(format!{"{}{}", format!{"{:.5}",num}.trim_right_matches('0').trim_right_matches('.'),suffix}),
        EvaluatedExpr::Colour(ref col) => {
            Ok(format!["rgba({},{},{},{})", col.red_u8(), col.green_u8(), col.blue_u8(), col.alpha()])
        },
        _ => ShapeError::InvalidExpressionInCssValue(Box::new(e.to_owned())).into()
    }
}

pub fn raw_boolean(e: &EvaluatedExpr) -> Result<bool,ErrorKind> {
    Ok(match *e {
        EvaluatedExpr::Str(ref s) => s.len() > 0,
        EvaluatedExpr::Bool(ref b) => *b,
        EvaluatedExpr::Unit(ref n,_) => *n != ::std::f64::NAN && *n != 0f64,
        EvaluatedExpr::Undefined => false,
        _ => true
    })
}