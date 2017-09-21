use types::*;

/// cast an expression to a boolean as best we can
pub fn boolean(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 1 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 1, got: args.len() });
    }

    let raw = raw_boolean(&args[0].expr)?;
    Ok(EvaluatedExpr::Bool(raw))

}

/// cast an expression to a string where possible
pub fn string(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 1 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 1, got: args.len() });
    }

    let raw = raw_string(&args[0].expr)?;
    Ok(EvaluatedExpr::Str(raw))

}

pub fn raw_string(e: &EvaluatedExpr) -> Result<String,ErrorType> {
    match *e {
        EvaluatedExpr::Str(ref s) =>
            Ok(s.to_owned()),
        EvaluatedExpr::Bool(ref b) =>
            Ok(if *b { "true".to_owned() } else { "false".to_owned() }),
        EvaluatedExpr::Unit(ref num, ref suffix) =>
            Ok(format!{"{}{}", format!{"{:.5}",num}.trim_right_matches('0').trim_right_matches('.'),suffix}),
        EvaluatedExpr::Colour(ref col) => {
            let val = |n| (n * 255f32) as u8;
            Ok(format!["rgba({},{},{},{})", val(col.red()), val(col.green()), val(col.blue()), col.alpha()])
        },
        _ => Err(ErrorType::InvalidExpressionInCssValue(Box::new(e.to_owned())))
    }
}

pub fn raw_boolean(e: &EvaluatedExpr) -> Result<bool,ErrorType> {
    Ok(match *e {
        EvaluatedExpr::Str(ref s) => s.len() > 0,
        EvaluatedExpr::Bool(ref b) => *b,
        EvaluatedExpr::Unit(ref n,_) => *n != ::std::f64::NAN && *n != 0f64,
        EvaluatedExpr::Undefined => false,
        _ => true
    })
}