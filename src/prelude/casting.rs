use types::*;
use types::Primitive::*;

/// cast an expression to a boolean as best we can
pub fn boolean(mut args: Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 1 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 1, got: args.len() });
    }

    let a = args.remove(0);
    let raw = raw_boolean(&a.expr)?;
    Ok(Expr::Prim(Bool(raw)))

}

/// cast an expression to a string where possible
pub fn string(mut args: Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 1 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 1, got: args.len() });
    }

    let a = args.remove(0);
    let raw = raw_string(&a.expr)?;
    Ok(Expr::Prim(Str(raw)))

}

pub fn raw_string(e: &Expr) -> Result<String,ErrorType> {
    match *e {
        Expr::Prim(Str(ref s)) => Ok(s.to_owned()),
        Expr::Prim(Bool(ref b)) => Ok(if *b { "true".to_owned() } else { "false".to_owned() }),
        Expr::Prim(Unit(ref num, ref suffix)) => Ok(format!{"{}{}", format!{"{:.5}",num}.trim_right_matches('0').trim_right_matches('.'),suffix}),
        _ => Err(ErrorType::InvalidExpressionInCssValue(Box::new(e.to_owned())))
    }
}

pub fn raw_boolean(e: &Expr) -> Result<bool,ErrorType> {
    Ok(match *e {
        Expr::Prim(Str(ref s)) => s.len() > 0,
        Expr::Prim(Bool(ref b)) => *b,
        Expr::Prim(Unit(ref n,_)) => *n != ::std::f64::NAN && *n != 0f64,
        _ => true
    })
}