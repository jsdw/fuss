use types::*;
use errors::*;
use prelude::casting;

/// The "+" operator.
pub fn add(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() }.into();
    }

    match (&args[0].expr, &args[1].expr) {
        (&EvaluatedExpr::Unit(a, ref au), &EvaluatedExpr::Unit(b, ref bu)) => {
            let unit = pick_unit(au,bu)?;
            Ok(EvaluatedExpr::Unit(a + b, unit))
        },
        (&EvaluatedExpr::Str(ref a), &EvaluatedExpr::Str(ref b)) => {
            Ok(EvaluatedExpr::Str(a.to_owned() + b))
        },
        (_,&EvaluatedExpr::Str{..}) | (_,&EvaluatedExpr::Unit{..}) => {
            ApplicationError::WrongKindOfArguments{ index: 0, expected: vec![Kind::Str,Kind::Unit], got: args[0].expr.kind() }.into()
        },
        _ => {
            ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Str,Kind::Unit], got: args[1].expr.kind() }.into()
        }
    }

}

/// The "-" operator. Needs to handle unary minus or subtract.
pub fn subtract(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    let len = args.len();

    if len == 1 {

        let a = &args[0];

        if let EvaluatedExpr::Unit(a, ref au) = a.expr {
            Ok(EvaluatedExpr::Unit(-a, au.to_owned()))
        } else {
            ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Unit], got: args[0].expr.kind() }.into()
        }

    } else if len == 2 {

        match (&args[0].expr,&args[1].expr) {
            (&EvaluatedExpr::Unit(a, ref au), &EvaluatedExpr::Unit(b, ref bu)) => {
                let unit = pick_unit(au,bu)?;
                Ok(EvaluatedExpr::Unit(a - b, unit))
            },
            (_,&EvaluatedExpr::Unit{..}) => {
                ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Unit], got: args[0].expr.kind() }.into()
            },
            _ => {
                ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Unit], got: args[1].expr.kind() }.into()
            }
        }

    } else {
        ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() }.into()
    }

}

/// The "/" operator
pub fn divide(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() }.into();
    }

    match (&args[0].expr,&args[1].expr) {
        (&EvaluatedExpr::Unit(a, ref au), &EvaluatedExpr::Unit(b, ref bu)) => {
            let unit = pick_unit(au,bu)?;
            Ok(EvaluatedExpr::Unit(a / b, unit))
        },
        (_,&EvaluatedExpr::Unit{..}) => {
            ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Unit], got: args[0].expr.kind() }.into()
        },
        _ => {
            ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Unit], got: args[1].expr.kind() }.into()
        }
    }

}

/// The "*" operator
pub fn multiply(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() }.into();
    }

    match (&args[0].expr,&args[1].expr) {
        (&EvaluatedExpr::Unit(a, ref au), &EvaluatedExpr::Unit(b, ref bu)) => {
            let unit = pick_unit(au,bu)?;
            Ok(EvaluatedExpr::Unit(a * b, unit))
        },
        (_,&EvaluatedExpr::Unit{..}) => {
            ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Unit], got: args[0].expr.kind() }.into()
        },
        _ => {
            ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Unit], got: args[1].expr.kind() }.into()
        }
    }

}

/// The "^" operator
pub fn pow(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() }.into();
    }

    match (&args[0].expr,&args[1].expr) {
        (&EvaluatedExpr::Unit(a, ref au), &EvaluatedExpr::Unit(b, ref bu)) => {
            let unit = pick_unit(au,bu)?;
            Ok(EvaluatedExpr::Unit(a.powf(b), unit))
        },
        (_,&EvaluatedExpr::Unit{..}) => {
            ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Unit], got: args[0].expr.kind() }.into()
        },
        _ => {
            ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Unit], got: args[1].expr.kind() }.into()
        }
    }

}

/// The unary "!" operator
pub fn not(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 1 {
        return ApplicationError::WrongNumberOfArguments{ expected: 1, got: args.len() }.into();
    }

    let b = casting::raw_boolean(&args[0].expr)?;

    Ok(EvaluatedExpr::Bool(!b))

}

/// "=="
pub fn equal(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() }.into();
    }

    Ok(EvaluatedExpr::Bool(args[0].expr == args[1].expr))

}

/// "!="
pub fn not_equal(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() }.into();
    }

    Ok(EvaluatedExpr::Bool(args[0].expr != args[1].expr))

}

/// ">"
pub fn greater_than(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() }.into();
    }

    match (&args[0].expr, &args[1].expr) {
        (&EvaluatedExpr::Unit(a, _), &EvaluatedExpr::Unit(b, _)) => {
            Ok(EvaluatedExpr::Bool(a > b))
        },
        (&EvaluatedExpr::Str(ref a), &EvaluatedExpr::Str(ref b)) => {
            Ok(EvaluatedExpr::Bool(a > b))
        },
        (_,&EvaluatedExpr::Str{..}) | (_,&EvaluatedExpr::Unit{..}) => {
            ApplicationError::WrongKindOfArguments{ index: 0, expected: vec![Kind::Str,Kind::Unit], got: args[0].expr.kind() }.into()
        },
        _ => ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Str,Kind::Unit], got: args[1].expr.kind() }.into()    }

}

/// ">="
pub fn greater_than_or_equal(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() }.into();
    }

    match (&args[0].expr, &args[1].expr) {
        (&EvaluatedExpr::Unit(a, _), &EvaluatedExpr::Unit(b, _)) => {
            Ok(EvaluatedExpr::Bool(a >= b))
        },
        (&EvaluatedExpr::Str(ref a), &EvaluatedExpr::Str(ref b)) => {
            Ok(EvaluatedExpr::Bool(a >= b))
        },
        (_,&EvaluatedExpr::Str{..}) | (_,&EvaluatedExpr::Unit{..}) => {
            ApplicationError::WrongKindOfArguments{ index: 0, expected: vec![Kind::Str,Kind::Unit], got: args[0].expr.kind() }.into()
        },
        _ => ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Str,Kind::Unit], got: args[1].expr.kind() }.into()    }

}

/// "<"
pub fn less_than(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() }.into();
    }

    match (&args[0].expr, &args[1].expr) {
        (&EvaluatedExpr::Unit(a, _), &EvaluatedExpr::Unit(b, _)) => {
            Ok(EvaluatedExpr::Bool(a < b))
        },
        (&EvaluatedExpr::Str(ref a), &EvaluatedExpr::Str(ref b)) => {
            Ok(EvaluatedExpr::Bool(a < b))
        },
        (_,&EvaluatedExpr::Str{..}) | (_,&EvaluatedExpr::Unit{..}) => {
            ApplicationError::WrongKindOfArguments{ index: 0, expected: vec![Kind::Str,Kind::Unit], got: args[0].expr.kind() }.into()
        },
        _ => ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Str,Kind::Unit], got: args[1].expr.kind() }.into()    }

}

/// "<="
pub fn less_than_or_equal(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() }.into();
    }

    match (&args[0].expr, &args[1].expr) {
        (&EvaluatedExpr::Unit(a, _), &EvaluatedExpr::Unit(b, _)) => {
            Ok(EvaluatedExpr::Bool(a <= b))
        },
        (&EvaluatedExpr::Str(ref a), &EvaluatedExpr::Str(ref b)) => {
            Ok(EvaluatedExpr::Bool(a <= b))
        },
        (_,&EvaluatedExpr::Str{..}) | (_,&EvaluatedExpr::Unit{..}) => {
            ApplicationError::WrongKindOfArguments{ index: 0, expected: vec![Kind::Str,Kind::Unit], got: args[0].expr.kind() }.into()
        },
        _ => ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Str,Kind::Unit], got: args[1].expr.kind() }.into()    }

}

/// "&&"
pub fn boolean_and(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() }.into();
    }

    match (&args[0].expr,&args[1].expr) {
        (&EvaluatedExpr::Bool(a), &EvaluatedExpr::Bool(b)) => {
            Ok(EvaluatedExpr::Bool(a && b))
        },
        (_,&EvaluatedExpr::Bool{..}) => {
            ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Bool], got: args[0].expr.kind() }.into()
        },
        _ => {
            ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Bool], got: args[1].expr.kind() }.into()
        }
    }

}

/// "||"
pub fn boolean_or(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() }.into();
    }

    match (&args[0].expr,&args[1].expr) {
        (&EvaluatedExpr::Bool(a), &EvaluatedExpr::Bool(b)) => {
            Ok(EvaluatedExpr::Bool(a || b))
        },
        (_,&EvaluatedExpr::Bool{..}) => {
            ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Bool], got: args[0].expr.kind() }.into()
        },
        _ => {
            ApplicationError::WrongKindOfArguments{ index: 1, expected: vec![Kind::Bool], got: args[1].expr.kind() }.into()
        }
    }

}

fn pick_unit(au: &str, bu: &str) -> Result<String,ErrorKind> {
    if au.len() > 0 && bu.len() > 0 && au != bu {
        ApplicationError::UnitMismatch.into()
    } else if au.len() > 0 {
        Ok(au.to_owned())
    } else {
        Ok(bu.to_owned())
    }
}