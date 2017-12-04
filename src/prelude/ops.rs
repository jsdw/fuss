use types::*;
use errors::*;
use prelude::casting;

/// The "+" operator.
pub fn add(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    match (&args[0].expr, &args[1].expr) {
        (&EvaluatedExpr::Unit(a, ref au), &EvaluatedExpr::Unit(b, ref bu)) => {

            let unit = pick_unit(au,bu)?;
            Ok(EvaluatedExpr::Unit(a + b, unit))

        },
        (&EvaluatedExpr::Str(ref a), &EvaluatedExpr::Str(ref b)) => {

            Ok(EvaluatedExpr::Str(a.to_owned() + b))

        },
        _ => Err(ApplicationError::WrongKindOfArguments{ message: "only numbers and strings can be added together".to_owned() }.into())
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
            Err(ApplicationError::WrongKindOfArguments{ message: "unary '-' can only be used on numbers".to_owned() }.into())
        }

    } else if len == 2 {

        if let (&EvaluatedExpr::Unit(a, ref au), &EvaluatedExpr::Unit(b, ref bu)) = (&args[0].expr,&args[1].expr) {

            let unit = pick_unit(au,bu)?;
            Ok(EvaluatedExpr::Unit(a - b, unit))

        } else {
            Err(ApplicationError::WrongKindOfArguments{ message: "only numbers can be minused from eachother".to_owned() }.into())
        }


    } else {
        Err(ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() })
    }

}

/// The "/" operator
pub fn divide(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    if let (&EvaluatedExpr::Unit(a, ref au), &EvaluatedExpr::Unit(b, ref bu)) = (&args[0].expr,&args[1].expr) {

        let unit = pick_unit(au,bu)?;
        Ok(EvaluatedExpr::Unit(a / b, unit))

    } else {
        Err(ApplicationError::WrongKindOfArguments{ message: "only numbers can be divided with eachother".to_owned() }.into())
    }

}

/// The "*" operator
pub fn multiply(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    if let (&EvaluatedExpr::Unit(a, ref au), &EvaluatedExpr::Unit(b, ref bu)) = (&args[0].expr,&args[1].expr) {

        let unit = pick_unit(au,bu)?;
        Ok(EvaluatedExpr::Unit(a * b, unit))

    } else {
        Err(ApplicationError::WrongKindOfArguments{ message: "only numbers can be multiplied with eachother".to_owned() }.into())
    }

}

/// The "^" operator
pub fn pow(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    if let (&EvaluatedExpr::Unit(a, ref au), &EvaluatedExpr::Unit(b, ref bu)) = (&args[0].expr,&args[1].expr) {

        let unit = pick_unit(au,bu)?;
        Ok(EvaluatedExpr::Unit(a.powf(b), unit))

    } else {
        Err(ApplicationError::WrongKindOfArguments{ message: "only numbers can be divided with eachother".to_owned() }.into())
    }

}

/// The unary "!" operator
pub fn not(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 1 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 1, got: args.len() });
    }

    let b = casting::raw_boolean(&args[0].expr)?;

    Ok(EvaluatedExpr::Bool(!b))

}

/// "=="
pub fn equal(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    Ok(EvaluatedExpr::Bool(args[0].expr == args[1].expr))

}

/// "!="
pub fn not_equal(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    Ok(EvaluatedExpr::Bool(args[0].expr != args[1].expr))

}

/// ">"
pub fn greater_than(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    match (&args[0].expr, &args[1].expr) {
        (&EvaluatedExpr::Unit(a, _), &EvaluatedExpr::Unit(b, _)) => {
            Ok(EvaluatedExpr::Bool(a > b))
        },
        (&EvaluatedExpr::Str(ref a), &EvaluatedExpr::Str(ref b)) => {
            Ok(EvaluatedExpr::Bool(a > b))
        },
        _ => Err(ApplicationError::WrongKindOfArguments{ message: "only numbers and strings can be compared with '>'".to_owned() }.into())
    }

}

/// ">="
pub fn greater_than_or_equal(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    match (&args[0].expr, &args[1].expr) {
        (&EvaluatedExpr::Unit(a, _), &EvaluatedExpr::Unit(b, _)) => {
            Ok(EvaluatedExpr::Bool(a >= b))
        },
        (&EvaluatedExpr::Str(ref a), &EvaluatedExpr::Str(ref b)) => {
            Ok(EvaluatedExpr::Bool(a >= b))
        },
        _ => Err(ApplicationError::WrongKindOfArguments{ message: "only numbers and strings can be compared with '>='".to_owned() }.into())
    }

}

/// "<"
pub fn less_than(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    match (&args[0].expr, &args[1].expr) {
        (&EvaluatedExpr::Unit(a, _), &EvaluatedExpr::Unit(b, _)) => {
            Ok(EvaluatedExpr::Bool(a < b))
        },
        (&EvaluatedExpr::Str(ref a), &EvaluatedExpr::Str(ref b)) => {
            Ok(EvaluatedExpr::Bool(a < b))
        },
        _ => Err(ApplicationError::WrongKindOfArguments{ message: "only numbers and strings can be compared with '<'".to_owned() }.into())
    }

}

/// "<="
pub fn less_than_or_equal(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    match (&args[0].expr, &args[1].expr) {
        (&EvaluatedExpr::Unit(a, _), &EvaluatedExpr::Unit(b, _)) => {
            Ok(EvaluatedExpr::Bool(a <= b))
        },
        (&EvaluatedExpr::Str(ref a), &EvaluatedExpr::Str(ref b)) => {
            Ok(EvaluatedExpr::Bool(a <= b))
        },
        _ => Err(ApplicationError::WrongKindOfArguments{ message: "only numbers and strings can be compared with '<='".to_owned() }.into())
    }

}

/// "&&"
pub fn boolean_and(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    if let (&EvaluatedExpr::Bool(a), &EvaluatedExpr::Bool(b)) = (&args[0].expr,&args[1].expr) {
        Ok(EvaluatedExpr::Bool(a && b))
    } else {
        Err(ApplicationError::WrongKindOfArguments{ message: "only booleans can be &&'d".to_owned() }.into())
    }

}

/// "||"
pub fn boolean_or(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    if let (&EvaluatedExpr::Bool(a), &EvaluatedExpr::Bool(b)) = (&args[0].expr,&args[1].expr) {
        Ok(EvaluatedExpr::Bool(a || b))
    } else {
        Err(ApplicationError::WrongKindOfArguments{ message: "only booleans can be ||'d".to_owned() }.into())
    }

}

fn pick_unit(au: &str, bu: &str) -> Result<String,ErrorType> {
    if au.len() > 0 && bu.len() > 0 && au != bu {
        Err(ErrorType::UnitMismatch)
    } else if au.len() > 0 {
        Ok(au.to_owned())
    } else {
        Ok(bu.to_owned())
    }
}