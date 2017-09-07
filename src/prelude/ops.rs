use types::*;
use prelude::casting;

/// The "+" operator.
pub fn add(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    match (&args[0].expr, &args[1].expr) {
        (&Expr::Unit(a, ref au), &Expr::Unit(b, ref bu)) => {

            let unit = pick_unit(au,bu)?;
            Ok(Expr::Unit(a + b, unit))

        },
        (&Expr::Str(ref a), &Expr::Str(ref b)) => {

            Ok(Expr::Str(a.to_owned() + b))

        },
        _ => Err(ErrorType::WrongTypeOfArguments{ message: "only numbers and strings can be added together".to_owned() })
    }

}

/// The "-" operator. Needs to handle unary minus or subtract.
pub fn subtract(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    let len = args.len();

    if len == 1 {

        let a = &args[0];

        if let Expr::Unit(a, ref au) = a.expr {
            Ok(Expr::Unit(-a, au.to_owned()))
        } else {
            Err(ErrorType::WrongTypeOfArguments{ message: "unary '-' can only be used on numbers".to_owned() })
        }

    } else if len == 2 {

        if let (&Expr::Unit(a, ref au), &Expr::Unit(b, ref bu)) = (&args[0].expr,&args[1].expr) {

            let unit = pick_unit(au,bu)?;
            Ok(Expr::Unit(a - b, unit))

        } else {
            Err(ErrorType::WrongTypeOfArguments{ message: "only numbers can be minused from eachother".to_owned() })
        }


    } else {
        Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() })
    }

}

/// The "/" operator
pub fn divide(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    if let (&Expr::Unit(a, ref au), &Expr::Unit(b, ref bu)) = (&args[0].expr,&args[1].expr) {

        let unit = pick_unit(au,bu)?;
        Ok(Expr::Unit(a / b, unit))

    } else {
        Err(ErrorType::WrongTypeOfArguments{ message: "only numbers can be divided with eachother".to_owned() })
    }

}

/// The "*" operator
pub fn multiply(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    if let (&Expr::Unit(a, ref au), &Expr::Unit(b, ref bu)) = (&args[0].expr,&args[1].expr) {

        let unit = pick_unit(au,bu)?;
        Ok(Expr::Unit(a * b, unit))

    } else {
        Err(ErrorType::WrongTypeOfArguments{ message: "only numbers can be multiplied with eachother".to_owned() })
    }

}

/// The "^" operator
pub fn pow(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    if let (&Expr::Unit(a, ref au), &Expr::Unit(b, ref bu)) = (&args[0].expr,&args[1].expr) {

        let unit = pick_unit(au,bu)?;
        Ok(Expr::Unit(a.powf(b), unit))

    } else {
        Err(ErrorType::WrongTypeOfArguments{ message: "only numbers can be divided with eachother".to_owned() })
    }

}

/// The unary "!" operator
pub fn not(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 1 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 1, got: args.len() });
    }

    let b = casting::raw_boolean(&args[0].expr)?;

    Ok(Expr::Bool(!b))

}

/// "=="
pub fn equal(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    Ok(Expr::Bool(args[0].expr == args[1].expr))

}

/// "!="
pub fn not_equal(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    Ok(Expr::Bool(args[0].expr != args[1].expr))

}

/// ">"
pub fn greater_than(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    match (&args[0].expr, &args[1].expr) {
        (&Expr::Unit(a, _), &Expr::Unit(b, _)) => {
            Ok(Expr::Bool(a > b))
        },
        (&Expr::Str(ref a), &Expr::Str(ref b)) => {
            Ok(Expr::Bool(a > b))
        },
        _ => Err(ErrorType::WrongTypeOfArguments{ message: "only numbers and strings can be compared with '>'".to_owned() })
    }

}

/// ">="
pub fn greater_than_or_equal(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    match (&args[0].expr, &args[1].expr) {
        (&Expr::Unit(a, _), &Expr::Unit(b, _)) => {
            Ok(Expr::Bool(a >= b))
        },
        (&Expr::Str(ref a), &Expr::Str(ref b)) => {
            Ok(Expr::Bool(a >= b))
        },
        _ => Err(ErrorType::WrongTypeOfArguments{ message: "only numbers and strings can be compared with '>='".to_owned() })
    }

}

/// "<"
pub fn less_than(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    match (&args[0].expr, &args[1].expr) {
        (&Expr::Unit(a, _), &Expr::Unit(b, _)) => {
            Ok(Expr::Bool(a < b))
        },
        (&Expr::Str(ref a), &Expr::Str(ref b)) => {
            Ok(Expr::Bool(a < b))
        },
        _ => Err(ErrorType::WrongTypeOfArguments{ message: "only numbers and strings can be compared with '<'".to_owned() })
    }

}

/// "<="
pub fn less_than_or_equal(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    match (&args[0].expr, &args[1].expr) {
        (&Expr::Unit(a, _), &Expr::Unit(b, _)) => {
            Ok(Expr::Bool(a <= b))
        },
        (&Expr::Str(ref a), &Expr::Str(ref b)) => {
            Ok(Expr::Bool(a <= b))
        },
        _ => Err(ErrorType::WrongTypeOfArguments{ message: "only numbers and strings can be compared with '<='".to_owned() })
    }

}

/// "&&"
pub fn boolean_and(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    if let (&Expr::Bool(a), &Expr::Bool(b)) = (&args[0].expr,&args[1].expr) {
        Ok(Expr::Bool(a && b))
    } else {
        Err(ErrorType::WrongTypeOfArguments{ message: "only booleans can be &&'d".to_owned() })
    }

}

/// "||"
pub fn boolean_or(args: &Vec<Expression>, _context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    if let (&Expr::Bool(a), &Expr::Bool(b)) = (&args[0].expr,&args[1].expr) {
        Ok(Expr::Bool(a || b))
    } else {
        Err(ErrorType::WrongTypeOfArguments{ message: "only booleans can be ||'d".to_owned() })
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