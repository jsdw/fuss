use types::*;
use types::Primitive::*;
use prelude::casting;

/// The "+" operator.
pub fn add(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    let b = args.remove(1);
    let a = args.remove(0);

    match (a.expr, b.expr) {
        (Expr::Prim(Unit(a, au)), Expr::Prim(Unit(b, bu))) => {

            let unit = pick_unit(au,bu)?;
            Ok(Expr::Prim(Unit(a + b, unit)))

        },
        (Expr::Prim(Str(a)), Expr::Prim(Str(b))) => {

            Ok(Expr::Prim(Str(a + &b)))

        },
        _ => Err(ErrorType::WrongTypeOfArguments{ message: "only numbers and strings can be added together".to_owned() })
    }

}

/// The "-" operator. Needs to handle unary minus or subtract.
pub fn subtract(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    let len = args.len();

    if len == 1 {

        /// unary negation
        let a = args.remove(0);

        if let Expr::Prim(Unit(a, au)) = a.expr {
            Ok(Expr::Prim(Unit(-a, au)))
        } else {
            Err(ErrorType::WrongTypeOfArguments{ message: "unary '-' can only be used on numbers".to_owned() })
        }

    } else if len == 2 {

        /// subtracting
        let b = args.remove(1);
        let a = args.remove(0);

        if let (Expr::Prim(Unit(a, au)), Expr::Prim(Unit(b, bu))) = (a.expr,b.expr) {

            let unit = pick_unit(au,bu)?;
            Ok(Expr::Prim(Unit(a - b, unit)))

        } else {
            Err(ErrorType::WrongTypeOfArguments{ message: "only numbers can be minused from eachother".to_owned() })
        }


    } else {
        Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() })
    }

}

/// The "/" operator
pub fn divide(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    let b = args.remove(1);
    let a = args.remove(0);

    if let (Expr::Prim(Unit(a, au)), Expr::Prim(Unit(b, bu))) = (a.expr,b.expr) {

        let unit = pick_unit(au,bu)?;
        Ok(Expr::Prim(Unit(a / b, unit)))

    } else {
        Err(ErrorType::WrongTypeOfArguments{ message: "only numbers can be divided with eachother".to_owned() })
    }

}

/// The "*" operator
pub fn multiply(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    let b = args.remove(1);
    let a = args.remove(0);

    if let (Expr::Prim(Unit(a, au)), Expr::Prim(Unit(b, bu))) = (a.expr,b.expr) {

        let unit = pick_unit(au,bu)?;
        Ok(Expr::Prim(Unit(a * b, unit)))

    } else {
        Err(ErrorType::WrongTypeOfArguments{ message: "only numbers can be multiplied with eachother".to_owned() })
    }

}

/// The "^" operator
pub fn pow(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    let b = args.remove(1);
    let a = args.remove(0);

    if let (Expr::Prim(Unit(a, au)), Expr::Prim(Unit(b, bu))) = (a.expr,b.expr) {

        let unit = pick_unit(au,bu)?;
        Ok(Expr::Prim(Unit(a.powf(b), unit)))

    } else {
        Err(ErrorType::WrongTypeOfArguments{ message: "only numbers can be divided with eachother".to_owned() })
    }

}

/// The unary "!" operator
pub fn not(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() != 1 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 1, got: args.len() });
    }

    let a = args.remove(0);
    let b = casting::raw_boolean(a.expr)?;

    Ok(Expr::Prim(Bool(!b)))

}

/// "=="
pub fn equal(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    let b = args.remove(1);
    let a = args.remove(0);

    Ok(Expr::Prim(Bool(a == b)))

}

/// "!="
pub fn not_equal(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    let b = args.remove(1);
    let a = args.remove(0);

    Ok(Expr::Prim(Bool(a != b)))

}

/// ">"
pub fn greater_than(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    let b = args.remove(1);
    let a = args.remove(0);

    match (a.expr, b.expr) {
        (Expr::Prim(Unit(a, au)), Expr::Prim(Unit(b, bu))) => {
            Ok(Expr::Prim(Bool(a > b)))
        },
        (Expr::Prim(Str(a)), Expr::Prim(Str(b))) => {
            Ok(Expr::Prim(Bool(a > b)))
        },
        _ => Err(ErrorType::WrongTypeOfArguments{ message: "only numbers and strings can be compared with '>'".to_owned() })
    }

}

/// ">="
pub fn greater_than_or_equal(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    let b = args.remove(1);
    let a = args.remove(0);

    match (a.expr, b.expr) {
        (Expr::Prim(Unit(a, au)), Expr::Prim(Unit(b, bu))) => {
            Ok(Expr::Prim(Bool(a >= b)))
        },
        (Expr::Prim(Str(a)), Expr::Prim(Str(b))) => {
            Ok(Expr::Prim(Bool(a >= b)))
        },
        _ => Err(ErrorType::WrongTypeOfArguments{ message: "only numbers and strings can be compared with '>='".to_owned() })
    }

}

/// "<"
pub fn less_than(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    let b = args.remove(1);
    let a = args.remove(0);

    match (a.expr, b.expr) {
        (Expr::Prim(Unit(a, au)), Expr::Prim(Unit(b, bu))) => {
            Ok(Expr::Prim(Bool(a < b)))
        },
        (Expr::Prim(Str(a)), Expr::Prim(Str(b))) => {
            Ok(Expr::Prim(Bool(a < b)))
        },
        _ => Err(ErrorType::WrongTypeOfArguments{ message: "only numbers and strings can be compared with '<'".to_owned() })
    }

}

/// "<="
pub fn less_than_or_equal(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    let b = args.remove(1);
    let a = args.remove(0);

    match (a.expr, b.expr) {
        (Expr::Prim(Unit(a, au)), Expr::Prim(Unit(b, bu))) => {
            Ok(Expr::Prim(Bool(a <= b)))
        },
        (Expr::Prim(Str(a)), Expr::Prim(Str(b))) => {
            Ok(Expr::Prim(Bool(a <= b)))
        },
        _ => Err(ErrorType::WrongTypeOfArguments{ message: "only numbers and strings can be compared with '<='".to_owned() })
    }

}

/// "&&"
pub fn boolean_and(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    let b = args.remove(1);
    let a = args.remove(0);

    if let (Expr::Prim(Bool(a)), Expr::Prim(Bool(b))) = (a.expr,b.expr) {
        Ok(Expr::Prim(Bool(a && b)))
    } else {
        Err(ErrorType::WrongTypeOfArguments{ message: "only booleans can be &&'d".to_owned() })
    }

}

/// "||"
pub fn boolean_or(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    let b = args.remove(1);
    let a = args.remove(0);

    if let (Expr::Prim(Bool(a)), Expr::Prim(Bool(b))) = (a.expr,b.expr) {
        Ok(Expr::Prim(Bool(a || b)))
    } else {
        Err(ErrorType::WrongTypeOfArguments{ message: "only booleans can be ||'d".to_owned() })
    }

}

fn pick_unit(au: String, bu: String) -> Result<String,ErrorType> {
    if au.len() > 0 && bu.len() > 0 && au != bu {
        Err(ErrorType::UnitMismatch)
    } else if au.len() > 0 {
        Ok(au)
    } else {
        Ok(bu)
    }
}