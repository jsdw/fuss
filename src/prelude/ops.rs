use types::*;
use types::Primitive::*;

/// The "+" operator.
pub fn Add(mut args: Vec<Expression>) -> PrimRes {

    if args.len() != 2 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() });
    }

    let b = args.remove(1);
    let a = args.remove(0);

    match (a.expr, b.expr) {
        (Expr::Prim(Unit(a, au)), Expr::Prim(Unit(b, bu))) => {

            let unit =
                if au.len() > 0 && bu.len() > 0 && au != bu {
                    Err(ErrorType::UnitMismatch)
                } else if au.len() > 0 {
                    Ok(au)
                } else {
                    Ok(bu)
                }?;

            Ok(Expr::Prim(Unit(a + b, unit)))

        },
        (Expr::Prim(Str(a)), Expr::Prim(Str(b))) => {

            Ok(Expr::Prim(Str(a + &b)))

        },
        _ => Err(ErrorType::WrongTypeOfArguments{ message: "only numbers and strings can be added together".to_owned() })
    }

}

/// The "-" operator. Needs to handle unary minus or subtract.
pub fn Subtract(mut args: Vec<Expression>) -> PrimRes {

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

            let unit =
                if au.len() > 0 && bu.len() > 0 && au != bu {
                    Err(ErrorType::UnitMismatch)
                } else if au.len() > 0 {
                    Ok(au)
                } else {
                    Ok(bu)
                }?;

            Ok(Expr::Prim(Unit(a - b, unit)))

        } else {
            Err(ErrorType::WrongTypeOfArguments{ message: "only numbers can be minused from eachother".to_owned() })
        }


    } else {
        Err(ErrorType::WrongNumberOfArguments{ expected: 2, got: args.len() })
    }

}