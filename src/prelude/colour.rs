use types::*;
use errors::*;

// red, green, blue, alpha
pub fn rgba<'a>(args: &'a Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() < 3 {
        return ApplicationError::WrongNumberOfArguments{ expected: 3, got: args.len() }.into();
    }
    if args.len() > 4 {
        return ApplicationError::WrongNumberOfArguments{ expected: 4, got: args.len() }.into();
    }

    let get_number = |idx, e: &'a EvaluatedExpression| {
        match *e.expr() {
            EvaluatedExpr::Unit(val, ref unit) => {
                Ok( (val, &**unit) )
            }
            _ => ApplicationError::WrongKindOfArguments{
                index: idx,
                expected: vec![Kind::Unit],
                got: e.expr().kind()
            }.into()
        }
    };

    let (mut red,   red_unit)   = get_number(0,&args[0])?;
    let (mut green, green_unit) = get_number(1,&args[1])?;
    let (mut blue,  blue_unit)  = get_number(2,&args[2])?;
    let (mut alpha, alpha_unit) = if args.len() > 3 { get_number(3, &args[3])? } else { (1.0,"") };

    if red_unit != blue_unit || blue_unit != green_unit {
        return ApplicationError::UnitMismatch.into();
    }
    if red_unit != "%" && red_unit != "" {
        return ApplicationError::WrongUnitOfArguments{ index: 0, expected: vec!["%".to_owned(), "".to_owned()], got: red_unit.to_owned() }.into()
    }
    if alpha_unit != "%" && alpha_unit != "" {
        return ApplicationError::WrongUnitOfArguments{ index: 3, expected: vec!["%".to_owned(), "".to_owned()], got: alpha_unit.to_owned() }.into()
    }

    if red_unit == "" {
        red   = red   / 255.0;
        green = green / 255.0;
        blue  = blue  / 255.0;
    } else {
        red   = red   / 100.0;
        green = green / 100.0;
        blue  = blue  / 100.0;
    }

    if alpha_unit == "%" {
        alpha = alpha / 100.0;
    }

    Ok(EvaluatedExpr::Colour(Colour::RGBA(red, green, blue, alpha)))

}

// hue,saturation,lightness,alpha
pub fn hsla<'a>(args: &'a Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() < 3 {
        return ApplicationError::WrongNumberOfArguments{ expected: 3, got: args.len() }.into();
    }
    if args.len() > 4 {
        return ApplicationError::WrongNumberOfArguments{ expected: 4, got: args.len() }.into();
    }

    let get_number = |idx, e: &'a EvaluatedExpression| {
        match *e.expr() {
            EvaluatedExpr::Unit(val, ref unit) => {
                Ok( (val, &**unit) )
            }
            _ => ApplicationError::WrongKindOfArguments{ index: idx, expected: vec![Kind::Unit], got: e.expr().kind() }.into()
        }
    };

    let (hue,   hue_unit)   = get_number(0, &args[0])?;
    let (sat,   sat_unit)   = get_number(1, &args[1])?;
    let (light, light_unit) = get_number(2, &args[2])?;
    let (mut alpha, alpha_unit) = if args.len() > 3 { get_number(3, &args[3])? } else { (1.0,"") };

    if hue_unit != "" && hue_unit != "deg" {
        return ApplicationError::WrongUnitOfArguments{ index: 0, expected: vec!["".to_owned(), "deg".to_owned()], got: hue_unit.to_owned() }.into()
    }
    if sat_unit != "%" {
        return ApplicationError::WrongUnitOfArguments{ index: 1, expected: vec!["%".to_owned()], got: sat_unit.to_owned() }.into()
    }
    if light_unit != "%" {
        return ApplicationError::WrongUnitOfArguments{ index: 2, expected: vec!["%".to_owned()], got: light_unit.to_owned() }.into()
    }
    if alpha_unit != "%" && alpha_unit != "" {
        return ApplicationError::WrongUnitOfArguments{ index: 3, expected: vec!["".to_owned(), "%".to_owned()], got: alpha_unit.to_owned() }.into()
    }
    if alpha_unit == "%" {
        alpha = alpha / 100.0;
    }

    Ok(EvaluatedExpr::Colour(Colour::HSLA(hue, sat/100.0, light/100.0, alpha)))

}