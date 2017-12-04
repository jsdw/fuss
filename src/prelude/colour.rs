use types::*;
use errors::*;

// red, green, blue, alpha
pub fn rgba<'a>(args: &'a Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() < 3 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 3, got: args.len() });
    }
    if args.len() > 4 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 4, got: args.len() });
    }

    let get_number = |e: &'a EvaluatedExpression| {
        match e.expr {
            EvaluatedExpr::Unit(val, ref unit) => {
                Ok( (val, &**unit) )
            }
            _ => Err(ErrorType::WrongTypeOfArguments{ message: "numbers required".to_owned() })
        }
    };

    let (mut red,   red_unit)   = get_number(&args[0])?;
    let (mut green, green_unit) = get_number(&args[1])?;
    let (mut blue,  blue_unit)  = get_number(&args[2])?;
    let (mut alpha, alpha_unit) = if args.len() > 3 { get_number(&args[3])? } else { (1.0,"") };

    if red_unit != blue_unit || blue_unit != green_unit || (red_unit != "%" && red_unit != "") {
        return Err(ErrorType::WrongTypeOfArguments{ message: "all values must be either percentages or unitless numbers".to_owned() })
    }
    if alpha_unit != "%" && alpha_unit != "" {
        return Err(ErrorType::WrongTypeOfArguments{ message: "alpha value is expected to be a percentage or unitless number".to_owned() })
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
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 3, got: args.len() });
    }
    if args.len() > 4 {
        return Err(ApplicationError::WrongNumberOfArguments{ expected: 4, got: args.len() });
    }

    let get_number = |e: &'a EvaluatedExpression| {
        match e.expr {
            EvaluatedExpr::Unit(val, ref unit) => {
                Ok( (val, &**unit) )
            }
            _ => Err(ErrorType::WrongTypeOfArguments{ message: "numbers required".to_owned() })
        }
    };

    let (hue,   hue_unit)   = get_number(&args[0])?;
    let (sat,   sat_unit)   = get_number(&args[1])?;
    let (light, light_unit) = get_number(&args[2])?;
    let (mut alpha, alpha_unit) = if args.len() > 3 { get_number(&args[3])? } else { (1.0,"") };

    if hue_unit != "" && hue_unit != "deg" {
        return Err(ErrorType::WrongTypeOfArguments{ message: "hue is expected to be unitless or degrees".to_owned() })
    }
    if sat_unit != "%" || light_unit != "%" {
        return Err(ErrorType::WrongTypeOfArguments{ message: "saturation and lightness should be percentages".to_owned() })
    }
    if alpha_unit != "%" && alpha_unit != "" {
        return Err(ErrorType::WrongTypeOfArguments{ message: "alpha value is expected to be a percentage or unitless number".to_owned() })
    }
    if alpha_unit == "%" {
        alpha = alpha / 100.0;
    }

    Ok(EvaluatedExpr::Colour(Colour::HSLA(hue, sat/100.0, light/100.0, alpha)))

}