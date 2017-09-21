use types::*;

pub fn rgba(args: &Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() < 3 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 3, got: args.len() });
    }
    if args.len() > 4 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 4, got: args.len() });
    }

    #[derive(PartialEq)]
    enum Mode { Percent, Plain, Unknown }

    let mut mode = Mode::Unknown;
    let mut get_number = |e: &EvaluatedExpression| {
        match e.expr {
            EvaluatedExpr::Unit(val, ref unit) => {
                if &*unit == "%" && mode != Mode::Plain {
                    mode = Mode::Percent;
                    Ok( (val/100f64).min(1.0).max(0.0) as f32 )
                } else if &*unit == "" && mode != Mode::Percent {
                    mode = Mode::Plain;
                    Ok( (val/255f64).min(1.0).max(0.0) as f32 )
                }  else {
                    Err(ErrorType::WrongTypeOfArguments{ message: "all values must be percentages or unitless numbers only".to_owned() })
                }
            }
            _ => Err(ErrorType::WrongTypeOfArguments{ message: "percentages or unitless numbers required".to_owned() })
        }
    };

    let red = get_number(&args[0])?;
    let green = get_number(&args[1])?;
    let blue = get_number(&args[2])?;

    let alpha = if args.len() > 3 {
        match args[3].expr {
            EvaluatedExpr::Unit(val, ref unit) => {
                if &*unit == "%" {
                    Ok( (val/100.0).min(1.0).max(0.0) as f32 )
                } else if &*unit == "" {
                    Ok( val.min(1.0).max(0.0) as f32 )
                }  else {
                    Err(ErrorType::WrongTypeOfArguments{ message: "alpha value should be a percentage or unitless number".to_owned() })
                }
            }
            _ => Err(ErrorType::WrongTypeOfArguments{ message: "alpha value should be a percentage or unitless number".to_owned() })
        }?
    } else {
        1f32
    };

    Ok(EvaluatedExpr::Colour(Colour::RGBA(red,green,blue,alpha)))

}