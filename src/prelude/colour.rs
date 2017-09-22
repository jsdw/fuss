use types::*;

// red, green, blue, alpha
pub fn rgba<'a>(args: &'a Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() < 3 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 3, got: args.len() });
    }
    if args.len() > 4 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 4, got: args.len() });
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

    let clamp = |n: f64| n.min(1.0).max(0.0);
    Ok(EvaluatedExpr::Colour(Colour::RGBA(clamp(red),clamp(green),clamp(blue),clamp(alpha))))

}

// hue,saturation,lightness,alpha
pub fn hsla<'a>(args: &'a Vec<EvaluatedExpression>, _context: &Context) -> PrimRes {

    if args.len() < 3 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 3, got: args.len() });
    }
    if args.len() > 4 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 4, got: args.len() });
    }

    let get_number = |e: &'a EvaluatedExpression| {
        match e.expr {
            EvaluatedExpr::Unit(val, ref unit) => {
                Ok( (val, &**unit) )
            }
            _ => Err(ErrorType::WrongTypeOfArguments{ message: "numbers required".to_owned() })
        }
    };

    let (mut hue,   hue_unit)   = get_number(&args[0])?;
    let (mut sat,   sat_unit)   = get_number(&args[1])?;
    let (mut light, light_unit) = get_number(&args[2])?;
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

    let clamp = |n: f64| n.min(1.0).max(0.0);

    hue   = ((hue % 360.0) / 60.0).min(60.0).max(0.0);
    sat   = clamp(sat / 100.0);
    light = clamp(light / 100.0);
    alpha = clamp(alpha);

    let (r,g,b) = hsl_to_rgb(hue,sat,light);
    Ok(EvaluatedExpr::Colour(Colour::RGBA(clamp(r),clamp(g),clamp(b),alpha)))

}

// From https://www.w3.org/TR/css-color-4/#hsl-to-rgb
// assumes hue is in [0,6) and s/l are in [0,1]
// outputs rgb in range [0,1]
//
fn hsl_to_rgb(h: f64, s: f64, l: f64) -> (f64,f64,f64) {
    let t2 = if l <= 0.5 { l * (s + 1.0) } else { l + s - (l * s) };
    let t1 = l * 2.0 - t2;
    ( hue_to_rgb(t1,t2,h+2.0), hue_to_rgb(t1,t2,h), hue_to_rgb(t1,t2,h-2.0) )
}

fn hue_to_rgb(t1: f64, t2: f64, mut hue: f64) -> f64 {
    if hue < 0.0 { hue += 6.0; }
    if hue >= 6.0 { hue -= 6.0; }

    if      hue < 1.0 { (t2 - t1) * hue + t1 }
    else if hue < 3.0 { t2 }
    else if hue < 4.0 { (t2 - t1) * (4.0 - hue) + t1 }
    else              { t1 }
}