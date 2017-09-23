
/// Colour
#[derive(PartialEq,Debug,Clone)]
pub struct Colour {
    red: f64,
    green: f64,
    blue: f64,
    alpha: f64
}

impl Colour {

    /// RGBA takes in red, green, blue and alpha values in the range [0,1]
    #[allow(non_snake_case)]
    pub fn RGBA(red: f64, green: f64, blue: f64, alpha: f64) -> Colour {
        Colour{
            red:   clamp(red,   0.0, 1.0),
            green: clamp(green, 0.0, 1.0),
            blue:  clamp(blue,  0.0, 1.0),
            alpha: clamp(alpha, 0.0, 1.0)
        }
    }

    /// A handy utility func to define standard colours.
    pub fn RGB_u8(red: u8, green: u8, blue: u8) -> Colour {
        Self::RGBA((red as f64)/255.0, (green as f64)/255.0, (blue as f64)/255.0, 1.0)
    }

    /// HSLA takes in a hue in degrees, and saturation, lightness and alpha in [0,1]
    #[allow(non_snake_case)]
    pub fn HSLA(mut hue: f64, mut saturation: f64, mut lightness: f64, alpha: f64) -> Colour {

        hue = (hue % 360.0) / 60.0;
        saturation = clamp(saturation,0.0,1.0);
        lightness = clamp(lightness,0.0,1.0);

        let (r,g,b) = hsl_to_rgb(hue,saturation,lightness);
        Self::RGBA(r,g,b,alpha)

    }

    pub fn red(&self)   -> f64 { self.red }
    pub fn green(&self) -> f64 { self.green }
    pub fn blue(&self)  -> f64 { self.blue }
    pub fn alpha(&self) -> f64 { self.alpha }

    pub fn red_u8(&self)   -> u8 { (self.red * 255.0).round() as u8 }
    pub fn green_u8(&self) -> u8 { (self.green * 255.0).round() as u8 }
    pub fn blue_u8(&self)  -> u8 { (self.blue * 255.0).round() as u8 }

}

fn clamp(val: f64, min: f64, max: f64) -> f64 {
    val.max(min).min(max)
}

/// Convert hsl to rgb
///
/// From https://www.w3.org/TR/css-color-4/#hsl-to-rgb
/// assumes hue is in [0,6) and s/l are in [0,1]
/// outputs rgb in range [0,1]
///
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

#[cfg(test)]
mod test {

    use super::*;

    fn rgb(r:i64,g:i64,b:i64) -> Colour {
        Colour::RGBA((r as f64)/255.0,(g as f64)/255.0,(b as f64)/255.0,1.0)
    }
    fn hsl(h:i64,s:i64,l:i64) -> Colour {
        Colour::HSLA((h as f64),(s as f64)/100.0,(l as f64)/100.0,1.0)
    }

    #[test]
    fn rgb_vs_hsl() {
        let cols = vec![
            ( rgb(0, 191, 255),   hsl(195, 100, 50) ),
            ( rgb(255, 0, 0),     hsl(360, 100, 50) ),
            ( rgb(0, 255, 255),   hsl(180, 100, 50) ),
            ( rgb(140, 115, 115), hsl(360, 10, 50)  ),
            ( rgb(0, 0, 0),       hsl(360, 100, 0)  )
        ];

        for (c1,c2) in cols {
            let c1_bits = ( c1.red_u8(), c1.green_u8(), c1.blue_u8() );
            let c2_bits = ( c2.red_u8(), c2.green_u8(), c2.blue_u8() );
            assert_eq!(c1_bits, c2_bits);
        }

    }

}