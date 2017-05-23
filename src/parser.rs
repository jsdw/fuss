use types::{Expr, CSSEntry, Primitive};
use chomp::prelude::*;
use chomp::parsers;

#[derive(PartialEq,Eq,Debug)]
enum MyError<I> {
    Err(I)
}

impl<I> From<parsers::Error<I>> for MyError<parsers::Error<I>> {
    fn from(e: parsers::Error<I>) -> MyError<parsers::Error<I>> {
        MyError::Err(e)
    }
}

type MyResult<'a,T> = ParseResult<&'a str,T,MyError<parsers::Error<char>>>;

fn tokens<'a>(i: &'a str, toks: &str) -> MyResult<'a,()> {
    let chars = toks.chars().collect::<Vec<char>>();
    string(i, &chars)
        .map(|_| ())
        .map_err(MyError::Err)
}

fn skip_horizontal_spaces(i: &str) -> MyResult<()> {
    skip_while(i, |c| c == '\t' || c == ' ').map_err(MyError::Err)
}
fn skip_spaces(i: &str) -> MyResult<()> {
    skip_while(i, |c| c == '\t' || c == ' ' || c == '\n').map_err(MyError::Err)
}

fn css_key(i: &str) -> MyResult<String> {
    parse!{i;
        let v = take_while1(|c| c >= 'a' && c <= 'z' || c == '-');
        ret v.to_owned()
    }
}

fn css_keyval(i: &str) -> MyResult<CSSEntry> {
    parse!{i;
        let key = css_key();
            skip_horizontal_spaces();
            token(':');
            skip_horizontal_spaces();
        let val = take_till(|c| c == ';');
            token(';');
        ret CSSEntry::KeyVal{ key: key.to_owned(), val: val.to_owned() }
    }
}

fn css_entry(i: &str) -> MyResult<CSSEntry> {
    parse!{i;
        let entry = css_keyval()
                <|> (i -> expr(i).map(CSSEntry::Expr));
        ret entry
    }
}

fn css_block(i: &str) -> MyResult<Vec<CSSEntry>> {
    parse!{i;
            token('{');
            skip_spaces();
        let keyvals = sep_by1(css_entry, skip_spaces);
            skip_spaces();
            token('}');
        ret keyvals;
    }
}

fn expr(i: &str) -> MyResult<Expr> {
    parse!{i;
        let expr = (i -> primitive(i).map(Expr::Prim));
        ret expr
    }
}

fn primitive(i: &str) -> MyResult<Primitive> {
    parse!{i;
        let val = (i -> primitive_string(i).map(Primitive::Str))
              <|> (i -> primitive_bool(i).map(Primitive::Bool))
              <|> (i -> primitive_unit(i).map(|(n,u)| Primitive::Unit(n,u)));
        ret val
    }
}

fn primitive_string(i: &str) -> MyResult<String> {
    let mut is_escaped = false;
    let mut out = vec![];
    parse!{i;
        let delim = token('"') <|> token('\'');
            take_till(|c| {

                //found closing delim, not escaped, so end:
                if c == delim && !is_escaped {
                    return true;
                }

                //not escaped, escape char seen, so ignore but escape next:
                if !is_escaped && c == '\\' {
                    is_escaped = true;
                    return false;
                }

                //get char, converting it if it's escaped:
                let mut actual = c;
                if is_escaped {
                    is_escaped = false;
                    actual = match c {
                        'n'  => '\n',
                        't'  => '\t',
                        '\\' => '\\',
                        a    => a,
                    }
                }

                out.push(actual);
                return false;

            });
            token(delim);
        ret out.into_iter().collect()
    }
}

fn primitive_bool(i: &str) -> MyResult<bool> {
    parse!{i;
            (tokens("true") >> ret true) <|> (tokens("false") >> ret false)
    }
}

fn primitive_unit(i: &str) -> MyResult<(f64,String)> {
    let is_unit = |c| c == '%' || (c > 'a' || c < 'z');
    parse!{i;
        let num = primitive_number();
        let unit = take_while(is_unit);
        ret (num, unit.to_owned())
    }
}

fn primitive_number(i: &str) -> MyResult<f64> {
    let digits = |i| take_while1(i, |c| c >= '0' && c <= '9');
    let is_neg = |i| option(i, |i| token(i,'-').map(|_| true), false);

    let res : MyResult<(bool,String)> = parse!{i;
        let minus = is_neg();
        let start = digits();
        let suffix = (token('.') >> digits()) <|> (ret "");
        ret (minus, [start, suffix].join("."))
    };

    res.map(|(minus, num_str)| {
        let negate = if minus { -1f64 } else { 1f64 };
        let num = num_str.parse::<f64>().expect("primitive_number parser unexpected failure");
        num * negate
    })
}

#[cfg(test)]
pub mod tests {

    use super::*;

    #[test]
    fn test_css_key() {
        let res = parse_only_str(|i| css_key(i), "-hello-there : ");
        assert_eq!(res, Ok("-hello-there".to_owned()));
    }

    #[test]
    fn test_css_keyval() {
        let res = parse_only_str(|i| css_keyval(i), "-hello-there\t : you(123,456 );");
        assert_eq!(res, Ok(
            CSSEntry::KeyVal{ key: "-hello-there".to_owned(), val: "you(123,456 )".to_owned() }
        ));
    }

    #[test]
    fn test_css_keyval_compact() {
        let res = parse_only_str(|i| css_keyval(i), "-hello-there:you;");
        assert_eq!(res, Ok(
            CSSEntry::KeyVal{ key: "-hello-there".to_owned(), val: "you".to_owned() }
        ));
    }

    #[test]
    fn test_not_css_keyval_number() {
        let res = parse_only_str(|i| css_keyval(i), "-hell2o-there:you;").map_err(|_| ());
        assert_eq!(res, Err(()));
    }

    #[test]
    fn test_css_block() {
        let res = parse_only_str(|i| css_block(i), "{\n\t -hello-there\t : you; another: two;\n\n}");
        assert_eq!(res, Ok( vec![
            CSSEntry::KeyVal{ key: "-hello-there".to_owned(), val: "you".to_owned() },
            CSSEntry::KeyVal{ key: "another".to_owned(), val: "two".to_owned() }
        ]));
    }

    #[test]
    fn test_primitive_number() {

        let nums : Vec<(&str,f64)> = vec![
            ("-43.1", -43.1),
            ("0.0", 0.0),
            ("-0.0", 0.0),
            ("100", 100.0),
            ("99999.99999", 99999.99999)
        ];

        for (s,n) in nums {
            let res = parse_only_str(|i| primitive_number(i), s);
            assert_eq!(res, Ok(n));
        }
    }

    #[test]
    fn test_primitive_string() {

        let strings : Vec<(&str,&str)> = vec![
            (r#""hello""#, "hello"),
            (r#""he\l\lo""#, "hello"),
            (r#""he\\llo""#, r#"he\llo"#),
            (r#"'he\\llo'"#, r#"he\llo"#),
            (r#""""#, ""),
            (r#""escaped \"lark\"""#, r#"escaped "lark""#),
        ];

        for (s,n) in strings {
            let res = parse_only_str(|i| primitive_string(i), s);
            assert_eq!(res, Ok(n.to_owned()));
        }
    }

}