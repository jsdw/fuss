use types::{Expr, Block, CSSEntry, Primitive};
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

type MyErr = MyError<parsers::Error<char>>;
type MyResult<'a,T> = ParseResult<&'a str,T,MyErr>;

const VAR_PREFIX: char = '$';

fn tokens<'a>(i: &'a str, toks: &str) -> MyResult<'a,String> {
    let chars = toks.chars().collect::<Vec<char>>();
    string(i, &chars)
        .map(|s| s.to_owned())
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
        let expr = paren_expr()
               <|> primitive_expr()
               <|> if_then_else_expr()
               <|> function_declaration_expr()
               <|> application_expr()
               <|> variable_name_expr();
        ret expr
    }
}

fn primitive_expr(i: &str) -> MyResult<Expr> {
    parse!{i;
        let val = primitive_string()
              <|> primitive_bool()
              <|> primitive_unit();
        ret Expr::Prim(val)
    }
}

fn primitive_string(i: &str) -> MyResult<Primitive> {
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
        ret Primitive::Str(out.into_iter().collect())
    }
}

fn primitive_bool(i: &str) -> MyResult<Primitive> {
    parse!{i;
            (tokens("true") >> ret Primitive::Bool(true)) <|> (tokens("false") >> ret Primitive::Bool(false))
    }
}

fn primitive_unit(i: &str) -> MyResult<Primitive> {
    let is_unit = |c| c == '%' || (c >= 'a' && c <= 'z');
    parse!{i;
        let num = number();
        let unit = take_while(is_unit);
        ret Primitive::Unit(num, unit.to_owned())
    }
}

fn number(i: &str) -> MyResult<f64> {
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

fn if_then_else_expr(i: &str) -> MyResult<Expr> {
    parse!{i;
            tokens("if");
            skip_spaces();
        let cond = expr();
            skip_spaces();
            tokens("then");
            skip_spaces();
        let then = expr();
            skip_spaces();
            tokens("else");
            skip_spaces();
        let otherwise = expr();
        ret Expr::If{ cond:Box::new(cond), then:Box::new(then), otherwise:Box::new(otherwise) }
    }
}

fn variable_name_expr(i: &str) -> MyResult<Expr> {
    variable_string(i).map(Expr::Var)
}

fn variable_string(i: &str) -> MyResult<String> {
    parse!{i;
        token(VAR_PREFIX);
        let name = raw_variable_string();
        ret name
    }
}

fn raw_variable_string(i: &str) -> MyResult<String> {
    parse!{i;
        let name = take_while1(|c| c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_');
        ret name.to_owned()
    }
}

fn function_arg_sep(i: &str) -> MyResult<()> {
    parse!{i;
        skip_spaces();
        token(',');
        skip_spaces();
        ret ()
    }
}

fn function_declaration_expr(i: &str) -> MyResult<Expr> {
    parse!{i;
            token('(');
            skip_spaces();
        let vars = sep_by(variable_string, function_arg_sep);
            skip_spaces();
            token(')');
            skip_spaces();
            tokens("=>");
            skip_spaces();
        let expr = expr();
        ret Expr::Func{ inputs:vars, output: Box::new(expr) }
    }
}

fn paren_expr(i: &str) -> MyResult<Expr> {
    parse!{i;
            token('(');
            skip_spaces();
        let expr = expr();
            skip_spaces();
            token(')');
        ret expr
    }
}

fn prefix_application_expr(i: &str) -> MyResult<Expr> {
    parse!{i;
        let left = variable_name_expr() <|> paren_expr();
            skip_spaces();
            token('(');
            skip_spaces();
        let args = sep_by(expr, function_arg_sep);
            skip_spaces();
            token(')');
        ret Expr::App{ expr: Box::new(left), args: args }
    }
}

fn unary_application_expr(i: &str) -> MyResult<Expr> {
    parse!{i;
        let tok = token('!') <|> token('-');
        let arg = prefix_application_expr() <|> primitive_expr() <|> variable_name_expr() <|> paren_expr();
        ret Expr::App{ expr: Box::new(Expr::Var(tok.to_string())), args: vec![arg] }
    }
}

fn infix_application_expr(i: &str) -> MyResult<Expr> {

    let mut exprs = vec![];
    let mut ops = vec![];

    let get_expr = parser!{
        let expr = variable_name_expr() <|> unary_application_expr() <|> prefix_application_expr() <|> paren_expr();
        ret @ Expr,MyErr : expr
    };
    let get_op = parser!{
            skip_spaces();
        let op = take_while1(|c| {
                c == '+' || c == '-' || c == '*' || c == '/' || c == '<' || c == '=' || c == '>' || c == '^'
            });
            skip_spaces();
        ret @ String,MyErr : op.to_owned()
    };

    let parser: MyResult<Vec<()>> = sep_by1(i,
        |i| get_expr(i).map(|e| { exprs.push(e); }),
        |i| get_op(i).map(|op| { ops.push(op); })
    );

    parser.map(|_| Expr::Var("hi".to_owned()))
    //@TODO Finish this: decide which order to build expr from ops in and do it.
    // refactor to avoid sep_by accumulating vector
}

fn application_expr(i: &str) -> MyResult<Expr> {
    parse!{i;
        let res = prefix_application_expr()
              <|> unary_application_expr();
        ret res
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;

    fn s(s: &str) -> String {
        s.to_owned()
    }

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
    fn test_number() {

        let nums : Vec<(&str,f64)> = vec![
            ("-43.1", -43.1),
            ("0.0", 0.0),
            ("-0.0", 0.0),
            ("100", 100.0),
            ("99999.99999", 99999.99999)
        ];

        for (s,n) in nums {
            let res = parse_only_str(|i| number(i), s);
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
            assert_eq!(res, Ok( Primitive::Str(n.to_owned()) ) );
        }
    }

    #[test]
    fn test_function_declaration_expr() {

        let decls = vec![
           ( "($apPl_3s, $b2ananA) => true"
           , Expr::Func{ inputs: vec![s("apPl_3s"),s("b2ananA")], output: Box::new(Expr::Prim(Primitive::Bool(true))) }) ,
           ( "(\n$a\t,\n\t \n$b\n)\n\t\t=>\n\t\tfalse"
           , Expr::Func{ inputs: vec![s("a"),s("b")], output: Box::new(Expr::Prim(Primitive::Bool(false))) })
        ];

        for (s,n) in decls {
            let res = parse_only_str(|i| function_declaration_expr(i), s);
            assert_eq!(res, Ok(n));
        }
    }

    #[test]
    fn test_application_expr() {

        let var = |st| Box::new(Expr::Var(s(st)));

        let decls = vec![
            ( "$hello(2px, true)"
            , Expr::App{
                expr: var("hello"),
                args: vec![ Expr::Prim(Primitive::Unit(2.0,s("px"))), Expr::Prim(Primitive::Bool(true)) ]
            }) ,
            ( "!$hello"
            , Expr::App{
                expr: var("!"),
                args: vec![ Expr::Var(s("hello")) ]
            }) ,
            ( "!$hello()"
            , Expr::App{
                expr: var("!"),
                args: vec![ Expr::App{ expr: var("hello"), args: vec![] } ]
            }) ,
            ( "!$hello($a)"
            , Expr::App{
                expr: var("!"),
                args: vec![
                    Expr::App{
                        expr: var("hello"),
                        args: vec![ Expr::Var(s("a")) ]
                    }
                ]
            })
        ];

        for (s,n) in decls {
            let res = parse_only_str(|i| application_expr(i), s);
            assert_eq!(res, Ok(n));
        }
    }

}