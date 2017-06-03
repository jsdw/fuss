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

/// like sep_by1 except we ignore the result; this is useful for side effects, like
/// collecting items up that match, without accumulating a vector of matches we'll ignore.
fn skip_sep_by1<I: Input, E, R, F, N, V>(i: I, mut p: R, mut sep: F) -> ParseResult<I, (), E>
    where E: From<N>,
          R: FnMut(I) -> ParseResult<I, (), E>,
          F: FnMut(I) -> ParseResult<I, V, N> {
    p(i).then(|i| skip_many(i, |i| sep(i).then(&mut p)))
}

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
            token(':');
        let val = take_till(|c| c == ';');
            token(';');
        ret CSSEntry::KeyVal{ key: key.to_owned(), val: val.trim().to_owned() }
    }
}

fn css_expr(i: &str) -> MyResult<CSSEntry> {
    parse!{i;
        let e = expr();
            skip_spaces();
            token(';');
        ret CSSEntry::Expr(e)
    }
}

fn css_entry(i: &str) -> MyResult<CSSEntry> {
    parse!{i;
        let entry = css_keyval()
                // allow css blocks to not need a semicolon after:
                <|> (i -> css_block_expr(i).map(CSSEntry::Expr))
                // css expressions need to end with a semi-colon:
                <|> css_expr();
        ret entry
    }
}

fn css_scope_variable(i: &str) -> MyResult<(String,Expr)> {
    parse!{i;
        let name = variable_string();
            token(':');
            skip_spaces();
        let expr = expr();
            skip_spaces();
            token(';');
        ret (name.to_owned(),expr)
    }
}

fn css_block(i: &str) -> MyResult<Block> {

    use std::collections::HashMap;

    let push_all = |i| {

        let mut css_entries = vec![];
        let mut scope = HashMap::new();
        let res = skip_sep_by1(i, |i| or(i,
                |i| { css_scope_variable(i).map(|(varname,expr)| { scope.insert(varname,expr); }) },
                |i| { css_entry(i).map(|res| { css_entries.push(res); }) }), skip_spaces);

        res.map(|_| (css_entries,scope))
    };

    parse!{i;
        let selector = take_while(|c| c != ';' && c != '{'); // hone what a valid CSS selector can be to reduce chance of miss parsing.
            token('{');
            skip_spaces();
        let (css,scope) = push_all();
            skip_spaces();
            token('}');
        ret Block{ scope:scope, selector:selector.trim().to_owned(), css:css }
    }
}

fn expr(i: &str) -> MyResult<Expr> {
    parse!{i;
        let expr = application_expr() // this will try parsing a bunch of exprs via infix_application_expr, so no need to try again here.
               <|> css_block_expr()
               <|> if_then_else_expr()
               <|> function_declaration_expr();
        ret expr
    }
}

fn css_block_expr(i: &str) -> MyResult<Expr> {
    css_block(i).map(Expr::Block)
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

    let mut ops = vec![];
    let mut exprs = vec![];

    let out = {
        fn is_op_char(c: char) -> bool {
            c == '.' || c == '+' || c == '-' || c == '*' || c == '/' || c == '<' || c == '=' || c == '>' || c == '^'
        };
        let push_expr = |i| -> MyResult<()> {
            let res = parse!{i;
                    unary_application_expr()
                <|> primitive_expr()
                <|> prefix_application_expr()
                <|> variable_name_expr()
                <|> paren_expr()
            };
            res.map(|e| { exprs.push(e); })
        };
        let push_op = |i| -> MyResult<()> {
            let res = parse!{i;
                    skip_spaces();
                let op = take_while1(is_op_char);
                    skip_spaces();
                ret op
            };
            res.map(|op| { ops.push(op.to_owned()); })
        };
        skip_sep_by1(i, push_expr, push_op)
    };

    out.bind(|i, _| {

        // return the EXPR here if there aren't any ops, so that we don't have to
        // try parsing it again elsewhere:
        if exprs.len() == 1 {
            return i.ret(exprs.remove(0));
        }

        assert!(ops.len() == exprs.len() - 1, "Expecting one less op than expr for infix parsing");

        while exprs.len() > 1 {

            // find next idx to construct expr from
            let best_idx = ops.iter().enumerate().fold((1,0), |(best_idx,best_prec), (idx,op)|{
                let this_prec = get_operator_precedence(op);
                return if this_prec > best_prec { (idx,this_prec) } else { (best_idx,best_prec) }
            }).0;

            // build next expr:
            let new_expr = Expr::App{
                expr: Box::new(Expr::Var(ops[best_idx].clone())),
                args: vec![ exprs[best_idx].clone(), exprs[best_idx+1].clone() ]
            };

            // update vecs with new expr, removing just-used bits:
            ops.remove(best_idx);
            exprs.remove(best_idx+1);
            exprs[best_idx] = new_expr;

        }

        i.ret(exprs.remove(0))

    })

}

fn get_operator_precedence(op: &str) -> usize {
    match op {
        "." => 10,
        "^" => 8,
        "*" | "/" => 7,
        "+" | "-" => 6,
        "==" | "!=" | "<" | "<=" | ">" | ">=" => 4,
        "&&" => 3,
        "||" => 2,
        _ => 5
    }
}

fn application_expr(i: &str) -> MyResult<Expr> {
    parse!{i; infix_application_expr() <|> prefix_application_expr() <|> unary_application_expr() }
}

#[cfg(test)]
pub mod tests {

    use super::*;

    fn s(s: &str) -> String {
        s.to_owned()
    }

    #[test]
    fn test_css_keyval() {
        let res = parse_only_str(|i| css_keyval(i), "-hello-there: you(123,456 );");
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

    #[test]
    fn test_precedence() {

        let same = vec![
            ("1 + 2 + 3", "(1 + 2) + 3"),
            ("1 + 2 * 3 + 4", "1 + (2 * 3) + 4"),
            ("1 + 2 * 3 * 4 + 5", "1 + (2 * 3 * 4) + 5"),
            ("1 + 2 * 3 * 4 + 5", "(1 + ((2 * 3) * 4)) + 5"),
            ("1 * 2 / 3 * 4", "((1 * 2) / 3) * 4")
        ];

        let different = vec![
            ("1 + 2 + 3", "1 + (2 + 3)")
        ];

        for (a,b) in same {
            let res_a = parse_only_str(|i| expr(i), a);
            let res_b = parse_only_str(|i| expr(i), b);
            assert_eq!(res_a, res_b);
        }

        for (a,b) in different {
            let res_a = parse_only_str(|i| expr(i), a);
            let res_b = parse_only_str(|i| expr(i), b);
            assert_ne!(res_a, res_b);
        }

    }

    #[test]
    fn test_css_block() {

        let a = parse_only_str(|i| expr(i), "
            .some-class:not(:last-child) {

                $hello: ($a, $b) => $a + $b;
                $another: 2;

                $lark: {
                    $sub1: 2px;
                };

                $hello(2px, 5px);
                {
                    border: 1px solid black;
                    {
                        lark: another thing hereee;
                    }
                }

                if true then $hello else $bye;

                & .a-sub-class .more.another,
                .sub-clas-two {
                    $subThing: -2px + 4px;
                    color: red;
                }

                -moz-background-color:
                                1px solid blue;
                border-radius: 10px;

                $more: $lark.$sub1;
            }
        ");

        let b = parse_only_str(|i| expr(i), "
            .some-class:not(:last-child) {

                $hello: ($a, $b) => $a + $b;
                $another: 2;
                $lark: { $sub1: 2px; };
                $more: $lark.$sub1;

                $hello(2px, 5px);
                { border: 1px solid black; { lark: another thing hereee; }}
                if true then $hello else $bye;

                & .a-sub-class .more.another,
                .sub-clas-two { color: red; $subThing: -2px + 4px; }
                -moz-background-color: 1px solid blue;
                border-radius: 10px;
            }
        ");

        assert!(a.is_ok(), "Expecting valid Exprs to be parsed (a)");
        assert!(b.is_ok(), "Expecting valid Exprs to be parsed (b)");
        assert_eq!(a, b);

    }

}
