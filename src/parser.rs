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

    let mut push_all = |i| {
        let mut css_entries = vec![];
        let mut scope = HashMap::new();
        let res = sep_by1::<_,Vec<()>,_,_,_,_,_,_>(i, |i| or(i,
                |i| { css_scope_variable(i).map(|(varname,expr)| { scope.insert(varname,expr); }) },
                |i| { css_entry(i).map(|res| { css_entries.push(res); }) }), skip_spaces);

        res.map(|_| (css_entries,scope))
    };

    parse!{i;
        let selector = take_while(|c| c != '{');
            token('{');
            skip_spaces();
        let (css,scope) = push_all();
            skip_spaces();
            token('}');
        ret Block{ scope:scope, selector:selector.to_owned(), css:css }
    }
}

fn expr(i: &str) -> MyResult<Expr> {
    parse!{i;
        let expr = application_expr()
               <|> css_block_expr()
               <|> if_then_else_expr()
               <|> function_declaration_expr()
               <|> primitive_expr()
               <|> variable_name_expr()
               <|> paren_expr();
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

    use std::cell::RefCell;
    use std::cmp::Ordering;

    #[derive(Clone)]
    enum Tok { E(Expr), O(String) }
    let mut tokens: RefCell<Vec<Tok>> = RefCell::new(vec![]);

    let out = {
        fn is_op_char(c: char) -> bool {
            c == '+' || c == '-' || c == '*' || c == '/' || c == '<' || c == '=' || c == '>' || c == '^'
        };
        let mut push_expr = |i| -> MyResult<()> {
            let res = parse!{i;
                    unary_application_expr()
                <|> primitive_expr()
                <|> prefix_application_expr()
                <|> variable_name_expr()
                <|> paren_expr()
            };
            res.map(|e| { tokens.borrow_mut().push(Tok::E(e)); })
        };
        let mut push_op = |i| -> MyResult<()> {
            let res = parse!{i; skip_spaces(); let op = take_while1(is_op_char); skip_spaces(); ret op };
            res.map(|op| { tokens.borrow_mut().push(Tok::O(op.to_owned())); })
        };
        push_expr(i).then(|i| skip_many1(i, |i| push_op(i).then(|i| push_expr(i))))
    };

    // if we successfully parsed an infix expression, apply precedence rules to build correct Expr:
    out.map(|_| {

        let mut tokens = tokens.into_inner();
        assert!(tokens.len() >= 3, "expecting 3 or more tokens at least from infix parsing");
        assert!(tokens.len() % 2 == 1, "expecting an odd length of tokens from infix op parsing");

        while tokens.len() > 1 {

            // find next token to make expr from:
            let mut best_idx = 1;
            let mut best_prec = 0;
            let mut best_s = String::new();
            for (idx,tok) in tokens.iter().enumerate() {
                if let Tok::O(ref s) = *tok {
                    let this_prec = get_operator_precedence(s);
                    if this_prec > best_prec {
                        best_idx = idx;
                        best_prec = this_prec;
                        best_s = s.clone();
                    }
                }
            }

            // build expression from op and surrounding exprs:
            let left = tokens[best_idx-1].clone();
            let right = tokens[best_idx+1].clone();
            let exp = match (left,right) {
                (Tok::E(left), Tok::E(right)) => Expr::App{
                    expr: Box::new(Expr::Var(best_s)),
                    args: vec![left, right]
                },
                _ => panic!("Expected Exprs on either side of tok for infix parsing")
            };

            //reassign tokens, splicing in the new expr in place of the triplet.
            let mut new_tokens = Vec::with_capacity(tokens.len()-2);
            for (idx,tok) in tokens.into_iter().enumerate() {
                if idx == best_idx-1 {
                    new_tokens.push(Tok::E(exp.clone()));
                } else if idx != best_idx && idx != best_idx+1 {
                    new_tokens.push(tok);
                }
            }
            tokens = new_tokens;

        }

        match tokens.remove(0) {
            Tok::E(exp) => exp,
            _ => panic!("Expected final tok to be an Expr")
        }

    })

}

fn get_operator_precedence(op: &str) -> usize {
    match op {
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

        let res = parse_only_str(|i| application_expr(i), "{ $hello: 1; $another:2; }");
        assert_eq!(res, Ok(Expr::Var(s("hello"))));

    }

}
