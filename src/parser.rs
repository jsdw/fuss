use types::{Expression, Expr, Block, CSSEntry, Primitive, Position};
use chomp::types::numbering::InputPosition;
use chomp::prelude::*;
use chomp::parsers;

#[derive(PartialEq,Eq,Debug)]
enum MyError<I> {
    Err(I),
    UnknownError
}

impl<I> From<parsers::Error<I>> for MyError<parsers::Error<I>> {
    fn from(e: parsers::Error<I>) -> MyError<parsers::Error<I>> {
        MyError::Err(e)
    }
}
impl<I> From<()> for MyError<I> {
    fn from(_: ()) -> MyError<I> {
        MyError::UnknownError
    }
}

/// describe how to get a Position:
trait Positioned {
    fn get_position(&self) -> Position;
}
impl <I: Input<Token=char>> Positioned for InputPosition<I,Position> {
    fn get_position(&self) -> Position {
        self.position()
    }
}
impl<'a> Positioned for &'a str {
    fn get_position(&self) -> Position {
        Position::new()
    }
}

// macro_rules! parse_expr {
//     ($i:expr; $($t:tt)*) => ({

//         pos($i).bind(|i,start_pos| {
//             let res = parse!{i; $($t)* };
//             res.map(|expr| {
//                 (expr,start_pos)
//             })
//         }).bind(|i,(res,start_pos)| {

//             pos(i).map(|end_pos| {
//                 Expression{ start:start_pos, end:end_pos, expr:res}
//             })

//         })

//     })
// }

trait MyInput: Input<Token=char> + Positioned {}
impl <I: Input<Token=char> + Positioned> MyInput for I {}
type MyOutput<I,O> = ParseResult<I,O,MyErr>;
type MyErr = MyError<parsers::Error<char>>;

/// convert a buffer of the sort MyInput uses, and thus which
/// parsers like take_while1 return, into a String.
fn as_string<B: Buffer<Token=char>>(buf: B) -> String {
    let mut s = String::with_capacity(buf.len());

    buf.iterate(|c| s.push(c));
    s
}

const VAR_PREFIX: char = '$';

/// like sep_by1 except we ignore the result; this is useful for side effects, like
/// collecting items up that match, without accumulating a vector of matches we'll ignore.
fn skip_sep_by1<I: Input, E, R, F, N, V>(i: I, mut p: R, mut sep: F) -> ParseResult<I, (), E>
    where E: From<N>,
          R: FnMut(I) -> ParseResult<I, (), E>,
          F: FnMut(I) -> ParseResult<I, V, N> {
    p(i).then(|i| skip_many(i, |i| sep(i).then(&mut p)))
}

fn skip_tokens<I: MyInput>(i: I, toks: &str) -> MyOutput<I,I::Buffer> {
    let chars = toks.chars().collect::<Vec<char>>();
    string(i, &chars).map_err(MyError::Err)
}

/// Get the current position of the input:
fn pos<I: MyInput>(i: I) -> MyOutput<I,Position> {
    let pos = i.get_position();
    i.ret(pos).map_err(|()| MyError::UnknownError)
}

// fn skip_horizontal_spaces<I: MyInput>(i: I) -> MyOutput<I,()> {
//     skip_while(i, |c| c == '\t' || c == ' ').map_err(MyError::Err)
// }
fn skip_spaces<I: MyInput>(i: I) -> MyOutput<I,()> {
    skip_while(i, |c| c == '\t' || c == ' ' || c == '\n').map_err(MyError::Err)
}

fn css_key<I: MyInput>(i: I) -> MyOutput<I,String> {
    parse!{i;
        let v = take_while1(|c| c >= 'a' && c <= 'z' || c == '-');
        ret as_string(v)
    }
}

fn css_keyval<I: MyInput>(i: I) -> MyOutput<I,CSSEntry> {
    parse!{i;
        let key = css_key();
            token(':');
        let val = take_till(|c| c == ';');
            token(';');
        ret CSSEntry::KeyVal{ key: key.to_owned(), val: as_string(val).trim().to_owned() }
    }
}

fn css_expr<I: MyInput>(i: I) -> MyOutput<I,CSSEntry> {
    parse!{i;
        let e = expr();
            skip_spaces();
            token(';');
        ret CSSEntry::Expr(e)
    }
}

fn css_entry<I: MyInput>(i: I) -> MyOutput<I,CSSEntry> {
    parse!{i;
        let entry = css_keyval()
                // allow css blocks to not need a semicolon after:
                <|> (i -> css_block_expr(i).map(CSSEntry::Expr))
                // css expressions need to end with a semi-colon:
                <|> css_expr();
        ret entry
    }
}

fn css_scope_variable<I: MyInput>(i: I) -> MyOutput<I,(String,Expression)> {
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

fn css_block<I: MyInput>(i: I) -> MyOutput<I,Block> {

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
        ret Block{ scope:scope, selector:as_string(selector).trim().to_owned(), css:css }
    }
}

fn expr<I: MyInput>(i: I) -> MyOutput<I,Expression> {
    parse!{i;
        let expr = application_expr() // this will try parsing a bunch of exprs via infix_application_expr, so no need to try again here.
               <|> css_block_expr()
               <|> if_then_else_expr()
               <|> function_declaration_expr();
        ret expr
    }
}

fn css_block_expr<I: MyInput>(i: I) -> MyOutput<I,Expression> {
    parse!{i;
        let start_pos = pos();
        let block = css_block();
        let end_pos = pos();
        ret Expression{ start:start_pos, end:end_pos, expr:Expr::Block(block) }
    }
}

fn primitive_expr<I: MyInput>(i: I) -> MyOutput<I,Expression> {
    parse!{i;
        let start_pos = pos();
        let val = primitive_string()
              <|> primitive_bool()
              <|> primitive_unit();
        let end_pos = pos();
        ret Expression{ start:start_pos, end:end_pos, expr:Expr::Prim(val) }
    }
}

fn primitive_string<I: MyInput>(i: I) -> MyOutput<I,Primitive> {
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

fn primitive_bool<I: MyInput>(i: I) -> MyOutput<I,Primitive> {
    parse!{i;
            (skip_tokens("true") >> ret Primitive::Bool(true)) <|> (skip_tokens("false") >> ret Primitive::Bool(false))
    }
}

fn primitive_unit<I: MyInput>(i: I) -> MyOutput<I,Primitive> {
    let is_unit = |c| c == '%' || (c >= 'a' && c <= 'z');
    parse!{i;
        let num = number();
        let unit = take_while(is_unit);
        ret Primitive::Unit(num, as_string(unit).to_owned())
    }
}

fn number<I: MyInput>(i: I) -> MyOutput<I,f64> {
    let digits = |i| take_while1(i, |c| c >= '0' && c <= '9').map(as_string);
    let is_neg = |i| option(i, |i| token(i,'-').map(|_| true), false);

    let res : MyOutput<I,(bool,String)> = parse!{i;
        let minus = is_neg();
        let start = digits();
        let suffix = (i -> token(i,'.') >> digits()) <|> (ret "".to_owned());
        ret (minus, [start, suffix].join("."))
    };

    res.map(|(minus, num_str)| {
        let negate = if minus { -1f64 } else { 1f64 };
        let num = num_str.parse::<f64>().expect("primitive_number parser unexpected failure");
        num * negate
    })
}

fn if_then_else_expr<I: MyInput>(i: I) -> MyOutput<I,Expression> {
    parse!{i;
        let start_pos = pos();
            skip_tokens("if");
            skip_spaces();
        let cond = expr();
            skip_spaces();
            skip_tokens("then");
            skip_spaces();
        let then = expr();
            skip_spaces();
            skip_tokens("else");
            skip_spaces();
        let otherwise = expr();
        let end_pos = pos();
        ret Expression{
            start:start_pos,
            end:end_pos,
            expr:Expr::If{ cond:Box::new(cond), then:Box::new(then), otherwise:Box::new(otherwise) }
        }
    }
}

fn variable_name_expr<I: MyInput>(i: I) -> MyOutput<I,Expression> {
    parse!{i;
        let start_pos = pos();
        let var = variable_string();
        let end_pos = pos();
        ret Expression{
            start:start_pos,
            end:end_pos,
            expr:Expr::Var(var)
        }
    }
}

fn variable_string<I: MyInput>(i: I) -> MyOutput<I,String> {
    parse!{i;
        token(VAR_PREFIX);
        let name = raw_variable_string();
        ret name
    }
}

fn raw_variable_string<I: MyInput>(i: I) -> MyOutput<I,String> {
    parse!{i;
        let name = take_while1(|c| c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_');
        ret as_string(name)
    }
}

fn function_arg_sep<I: MyInput>(i: I) -> MyOutput<I,()> {
    parse!{i;
        skip_spaces();
        token(',');
        skip_spaces();
        ret ()
    }
}

fn function_declaration_expr<I: MyInput>(i: I) -> MyOutput<I,Expression> {
    parse!{i;
        let start_pos = pos();
            token('(');
            skip_spaces();
        let vars = sep_by(variable_string, function_arg_sep);
            skip_spaces();
            token(')');
            skip_spaces();
            skip_tokens("=>");
            skip_spaces();
        let expr = expr();
        let end_pos = pos();
        ret Expression{
            start:start_pos,
            end:end_pos,
            expr:Expr::Func{ inputs:vars, output: Box::new(expr) }
        }
    }
}

fn paren_expr<I: MyInput>(i: I) -> MyOutput<I,Expression> {
    parse!{i;
            token('(');
            skip_spaces();
        let expr = expr();
            skip_spaces();
            token(')');
        ret expr
    }
}

fn prefix_application_expr<I: MyInput>(i: I) -> MyOutput<I,Expression> {
    parse!{i;
        let start_pos = pos();
        let left = variable_name_expr() <|> paren_expr();
            skip_spaces();
            token('(');
            skip_spaces();
        let args = sep_by(expr, function_arg_sep);
            skip_spaces();
            token(')');
        let end_pos = pos();
        ret Expression{
            start:start_pos,
            end:end_pos,
            expr:Expr::App{ expr: Box::new(left), args: args }
        }
    }
}

fn unary_application_expr<I: MyInput>(i: I) -> MyOutput<I,Expression> {
    parse!{i;
        let start_pos = pos();
        let tok = token('!') <|> token('-');
        let end_pos_tok = pos();
        let arg = prefix_application_expr() <|> primitive_expr() <|> variable_name_expr() <|> paren_expr();
        let end_pos = pos();
        ret Expression{
            start:start_pos,
            end:end_pos,
            expr:Expr::App{
                expr: Box::new(Expression{
                    start:start_pos,
                    end:end_pos_tok,
                    expr:Expr::Var(tok.to_string())
                }),
                args: vec![arg]
            }
        }
    }
}

fn infix_application_expr<I: MyInput>(i: I) -> MyOutput<I,Expression> {

    let mut ops = vec![];
    let mut exprs = vec![];

    let out = {
        fn is_op_char(c: char) -> bool {
            c == '.' || c == '+' || c == '-' || c == '*' || c == '/' || c == '<' || c == '=' || c == '>' || c == '^'
        };
        let push_expr = |i: I| -> MyOutput<I,()> {
            let res = parse!{i;
                    unary_application_expr()
                <|> primitive_expr()
                <|> prefix_application_expr()
                <|> variable_name_expr()
                <|> paren_expr()
            };
            res.map(|e| { exprs.push(e); })
        };
        let push_op = |i: I| -> MyOutput<I,()> {
            let res = parse!{i;
                    skip_spaces();
                let start_pos = pos();
                let op = take_while1(is_op_char);
                let end_pos = pos();
                    skip_spaces();
                ret (as_string(op),start_pos,end_pos)
            };
            res.map(|op| { ops.push(op); })
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
                let this_prec = get_operator_precedence(&op.0);
                return if this_prec > best_prec { (idx,this_prec) } else { (best_idx,best_prec) }
            }).0;

            // build next expr:
            let first_arg = exprs[best_idx].clone();
            let second_arg = exprs[best_idx+1].clone();
            let (op,op_start,op_end) = ops[best_idx].clone();
            let new_expr = Expression{
                start: first_arg.start,
                end: second_arg.end,
                expr: Expr::App{
                    expr: Box::new(Expression{
                        start: op_start,
                        end: op_end,
                        expr: Expr::Var(op)
                    }),
                    args: vec![ first_arg, second_arg ]
                }
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

fn application_expr<I: MyInput>(i: I) -> MyOutput<I,Expression> {
    parse!{i; infix_application_expr() <|> prefix_application_expr() <|> unary_application_expr() }
}

#[cfg(test)]
pub mod tests {

    use super::*;

    fn s(s: &str) -> String {
        s.to_owned()
    }

    fn e(e: Expr) -> Expression {
        Expression{start:Position::new(), end:Position::new(), expr:e}
    }

    // parses the inner part of the macro; this can determine what
    // conditions we can test for.
    macro_rules! push_test_condition {
        ($vec:expr; $input:expr => $output:expr; $($rest:tt)*) => {
            $vec.push(($input,$output));
            push_test_condition!{$vec; $($rest)* };
        };
        ($vec:expr;) => ( () )
    }

    // parse the outer part of the test macro.
    macro_rules! parse_test {
        ($i:ident using $test:expr; $($rest:tt)+) => (
            #[test]
            fn $i() {
                let mut mapping = vec![];
                push_test_condition!{mapping; $($rest)+ };
                for (input,output) in mapping {
                    let res = parse_only_str(|i| $test(i), input);
                    assert_eq!(res, Ok(output));
                }
            }
        );
        ($i:ident; $($rest:tt)+ ) => (
            parse_test!{ $i using expr; $($rest)+ }
        )
    }

    parse_test!{hi using expr;
        "$hello" => e(Expr::Var(s("hello")));
        "4" => e(Expr::Prim(Primitive::Unit(4f64,s(""))));
    }

    parse_test!{test_css_keyval using css_keyval;
        "-hello-there: you(123,456 );" =>
            CSSEntry::KeyVal{ key: "-hello-there".to_owned(), val: "you(123,456 )".to_owned() };
        "-hello-there:you;" =>
            CSSEntry::KeyVal{ key: "-hello-there".to_owned(), val: "you".to_owned() };
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
           , Expr::Func{ inputs: vec![s("apPl_3s"),s("b2ananA")], output: Box::new(e(Expr::Prim(Primitive::Bool(true)))) }) ,
           ( "(\n$a\t,\n\t \n$b\n)\n\t\t=>\n\t\tfalse"
           , Expr::Func{ inputs: vec![s("a"),s("b")], output: Box::new(e(Expr::Prim(Primitive::Bool(false)))) })
        ];

        for (s,n) in decls {
            let res = parse_only_str(|i| function_declaration_expr(i), s).map(|e| e.expr);
            assert_eq!(res, Ok(n));
        }
    }

    #[test]
    fn test_application_expr() {

        let var = |st| Box::new(e(Expr::Var(s(st))));

        let decls = vec![
            ( "$hello(2px, true)"
            , Expr::App{
                expr: var("hello"),
                args: vec![ e(Expr::Prim(Primitive::Unit(2.0,s("px")))), e(Expr::Prim(Primitive::Bool(true))) ]
            }) ,
            ( "!$hello"
            , Expr::App{
                expr: var("!"),
                args: vec![ e(Expr::Var(s("hello"))) ]
            }) ,
            ( "!$hello()"
            , Expr::App{
                expr: var("!"),
                args: vec![ e(Expr::App{ expr: var("hello"), args: vec![] }) ]
            }) ,
            ( "!$hello($a)"
            , Expr::App{
                expr: var("!"),
                args: vec![
                    e(Expr::App{
                        expr: var("hello"),
                        args: vec![ e(Expr::Var(s("a"))) ]
                    })
                ]
            })
        ];

        for (s,n) in decls {
            let res = parse_only_str(|i| application_expr(i), s);
            assert_eq!(res, Ok(e(n)));
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
