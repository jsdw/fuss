use types::*;
use chomp::types::numbering::InputPosition;
use chomp::prelude::*;
use chomp::parsers;

/// parse takes in some input-like hting (eg a &str), and returns
/// output which has in it the remaining state of I, and either an
/// Expression or an error depending on success.
pub fn parse<I: Chars>(i: I) -> Output<I,Expression> {
    expr(i)
}

#[derive(PartialEq,Eq,Debug)]
pub enum Error {
    UnexpectedCharacter(parsers::Error<char>),
    UnknownError
}

impl From<parsers::Error<char>> for Error {
    fn from(e: parsers::Error<char>) -> Error {
        Error::UnexpectedCharacter(e)
    }
}
impl From<()> for Error {
    fn from(_: ()) -> Error {
        Error::UnknownError
    }
}

/// describe how to get a Position:
pub trait Positioned {
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

pub trait Chars: Input<Token=char> + Positioned {}
impl <I: Input<Token=char> + Positioned> Chars for I {}
pub type Output<I,O> = ParseResult<I,O,Error>;

/// convert a buffer of the sort Chars uses, and thus which
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
fn skip_sep_by<I: Input, E, R, F, N, V>(i: I, p: R, sep: F) -> ParseResult<I, (), E>
    where E: From<N>,
          R: FnMut(I) -> ParseResult<I, (), E>,
          F: FnMut(I) -> ParseResult<I, V, N> {
    parse!{i; skip_sep_by1(p,sep) <|> (ret ()) }
}

fn skip_tokens<I: Chars>(i: I, toks: &str) -> Output<I,I::Buffer> {
    let chars = toks.chars().collect::<Vec<char>>();
    string(i, &chars).map_err(Error::UnexpectedCharacter)
}

/// Get the current position of the input:
fn pos<I: Chars>(i: I) -> Output<I,Position> {
    let pos = i.get_position();
    i.ret(pos).map_err(|()| Error::UnknownError)
}

// fn skip_horizontal_spaces<I: Chars>(i: I) -> Output<I,()> {
//     skip_while(i, |c| c == '\t' || c == ' ').map_err(Error::UnexpectedCharacter)
// }

fn skip_comment_or_spaces<I: Chars>(i: I) -> Output<I,()> {
    fn skip_space<I: Chars>(i: I) -> Output<I,()> {
        parse!{i;
            token(' ') <|> token('\n') <|> token('\t');
            ret ()
        }
    }
    fn skip_simple_comment<I: Chars>(i: I) -> Output<I,()> {
        parse!{i;
            token('/');
            token('/');
            skip_while(|c| c != '\n');
            token('\n');
            ret ()
        }
    }
    fn skip_block_comment<I: Chars>(i: I) -> Output<I,()> {
        fn block_comment_end<I: Chars>(i: I) -> Output<I,()> {
            parse!{i;
                token('*');
                token('/');
                ret ()
            }
        }
        fn take_until_end<I: Chars>(i: I) -> Output<I,()> {
            let out: Output<I,Vec<_>> = parse!{
                many_till(i, |i| any(i).map_err(Error::UnexpectedCharacter), block_comment_end)
            };
            out.map(|_| ())
        }
        parse!{i;
            token('/');
            token('*');
            take_until_end();
            ret ()
        }
    }
    fn skip_one_of_the_above<I: Chars>(i: I) -> Output<I,()> {
        parse!{i;
            skip_space() <|> skip_block_comment() <|> skip_simple_comment();
        }
    }

    parse!{i;
        skip_many(skip_one_of_the_above)
    }
}

fn skip_spaces<I: Chars>(i: I) -> Output<I,()> {
    skip_comment_or_spaces(i)
}

fn css_keyval<I: Chars>(i: I) -> Output<I,CSSEntry> {
    parse!{i;
        let key = many1(css_key_bit);
            token(':');
            skip_spaces();
        let val = many1(css_value_bit);
            skip_spaces();
            token(';');
        ret CSSEntry::KeyVal{
            key: key,
            val: val
        }
    }
}

// TODO: allow css keys to contain ${} exprs.
fn css_key_bit<I: Chars>(i: I) -> Output<I,CSSBit> {
    parse!{i;
        css_key_string()
    }
}
fn css_key_string<I: Chars>(i: I) -> Output<I,CSSBit> {
    parse!{i;
        let s = take_while1(|c| c >= 'a' && c <= 'z' || c == '-');
        ret CSSBit::Str(as_string(s))
    }
}

fn css_value_bit<I: Chars>(i: I) -> Output<I,CSSBit> {
    parse!{i;
        css_expr_anything() <|> css_expr_var() <|> css_value_string()
    }
}
fn css_value_string<I: Chars>(i: I) -> Output<I,CSSBit> {
    parse!{i;
        let s = take_while1(|c| c != '$' && c != ';');
        ret CSSBit::Str(as_string(s))
    }
}

// TODO: allow selectors to contain ${} exprs.
fn css_selector_bit<I: Chars>(i: I) -> Output<I,CSSBit> {
    parse!{i;
        css_selector_string()
    }
}
fn css_selector_string<I: Chars>(i: I) -> Output<I,CSSBit> {
    parse!{i;
        let s = take_while1(|c| c != ';' && c != '{');
        ret CSSBit::Str(as_string(s))
    }
}

fn css_expr_var<I: Chars>(i: I) -> Output<I,CSSBit> {
    parse!{i;
        let expr = variable_name_expr();
        ret CSSBit::Expr(expr);
    }
}
fn css_expr_anything<I: Chars>(i: I) -> Output<I,CSSBit> {
    parse!{i;
            token('$');
            token('{');
            skip_spaces();
        let expr = expr();
            skip_spaces();
            token('}');
        ret CSSBit::Expr(expr)
    }
}

fn css_expr<I: Chars>(i: I) -> Output<I,CSSEntry> {
    parse!{i;
        let e = expr();
            skip_spaces();
            token(';');
        ret CSSEntry::Expr(e)
    }
}

fn css_entry<I: Chars>(i: I) -> Output<I,CSSEntry> {
    parse!{i;
        let entry = css_keyval()
                // allow css blocks to not need a semicolon after:
                <|> (i -> css_block_expr(i).map(CSSEntry::Expr))
                // css expressions need to end with a semi-colon:
                <|> css_expr();
        ret entry
    }
}

fn css_scope_variable<I: Chars>(i: I) -> Output<I,(String,Expression)> {
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

fn css_block<I: Chars>(i: I) -> Output<I,Block> {

    use std::collections::HashMap;

    let push_all = |i| {

        let mut css_entries = vec![];
        let mut scope = HashMap::new();
        let res = skip_sep_by(i, |i| or(i,
                |i| { css_scope_variable(i).map(|(varname,expr)| { scope.insert(varname,expr); }) },
                |i| { css_entry(i).map(|res| { css_entries.push(res); }) }), skip_spaces);

        res.map(|_| (css_entries,scope))
    };

    parse!{i;
        let selector = many(css_selector_bit);
            token('{');
            skip_spaces();
        let (css,scope) = push_all();
            skip_spaces();
            token('}');
        ret Block{ scope:scope, selector:selector, css:css }
    }
}

fn expr<I: Chars>(i: I) -> Output<I,Expression> {
    parse!{i;
        let expr = application_expr() // this will try parsing a bunch of exprs via infix_application_expr, so no need to try again here.
               <|> css_block_expr()
               <|> if_then_else_expr()
               <|> function_declaration_expr();
        ret expr
    }
}

fn css_block_expr<I: Chars>(i: I) -> Output<I,Expression> {
    parse!{i;
        let start_pos = pos();
        let block = css_block();
        let end_pos = pos();
        ret Expression{ start:start_pos, end:end_pos, expr:Expr::Block(block) }
    }
}

fn primitive_expr<I: Chars>(i: I) -> Output<I,Expression> {
    parse!{i;
        let start_pos = pos();
        let val = primitive_string()
              <|> primitive_bool()
              <|> primitive_unit();
        let end_pos = pos();
        ret Expression{ start:start_pos, end:end_pos, expr:Expr::Prim(val) }
    }
}

fn primitive_string<I: Chars>(i: I) -> Output<I,Primitive> {
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

fn primitive_bool<I: Chars>(i: I) -> Output<I,Primitive> {
    parse!{i;
            (skip_tokens("true") >> ret Primitive::Bool(true)) <|> (skip_tokens("false") >> ret Primitive::Bool(false))
    }
}

fn primitive_unit<I: Chars>(i: I) -> Output<I,Primitive> {
    let is_unit = |c| c == '%' || (c >= 'a' && c <= 'z');
    parse!{i;
        let num = number();
        let unit = take_while(is_unit);
        ret Primitive::Unit(num, as_string(unit).to_owned())
    }
}

fn number<I: Chars>(i: I) -> Output<I,f64> {
    let digits = |i| take_while1(i, |c| c >= '0' && c <= '9').map(as_string);
    let is_neg = |i| option(i, |i| token(i,'-').map(|_| true), false);

    let res : Output<I,(bool,String)> = parse!{i;
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

fn if_then_else_expr<I: Chars>(i: I) -> Output<I,Expression> {
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

fn variable_name_expr<I: Chars>(i: I) -> Output<I,Expression> {
    parse!{i;
        let start_pos = pos();
        token(VAR_PREFIX);
        let vars = sep_by1(raw_variable_string, |s| token(s,'.'));
        let end_pos = pos();
        ret {
            let mut vars: Vec<String> = vars;
            let first = vars.remove(0);
            Expression{
                start:start_pos,
                end:end_pos,
                expr:Expr::Var(first, vars)
            }
        }
    }
}

fn variable_string<I: Chars>(i: I) -> Output<I,String> {
    parse!{i;
        token(VAR_PREFIX);
        let name = raw_variable_string();
        ret name
    }
}

fn raw_variable_string<I: Chars>(i: I) -> Output<I,String> {
    parse!{i;
        let name = take_while1(|c| c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_');
        ret as_string(name)
    }
}

fn function_arg_sep<I: Chars>(i: I) -> Output<I,()> {
    parse!{i;
        skip_spaces();
        token(',');
        skip_spaces();
        ret ()
    }
}

fn function_declaration_expr<I: Chars>(i: I) -> Output<I,Expression> {
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
            expr:Expr::Func{ inputs:vars, output:Box::new(expr), scope:Scope::new() }
        }
    }
}

fn paren_expr<I: Chars>(i: I) -> Output<I,Expression> {
    parse!{i;
            token('(');
            skip_spaces();
        let expr = expr();
            skip_spaces();
            token(')');
        ret expr
    }
}

fn prefix_application_expr<I: Chars>(i: I) -> Output<I,Expression> {
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

fn unary_application_expr<I: Chars>(i: I) -> Output<I,Expression> {
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
                    expr:Expr::Var(tok.to_string(), vec![])
                }),
                args: vec![arg]
            }
        }
    }
}

fn infix_application_expr<I: Chars>(i: I) -> Output<I,Expression> {

    let mut ops = vec![];
    let mut exprs = vec![];
    let op_chars = ['^','*','-','+','=','<','>','&','|','/'];

    let out = {
        let is_op_char = |c: char| -> bool {
            (&op_chars).iter().find(|&a| *a == c).is_some()
        };
        let push_expr = |i: I| -> Output<I,()> {
            let res = parse!{i;
                    unary_application_expr()
                <|> primitive_expr()
                <|> prefix_application_expr()
                <|> variable_name_expr()
                <|> paren_expr()
            };
            res.map(|e| { exprs.push(e); })
        };
        let push_op = |i: I| -> Output<I,()> {
            let res = parse!{i;
                    skip_spaces();
                let start_pos = pos();
                let op = take_while1(&is_op_char);
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
                        expr: Expr::Var(op,vec![])
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
        "!" => 10,
        "^" => 8,
        "*" | "/" => 7,
        "+" | "-" => 6,
        "==" | "!=" | "<" | "<=" | ">" | ">=" => 4,
        "&&" => 3,
        "||" => 2,
        _ => 5
    }
}

fn application_expr<I: Chars>(i: I) -> Output<I,Expression> {
    parse!{i; infix_application_expr() } // this covers other applications
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

    fn var(name: &str) -> Box<Expression> {
        Box::new(e(Expr::Var(s(name),vec![])))
    }

    // parses the inner part of the macro; this can determine what
    // conditions we can test for.
    macro_rules! push_test_condition {
        // test that the thing on the left is parsed into the thing on the right:
        ($vec:ident using $test:expr; $input:expr => $output:expr; $($rest:tt)*) => {
            $vec.push(( parse_only_str($test,$input), Ok($output) ));
            push_test_condition!{$vec using $test; $($rest)* };
        };
        // test that the hting on the left and thing on the right both parse into the same thing:
        ($vec:ident using $test:expr; $input:expr , $output:expr; $($rest:tt)*) => {
            $vec.push(( parse_only_str($test,$input), parse_only_str($test,$output) ));
            push_test_condition!{$vec using $test; $($rest)* };
        };
        ($vec:ident using $test:expr;) => ( () )
    }

    // parse the outer part of the test macro.
    macro_rules! parse_test {
        ($i:ident using $test:expr; $($rest:tt)+) => (
            #[test]
            fn $i() {
                let mut mapping = vec![];
                push_test_condition!{mapping using $test; $($rest)+ };
                for (input,output) in mapping {
                    assert!(input.is_ok(), "input is not Ok: {:?}", input);
                    assert!(output.is_ok(), "output is not Ok: {:?}", output);
                    assert_eq!(input, output);
                }
            }
        );
        ($i:ident; $($rest:tt)+ ) => (
            parse_test!{ $i using expr; $($rest)+ }
        )
    }

    parse_test!{test_css_keyval using css_keyval;
        "-hello-there: you(123,456 );" =>
            CSSEntry::KeyVal{ key: vec![ CSSBit::Str(s("-hello-there")) ], val: vec![ CSSBit::Str(s("you(123,456 )")) ] };
        "-hello-there:you;" =>
            CSSEntry::KeyVal{ key: vec![ CSSBit::Str(s("-hello-there")) ], val: vec![ CSSBit::Str(s("you")) ] };
        "-hello-there: rgb($hello, $b, 2px);" =>
            CSSEntry::KeyVal{
                key: vec![ CSSBit::Str(s("-hello-there")) ],
                val: vec![
                    CSSBit::Str(s("rgb(")),
                    CSSBit::Expr( e(Expr::Var(s("hello"),vec![])) ),
                    CSSBit::Str(s(", ")),
                    CSSBit::Expr( e(Expr::Var(s("b"),vec![])) ),
                    CSSBit::Str(s(", 2px)")),
                ]
            };
        "-hello-there: stuff${ $hello }stuff;" =>
            CSSEntry::KeyVal{
                key: vec![ CSSBit::Str(s("-hello-there")) ],
                val: vec![
                    CSSBit::Str(s("stuff")),
                    CSSBit::Expr( e(Expr::Var(s("hello"),vec![])) ),
                    CSSBit::Str(s("stuff"))
                ]
            };
    }

    #[test]
    fn test_not_css_keyval_number() {
        let res = parse_only_str(|i| css_keyval(i), "-hell2o-there:you;").map_err(|_| ());
        assert_eq!(res, Err(()));
    }

    parse_test!{test_number using number;
        "-43.1" => -43.1;
        "0.0" => 0.0;
        "-0.0" => 0.0;
        "100" => 100.0;
        "99999.99999" => 99999.99999;
    }

    parse_test!{test_primitive_string using primitive_string;
        r#""hello""# =>
            Primitive::Str(s("hello"));
        r#""he\l\lo""# =>
            Primitive::Str(s("hello"));
        r#""he\\llo""# =>
            Primitive::Str(s(r#"he\llo"#));
        r#"'he\\llo'"# =>
            Primitive::Str(s(r#"he\llo"#));
        r#""""# =>
            Primitive::Str(s(""));
        r#""escaped \"lark\"""# =>
            Primitive::Str(s(r#"escaped "lark""#));
    }

    parse_test!{test_function_declaration_expr using function_declaration_expr;
        "($apPl_3s, $b2ananA) => true" =>
            e(Expr::Func{
                inputs: vec![s("apPl_3s"),s("b2ananA")],
                output: Box::new(e(Expr::Prim(Primitive::Bool(true)))),
                scope: Scope::new()
            });
        "(\n$a\t,\n\t \n$b\n)\n\t\t=>\n\t\tfalse" =>
            e(Expr::Func{
                inputs: vec![s("a"),s("b")],
                output: Box::new(e(Expr::Prim(Primitive::Bool(false)))),
                scope: Scope::new()
            });
    }

    parse_test!{test_application_expr using application_expr;
        "$hello(2px, true)" =>
            e(Expr::App{
                expr: var("hello"),
                args: vec![ e(Expr::Prim(Primitive::Unit(2.0,s("px")))), e(Expr::Prim(Primitive::Bool(true))) ]
            });
        "!$hello" =>
            e(Expr::App{
                expr: var("!"),
                args: vec![ e(Expr::Var(s("hello"),vec![])) ]
            });
        "!$hello()" =>
            e(Expr::App{
                expr: var("!"),
                args: vec![ e(Expr::App{ expr: var("hello"), args: vec![] }) ]
            });
        "!$hello($a)" =>
            e(Expr::App{
                expr: var("!"),
                args: vec![
                    e(Expr::App{
                        expr: var("hello"),
                        args: vec![ e(Expr::Var(s("a"),vec![])) ]
                    })
                ]
            });
        "$hello.there($a)" =>
            e(Expr::App{
                expr: Box::new(e(Expr::Var(s("hello"),vec![s("there")]))),
                args: vec![ e(Expr::Var(s("a"),vec![])) ]
            });
        "!$hello.there($a)" =>
            e(Expr::App{
                expr: var("!"),
                args: vec![
                    e(Expr::App{
                        expr: Box::new(e(Expr::Var(s("hello"),vec![s("there")]))),
                        args: vec![ e(Expr::Var(s("a"),vec![])) ]
                    })
                ]
            });
        "!$hello.there($a) + 2px" =>
            e(Expr::App{
                expr: var("+"),
                args: vec![
                    e(Expr::App{
                        expr: var("!"),
                        args: vec![
                            e(Expr::App{
                                expr: Box::new(e(Expr::Var(s("hello"),vec![s("there")]))),
                                args: vec![ e(Expr::Var(s("a"),vec![])) ]
                            })
                        ]
                    }),
                    e(Expr::Prim(Primitive::Unit(2.0,s("px"))))
                ]
            });
    }

    parse_test!{test_precedence using expr;
        "1 + 2 + 3"         , "(1 + 2) + 3";
        "1 + 2 * 3 + 4"     , "1 + (2 * 3) + 4";
        "1 + 2 * 3 * 4 + 5" , "1 + (2 * 3 * 4) + 5";
        "1 + 2 * 3 * 4 + 5" , "(1 + ((2 * 3) * 4)) + 5";
        "1 * 2 / 3 * 4"     , "((1 * 2) / 3) * 4";
        "-1+2 + 3"          , "(-1) + 2 + 3";
        "1 + -2 +3"         , "1 + (-2) + 3";
    }

    parse_test!{test_empty_block using expr;
        ".some-class {
            /* empty */
        }"
        ,
        ".some-class {}";
    }

    parse_test!{test_variable_in_block using expr;
        ".some-class {
            $hello: 2px;
        }" ,
        ".some-class { $hello: 2px; }";
    }

    parse_test!{test_function_in_block using expr;
        ".some-class {
            $hello: ($a, $b) => $a + $b;
        }" ,
        ".some-class { $hello: ($a, $b) => $a + $b; }";
    }

    parse_test!{test_sub_block_in_block using expr;
        ".some-class {
            $hello: { };
        }" ,
        ".some-class { $hello: { }; }";
    }

    parse_test!{test_css_block using expr;
        ".some-class:not(:last-child) {

            // A comment!
            $hello: ($a, $b) => $a + $b;
            $another: 2;

            $lark: {
                $sub1: 2px; /* another comment! */
            };

            $hello(2px, 5px);
            {
                border: 1px solid black;
                {
                    lark: another thing hereee;
                }
            }

            if true then $hello else $bye;

            & .a-sub-class .more.another, .sub-clas-two {
                $subThing: -2px + 4px;
                color: red;
            }

            -moz-background-color:
                            1px solid blue; // comments everywhere...
            border-radius: 10px;

            $more: $lark.sub1;
        }"
        ,
        ".some-class:not(:last-child) {

            $hello: ($a, $b) => $a + $b;
            $another: 2;
            $lark: { $sub1: 2px; };
            $more: $lark.sub1;

            $hello(2px, 5px);
            { border: 1px solid black; { lark: another thing hereee; }}
            if true then $hello else $bye;

            & .a-sub-class .more.another, .sub-clas-two { color: red; $subThing: -2px + 4px; }
            -moz-background-color: 1px solid blue;
            border-radius: 10px;
        }";
    }

}
