use types::*;
use errors::*;
use pest::*;
use pest::iterators::*;
//use pest::inputs::*;
use pest::prec_climber::{PrecClimber, Operator, Assoc};
use std::collections::{HashMap};

#[cfg(debug_assertions)]
const _GRAMMAR: &'static str = include_str!("grammar.pest");

#[derive(Parser)]
#[grammar = "./parser/grammar.pest"]
struct MyGrammar;

type MyPair<'a> = Pair<'a,Rule>;

macro_rules! match_rules {
    ($pair:expr, $(let $var:ident = $rule:ident);+ $(;)* ) => {
        let mut iter = $pair.into_inner();
        $(
            let $var = iter.next().unwrap();
            assert!($var.as_rule() == Rule::$rule, "match_rules expected the rule '{}' but got '{:?}'", stringify!($rule), $var.as_rule());
        )*
    }
}
macro_rules! parser_rules {
    ( $( fn $rule:ident ($pair:ident : MyPair) -> $resTy:path $block:block )+ ) => (
        $(
            fn $rule($pair: MyPair) -> $resTy {
                let r = $pair.as_rule();
                assert!(r == Rule::$rule, "parser_rules function '{}' expected the rule '{:?}', but got '{:?}'", stringify!($rule), Rule::$rule, r);
                $block
            }
        )+
    )
}

pub fn parse(input: &str, context: &Context) -> Result<Expression,::errors::Error> {

    match MyGrammar::parse(Rule::file, input) {
        Ok(mut pairs) => {
            match_rules!{pairs.next().unwrap(),
                let block_pair = block_inner;
            }
            Ok(to_expression(
                block_pair.clone(),
                Expr::Block(block_inner(block_pair))
            ))
        },
        Err(e) => {
            // extract the pest error and turn it into a shape more consistent with our own errors
            // (basically, pull the location information out and keep the rest as-is):
            use pest::Error::*;
            match e {
                ParsingError{positives, negatives, pos} => {
                    let loc = pos.pos();
                    Err(err(SyntaxError::BadRule{ positives, negatives }, At::position(&context.path,loc,loc)))
                },
                CustomErrorPos{message, pos} => {
                    let loc = pos.pos();
                    Err(err(SyntaxError::Custom{ message }, At::position(&context.path,loc,loc)))
                },
                CustomErrorSpan{message, span} => {
                    let start = span.start();
                    let end = span.end();
                    Err(err(SyntaxError::Custom{ message }, At::position(&context.path,start,end)))
                }
            }
        }
    }

}

fn primary_expression(pair: MyPair) -> Expression {
    match pair.as_rule() {
        // recursive rules:
        Rule::expression => {
            expression(pair)
        },
        Rule::paren_expression | Rule::variable_accessor => {
            primary_expression(inner_pair(pair))
        },
        // expression types:
        Rule::function => {
            to_expression(pair.clone(), function(pair))
        },
        Rule::if_then_else => {
            to_expression(pair.clone(), if_then_else(pair))
        },
        Rule::prefix_application => {
            to_expression(pair.clone(), prefix_application(pair))
        },
        Rule::accessible => {
            to_expression(pair.clone(), accessible(pair))
        },
        Rule::accessible_css => {
            to_expression(pair.clone(), accessible_css(pair))
        },
        Rule::block => {
            to_expression(pair.clone(), block(pair))
        },
        Rule::variable => {
            to_expression(pair.clone(), Expr::Var(variable_name_string(pair), VarType::User))
        },
        Rule::naked_variable => {
            to_expression(pair.clone(), Expr::Var(variable_name_string(pair), VarType::Builtin))
        },
        // primitives:
        Rule::string => {
            match_rules!{pair.clone(),
                let s = string_contents
            };
            to_expression(pair, Expr::Str(escaped_string(s.as_str())))
        },
        Rule::unit => {
            match_rules!{pair.clone(),
                let n = number;
                let s = number_suffix;
            }
            to_expression(pair, Expr::Unit( n.as_str().parse::<f64>().unwrap(), s.as_str().to_owned() ))
        },
        Rule::boolean => {
            let b = match pair.clone().into_inner().next().unwrap().as_rule() {
                Rule::boolean_true => true,
                Rule::boolean_false => false,
                _ => unreachable!()
            };
            to_expression(pair.clone(), Expr::Bool(b))
        },
        Rule::colour => {
            to_expression(pair.clone(), colour(pair))
        },
        Rule::undefined => {
            to_expression(pair.clone(), Expr::Undefined)
        },
        r => {
            unreachable!("primary_expression did not expect to get a {:?} rule", r)
        }
    }
}

// function names == name of rule it expects to be given.
// this macro extends each function declared within to check that this is
// indeed the case.
parser_rules!{

    fn expression(pair: MyPair) -> Expression {

        // define our precedences:
        let climber = PrecClimber::new(vec![
            Operator::new(Rule::infix0_op, Assoc::Left),
            Operator::new(Rule::infix1_op, Assoc::Left),
            Operator::new(Rule::infix2_op, Assoc::Left),
            Operator::new(Rule::infix3_op, Assoc::Left),
            Operator::new(Rule::infix4_op, Assoc::Left),
            Operator::new(Rule::infix5_op, Assoc::Right)
        ]);

        // eval infix ops:
        let infix = |left: Expression, op: MyPair, right: Expression| -> Expression {
            Expression::with_position(
                left.start,
                right.end,
                Expr::Accessed{
                    expression: naked_variable_expression(op),
                    access: vec![
                        Accessor::Function{
                            location: Location::at(left.start, right.end),
                            args:vec![left,right]
                        }
                    ]
                }
            )
        };

        // climb:
        climber.climb(pair.into_inner(), primary_expression, infix)
    }

    fn function(pair: MyPair) -> Expr {
        let mut inner = pair.into_inner();
        let args_pair = inner.next().unwrap();
        let body_pair = inner.next().unwrap();

        let args = if Rule::function_args == args_pair.as_rule() {
            args_pair.into_inner().map(|arg| variable_name_string(arg)).collect()
        } else {
            vec![]
        };

        Expr::Func{
            inputs: args,
            output: expression(body_pair)
        }
    }

    fn if_then_else(pair: MyPair) -> Expr {
        match_rules!{pair,
            let cond = expression;
            let then = expression;
            let otherwise = expression;
        }
        Expr::If{
            cond: expression(cond),
            then: expression(then),
            otherwise: expression(otherwise)
        }
    }

    fn colour(pair: MyPair) -> Expr {
        match_rules!{pair,
            let hex = hex_value;
        }
        match Colour::from_hex_str(hex.as_str()) {
            None => Expr::Colour(Colour::transparent()),
            Some(col) => Expr::Colour(col)
        }
    }

    fn prefix_application(pair: MyPair) -> Expr {
        let span = pair.clone().into_span();
        match_rules!{pair,
            let sign = prefix_application_fn;
            let arg = prefix_application_arg;
        }
        Expr::Accessed{
            expression: naked_variable_expression(sign),
            access: vec![
                Accessor::Function{
                    args: vec![primary_expression(inner_pair(arg))],
                    location: Location::at(span.start(), span.end())
                }
            ]
        }
    }

    fn accessible(pair: MyPair) -> Expr {
        accessible_like(pair)
    }

    fn accessible_css(pair: MyPair) -> Expr {
        accessible_like(pair)
    }

    fn access(pair: MyPair) -> Vec<Accessor> {
        let mut accessors = vec![];
        for accessor in pair.into_inner() {
            match accessor.as_rule() {
                Rule::property_access => {
                    let span = accessor.clone().into_span();
                    match_rules!{accessor,
                        let name = variable_name;
                    }
                    accessors.push(Accessor::Property{
                        name: name.as_str().to_owned(),
                        location: Location::at(span.start()+1, span.end())
                    });
                },
                Rule::function_access => {
                    let span = accessor.clone().into_span();
                    accessors.push(Accessor::Function{
                        args: function_access(accessor),
                        location: Location::at(span.start(), span.end())
                    });
                },
                _ => {
                    unreachable!();
                }
            }
        }
        accessors
    }

    fn function_access(pair: MyPair) -> Vec<Expression> {
        let function_access_args = inner_pair(pair);
        let mut args = vec![];
        for expr in function_access_args.into_inner() {
            args.push(expression(expr))
        }
        args
    }

    fn block_inner(pair: MyPair) -> Block {
        let mut scope = HashMap::new();
        let mut css = vec![];

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::block_assignment => {
                    match_rules!{inner,
                        let var = block_variable_assign;
                        let expr = expression;
                    }
                    scope.insert(variable_name_string(var),expression(expr));
                },
                Rule::block_expression => {
                    let expression = primary_expression(inner_pair(inner));
                    css.push(CSSEntry::Expr( expression ));
                },
                Rule::block_css => {
                    let block = block_css(inner);
                    css.push(block);
                },
                _ => {
                    unreachable!()
                }
            }
        }

        Block {
            scope:scope,
            css:css,
            selector:vec![]
        }
    }

    fn block(pair: MyPair) -> Expr {
        match_rules!{pair,
            let selector = block_selector;
            let open = block_open;
            let inner = block_inner;
            let close = block_close;
        }

        let mut block = block_inner(inner);
        block.selector = block_selector(selector);
        Expr::Block(block)
    }

    fn block_selector(pair: MyPair) -> Vec<CSSBit> {
        let mut out = vec![];
        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::block_selector_chars => {
                    out.push(CSSBit::Str(inner.as_str().to_owned()));
                },
                Rule::block_interpolated_expression => {
                    out.push(CSSBit::Expr( expression(inner_pair(inner)) ));
                },
                _ => {
                    unreachable!();
                }
            }
        }
        out
    }

    fn block_css(pair: MyPair) -> CSSEntry {
        match_rules!{pair,
            let key = block_css_key;
            let val = block_css_value;
        }
        CSSEntry::KeyVal{
            key: block_css_key(key),
            val: block_css_value(val)
        }
    }

    fn block_css_key(pair: MyPair) -> Vec<CSSBit> {
        let mut out = vec![];
        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::block_css_key_chars => {
                    out.push(CSSBit::Str( inner.as_str().trim().to_owned() ));
                },
                Rule::block_interpolated_expression => {
                    out.push(CSSBit::Expr( expression(inner_pair(inner)) ));
                },
                _ => {
                    unreachable!();
                }
            }
        }
        out
    }

    fn block_css_value(pair: MyPair) -> Vec<CSSBit> {
        let mut out = vec![];
        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::block_interpolated_expression | Rule::variable_accessor => {
                    out.push(CSSBit::Expr( primary_expression(inner_pair(inner)) ));
                },
                Rule::block_css_value_chars => {
                    out.push(CSSBit::Str( inner.as_str().to_owned() ));
                },
                _ => {
                    unreachable!();
                }
            }
        }
        out
    }

}

//
// Helpers:
//

fn to_expression(pair: MyPair, expr: Expr) -> Expression {
    let span = pair.into_span();
    Expression::with_position(
        span.start(),
        span.end(),
        expr
    )
}

// escape a string - allow \\ and \" in a string; resulting in \ and ".
fn escaped_string(s: &str) -> String {
    let mut out = String::new();
    let mut is_escaped = false;
    for c in s.chars() {
        if !is_escaped && c == '\\' {
           is_escaped = true;
        } else {
            out.push(c);
            is_escaped = false;
        }
    }
    out
}

// look for a variable_name in the current tree, and return it as a Builtin Expression
fn naked_variable_expression(pair: MyPair) -> Expression {
    let span = pair.clone().into_span();
    let tok = pair.as_str().to_owned();
    Expression::with_position(
        span.start(),
        span.end(),
        Expr::Var(tok, VarType::Builtin)
    )
}

// Look for a variable_name rule in the pair we have been handed and return it, panicking
// if we don't find one.
fn variable_name_string(pair: MyPair) -> String {
    match pair.as_rule() {
        Rule::variable_name => {
            pair.as_str().to_owned()
        },
        _ => {
            match pair.into_inner().next() {
                None => panic!("variable_name rule not found"),
                Some(inner) => variable_name_string(inner)
            }
        }
    }
}

fn accessible_like(pair: MyPair) -> Expr {
    let mut inner = pair.into_inner();
    let expr = primary_expression(inner.next().unwrap());

    let accessors = if let Some(access_pair) = inner.next() {
        access(access_pair)
    } else {
        vec![]
    };

    if accessors.len() > 0 {
        Expr::Accessed{
            expression: expr,
            access: accessors
        }
    } else {
        expr.into_expr().expect("should be able to unwrap accessible expr")
    }
}

// go one level down into a pair, giving back the first child pair or panicking if
// one doesn't exist.
fn inner_pair(pair: MyPair) -> MyPair {
    pair.into_inner().next().unwrap()
}

#[cfg(test)]
mod test {

    use super::*;

    fn s(s: &str) -> String {
        s.to_owned()
    }

    fn e(e: Expr) -> Expression {
        Expression::with_position(0, 0, e)
    }

    fn var(name: &str) -> Expression {
        e(Expr::Var(s(name), VarType::User))
    }

    fn builtin(name: &str) -> Expression {
        e(Expr::Var(s(name), VarType::Builtin))
    }

    fn b(b: bool) -> Expression {
        e(Expr::Bool(b))
    }

    fn unit(n: f64, s: &str) -> Expression {
        e(Expr::Unit(n,s.to_owned()))
    }

    fn app(expression: Expression, args: Vec<Accessor>) -> Expression {
        e(Expr::Accessed{
            expression: expression,
            access: args
        })
    }

    fn a_prop(name: &str) -> Accessor {
        Accessor::Property{ name: s(name) }
    }

    fn a_fn(args: Vec<Expression>) -> Accessor {
        Accessor::Function{ args: args }
    }

    fn op(expression: Expression, args: Vec<Expression>) -> Expression {
        e(Expr::Accessed{
            expression: expression,
            access: vec![ a_fn(args) ]
        })
    }

    // make defining hash maps a little easier:
    macro_rules! hash_map {
        ( $($key:expr => $val:expr);+ ) => ({
            use std::collections::HashMap;
            let mut map = HashMap::new();
            $( map.insert($key, $val); )*
            map
        });
        ( ) => ({
            use std::collections::HashMap;
            HashMap::new()
        })
    }

    // test the parsing aspect:
    macro_rules! parse_test {
        ( $func:ident; $( $input:expr => $token:ident [] );+ $(;)* ) => (
            #[test]
            #[allow(unused)]
            fn $func() {
                use std::fmt::Write;
                let mut errors = String::new();
                $(
                    if let Err(e) = MyGrammar::parse_str(Rule::$token, $input) {
                        writeln!(&mut errors, "parse_test: Error parsing: \n{}\n\n{}", $input, e);
                    }
                )+
                if errors.len() > 0 {
                    assert!(false, "\n{}", errors);
                }
            }
        )
    }

    // test the processing aspect:
    macro_rules! process_test {
        ( $func:ident; $( $input:expr => $output:expr );+ $(;)* ) => (
            #[test]
            #[allow(unused)]
            fn $func() {
                use std::fmt::Write;
                let mut errors = String::new();

                $({
                    match MyGrammar::parse_str(Rule::expression, $input) {
                        Ok(mut pairs) => {

                            let res = ::std::panic::catch_unwind(::std::panic::AssertUnwindSafe(||
                                expression(pairs.next().expect("process_test: expression expected"))
                            ));
                            match res {
                                Ok(expr) => {
                                    if expr != $output {
                                        writeln!(&mut errors,
                                            "\nERROR: exprs did not match!\n - input: {:?}\n - expected: {:?}\n - got: {:?}",
                                            $input,
                                            $output,
                                            expr
                                        );
                                    }
                                },
                                Err(_) => {
                                    writeln!(&mut errors,
                                        "\nERROR: process panic!\n - input: {:?}\n - expected: {:?}",
                                        $input,
                                        $output
                                    );
                                }
                            };

                        },
                        Err(e) => {
                            writeln!(&mut errors, "process_test: Error parsing: \n{}\n\n{}", $input, e);
                        }
                    }
                })+

                if errors.len() > 0 {
                    assert!(false, "\n{}", errors);
                }
            }
        )
    }

    parse_test!{test_variable;
        "$hello" => variable[];
        "$2ello" => variable[];
        "$_hello" => variable[];
        "$hello" => expression[];
        "$2ello" => expression[];
        "$_hello" => expression[];
    }

    process_test!{test_variable_e;
        "$hello" => var("hello");
        "$hello.there" => app(var("hello"), vec![a_prop("there")]);
        "$hello.there.you" => app(var("hello"), vec![a_prop("there"), a_prop("you")]);
    }

    parse_test!{test_numeric;
        "20" => number[];
        "-20" => number[];
        "0.12" => number[];
        "100.0" => number[];
        "100.0px" => unit[];
        "100.0em" => unit[];
        "0%" => unit[];
        "100.0px" => expression[];
        "100.0em" => expression[];
        "0%" => expression[];
    }

    parse_test!{text_colour_hex;
        "#fef" => colour[];
        "#fef0" => colour[];
        "#123456" => colour[];
        "#12345678" => colour[];
        "#09afAF" => colour[];
    }

    process_test!{test_number_e;
        "10" => unit(10.0,"");
        "0" => unit(0.0,"");
        "0.12" => unit(0.12,"");
        "0%" => unit(0.0,"%");
        "100.0px" => unit(100.0,"px");
    }

    parse_test!{test_if_then_else;
        "if true then  true \nelse true" => if_then_else[];
        "if 20px then 10 else 100" => if_then_else[];
        "if true then true else true" => expression[];
        "if 20px then 10 else 100" => expression[];
    }

    parse_test!{test_strings;
        r#""hello there""# => string[];
        r#""hello \"hello\" there""# => string[];
        r#""hello there""# => expression[];
        r#""hello \"hello\" there""# => expression[];
    }

    process_test!{test_strings_e;
        r#""hello""# =>
            e(Expr::Str(s("hello")));
        r#""""# =>
            e(Expr::Str(s("")));
        r#""escaped \"lark\"""# =>
            e(Expr::Str(s(r#"escaped "lark""#)));
        r#"" \\lark\\ ""# =>
            e(Expr::Str(s(r#" \lark\ "#)));
    }

    parse_test!{test_functions;
        "() => true" => function[];
        "($lark, $another) => $lark" => function[];
    }

    process_test!{test_functions_e;
        "() => true" =>
            e(Expr::Func{
                inputs: vec![],
                output: b(true)
            });
        "($apPl_3s, $b2ananA) => true" =>
            e(Expr::Func{
                inputs: vec![s("apPl_3s"),s("b2ananA")],
                output: b(true)
            });
        "(\n$a\t,\n\t \n$b\n)\n\t\t=>\n\t\tfalse" =>
            e(Expr::Func{
                inputs: vec![s("a"),s("b")],
                output: b(false)
            });
    }

    parse_test!{test_operators;
        "20 + 40" => expression[];
        "20 / 40" => expression[];
        "20 * 40" => expression[];
        "20 - 40" => expression[];
    }

    process_test!{test_precedence_e;
        "1 + 2 + 3" =>
            op(
                builtin("+"),
                vec![
                    op(builtin("+"), vec![ unit(1.0,""), unit(2.0,"") ]),
                    unit(3.0,""),
                ]);
        "1 + 2 * 3" =>
            op(
                builtin("+"),
                vec![
                    unit(1.0,""),
                    op(builtin("*"), vec![ unit(2.0,""), unit(3.0,"") ]),
                ]);
        "1 + 2 * 3 * 4" =>
            op(
                builtin("+"),
                vec![
                    unit(1.0,""),
                    op(builtin("*"),
                        vec![
                            op(builtin("*"), vec![unit(2.0,""), unit(3.0,"")]),
                            unit(4.0,"")
                        ]),
                ]);
        "-1+2" =>
            op(
                builtin("+"),
                vec![
                    unit(-1.0,""),
                    unit(2.0,""),
                ]);
        "1 + -2 + 3" =>
            op(
                builtin("+"),
                vec![
                    op(builtin("+"), vec![ unit(1.0,""), unit(-2.0,"") ]),
                    unit(3.0,"")
                ]);
        "1 ^ 2 ^ 3 ^ 4" =>
            op(
                builtin("^"),
                vec![
                    unit(1.0,""),
                    op(builtin("^"),
                        vec![
                            unit(2.0,""),
                            op(builtin("^"),
                                vec![
                                    unit(3.0,""),
                                    unit(4.0,"")
                                ])
                        ])
                ]);
    }

    parse_test!{test_func_applications;
        "$hello(2px,true)" => expression[];
        "$hello(2px, true)" => expression[];
        "$hello(2px, true)" => accessible[];
        "!$hello" => expression[];
        "!($hello)" => expression[];
        "!$hello()" => expression[];
        "!($hello())" => expression[];
        "!$hello(2px)" => expression[];
        "!($hello(2px))" => expression[];
        "!($hello(2px, true))" => expression[];
        "!$hello(2px, true)" => expression[];
        "!$hello()" => prefix_application[];
        "!$hello(1px)" => prefix_application[];
        "!$hello(1px, true)" => prefix_application[];
        "!($hello)" => prefix_application[];
        "!($hello())" => prefix_application[];
        "!($hello(1px))" => prefix_application[];
        "!($hello(1px, true))" => prefix_application[];
        "$hello.there($a).b" => expression[];
    }

    process_test!{test_func_applications_e;
        "$hello(2px, true)" =>
            op(
                var("hello"),
                vec![ unit(2.0,"px"), b(true) ]
            );
        "!$hello" =>
            op(
                builtin("!"),
                vec![ var("hello") ]
            );
        "!$hello.there" =>
            op(
                builtin("!"),
                vec![ app( var("hello"), vec![ a_prop("there") ]) ]
            );
        "!$hello()" =>
            op(
                builtin("!"),
                vec![ app( var("hello"), vec![a_fn(vec![])] ) ]
            );
        "!$hello().b" =>
            op(
                builtin("!"),
                vec![ app( var("hello"), vec![a_fn(vec![]),a_prop("b")] ) ]
            );
        "!$hello($a)" =>
            op(
                builtin("!"),
                vec![ app( var("hello"), vec![ a_fn(vec![var("a")]) ] ) ]
            );
        "$hello.there($a)" =>
            app(
                var("hello"),
                vec![ a_prop("there"), a_fn(vec![var("a")]) ]
            );
        "builtin_thing($a)" =>
            app(
                builtin("builtin_thing"),
                vec![ a_fn(vec![var("a")]) ]
            );
        "builtin_thing(builtin_var)" =>
            app(
                builtin("builtin_thing"),
                vec![ a_fn(vec![builtin("builtin_var")]) ]
            );
        "$hello.there(1px).b" =>
            app(
                var("hello"),
                vec![ a_prop("there"), a_fn(vec![unit(1.0,"px")]), a_prop("b") ]
            );
        "$hello.there($a).b" =>
            app(
                var("hello"),
                vec![ a_prop("there"), a_fn(vec![var("a")]), a_prop("b") ]
            );
        "$hello.there($a).b.c($d,$e)" =>
            app(
                var("hello"),
                vec![ a_prop("there"), a_fn(vec![var("a")]), a_prop("b"), a_prop("c"), a_fn(vec![var("d"),var("e")]) ]
            );
        "!$hello.there($a)" =>
            op(
                builtin("!"),
                vec![ app(var("hello"), vec![ a_prop("there"), a_fn(vec![var("a")]) ]) ]
            );
        "!$hello.there($a) + 2px" =>
            op(
                builtin("+"),
                vec![
                    op(
                        builtin("!"),
                        vec![ app(var("hello"), vec![ a_prop("there"), a_fn(vec![var("a")]) ]) ]
                    ),
                    unit(2.0, "px")
                ]
            );
    }

    parse_test!{test_block_css;
        "hello: there;" => block_css[];
        "${ $hello }: there;" => block_css[];
        "${ $hello }: the${ 2px }re;" => block_css[];
        "${ $hello }: the${ 2px * 2 }re;" => block_css[];
        "hello: the${ 2px * 2 }re;" => block_css[];
    }

    parse_test!{test_block_assignment;
        "$hello: 2px;" => block_assignment[];
        "$hello: ($a, $b) => $a + $b;" => block_assignment[];
        "$hello: {};" => block_assignment[];
    }

    parse_test!{test_block_with_css_only;
        "{}" => block[];
        ".stuff {}" => block[];
        ".stuff {}" => expression[];
        "{ hello: there; }" => block[];
        "{ ${ $hello }: there; }" => block[];
        "{ ${ $hello }: the${ 2px }re; }" => block[];
        "{ border: $a.b.c }" => expression[];
    }

    process_test!{test_empty_block_e;
        ".some-class {
            /* empty */
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![],
                css: vec![]
            }));
    }

    process_test!{test_block_e;
        ".some-class {
            hi: $a px;
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![],
                css: vec![
                    CSSEntry::KeyVal{
                        key: vec![CSSBit::Str(s("hi"))],
                        val: vec![
                            CSSBit::Expr(var("a")),
                            CSSBit::Str(s(" px"))
                        ]
                    },
                ]
            }));
        ".some-class {
            $hello: 2px;
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![
                    s("hello") => unit(2f64,"px")
                ],
                css: vec![]
            }));
        ".some-class {
            $hello: ($a, $b) => true;
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![
                    s("hello") => e(Expr::Func{
                        inputs: vec![s("a"), s("b")],
                        output: e(Expr::Bool(true))
                    })
                ],
                css: vec![]
            }));
        ".some-class {
            $hello: { };
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![
                    s("hello") => e(Expr::Block(Block{
                        selector: vec![],
                        scope: hash_map![],
                        css: vec![]
                    }))
                ],
                css: vec![]
            }));
        ".some-class {
            $hello;
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![],
                css: vec![
                    CSSEntry::Expr(var("hello"))
                ]
            }));
        ".some-class {
            $hello: ($a, $b) => true;
            $hello;
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![
                    s("hello") => e(Expr::Func{
                        inputs: vec![s("a"), s("b")],
                        output: b(true)
                    })
                ],
                css: vec![
                    CSSEntry::Expr(var("hello"))
                ]
            }));
        "{
            border: 1px solid black;
            {
                lark: another thing hereee;
            }
        }" =>
            e(Expr::Block(Block{
                scope: hash_map![],
                selector: vec![],
                css: vec![
                    CSSEntry::KeyVal{
                        key: vec![CSSBit::Str(s("border"))],
                        val: vec![CSSBit::Str(s("1px solid black"))]
                    },
                    CSSEntry::Expr(e(Expr::Block(Block{
                        scope: hash_map![],
                        selector: vec![],
                        css: vec![
                            CSSEntry::KeyVal{
                                key: vec![CSSBit::Str(s("lark"))],
                                val: vec![CSSBit::Str(s("another thing hereee"))]
                            }
                        ]
                    })))
                ]
            }));
        "{ border: $o.width; }" =>
            e(Expr::Block(Block{
                scope: hash_map![],
                selector: vec![],
                css: vec![
                    CSSEntry::KeyVal{
                        key: vec![CSSBit::Str(s("border"))],
                        val: vec![CSSBit::Expr(app(
                            var("o"),
                            vec![ a_prop("width") ]
                        ))]
                    },
                ]
            }));
        "{ .hello { a:1; }; .another { b:1; }; }" =>
            e(Expr::Block(Block{
                scope: hash_map![],
                selector: vec![],
                css: vec![
                    CSSEntry::Expr(e(Expr::Block(Block{
                        scope: hash_map![],
                        selector: vec![CSSBit::Str(s(".hello "))],
                        css: vec![
                            CSSEntry::KeyVal{
                                key: vec![CSSBit::Str(s("a"))],
                                val: vec![CSSBit::Str(s("1"))]
                            }
                        ]
                    }))),
                    CSSEntry::Expr(e(Expr::Block(Block{
                        scope: hash_map![],
                        selector: vec![CSSBit::Str(s(".another "))],
                        css: vec![
                            CSSEntry::KeyVal{
                                key: vec![CSSBit::Str(s("b"))],
                                val: vec![CSSBit::Str(s("1"))]
                            }
                        ]
                    })))
                ]
            }));
    }

    parse_test!(test_file_expression;
        "$hello: 1px; $another: 2px;" => file[];
    );

    parse_test!(test_endings;
        "{ $a: 2 }" => block[]; //no semicolon or newline ending assignment OK at end of block
        "{ $a: 2; .hello.there {} }" => block[]; //block ending OK at end of block
        "{{{}};{}}" => block[]; //empty blocks are fine next to eachother.
    );

    parse_test!{test_block_more;
        "{ $hello: 1px; ${ $hello }: the${ 2px }re; }" => block[];
        ".some-selector value here::after { $hello: 1px; ${ $hello }: the${ 2px }re; }" => block[];
        ".some-selector value here::after { $hello: 1px; -hello-${ $world }: the${ 2px }re; }" => block[];
        ".some-class:not(:last-child) {}" => block[];
        ".some-class:not(:last-child) {
            $hello: ($a, $b) => $a + $b;
            $another: 2;
            $lark: { $sub1: 2px; };
            $more: $lark.sub1;
        }" => block[];
        ".some-class:not(:last-child) {
            $hello(2px, 5px);
        }" => block[];
        ".some-class:not(:last-child) {
            $hello;
        }" => block[];
        ".some-class:not(:last-child) {
            $hello: ($a, $b) => $a + $b;
            $another: 2;
            $lark: { $sub1: 2px; };

            $more: $lark.sub1;
            /* block /* nested!! */ comment */

            -moz-background-color: 1px solid blue;
            border-radius: 10px;
        }" => block[];
        ".some-class:not(:last-child) {
            /* a comment till end of line
             * lark
             * woop
             */
            & .a-sub-class .more.another, .sub-clas-two { color: red; $subThing: -2px + 4px; }
            -moz-background-color: 1px solid blue;
            border-radius: 10px;
        }" => block[];
        "{ $lark: {}; .stuff {}; }" => block[];
        "{ .stuff {}; $lark: {}; }" => block[];
        ".some-class:not(:last-child) {

            /* A comment! */
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

            $bye.then;

            & .a-sub-class .more.another, .sub-clas-two {
                $subThing: -2px + 4px;
                color: red;
            }

            -moz-background-color:
                            1px solid blue; /* comments everywhere... */
            border-radius: 10px;

            $more: $lark.sub1;
        }" => block[];
    }

}
