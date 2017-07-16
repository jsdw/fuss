use pest::prelude::*;
use types::*;
use std::collections::LinkedList;

impl_rdp! {
    grammar! {
        expression = {
            { block | string | if_then_else | function | boolean | unit | application | paren_expression | variable_accessor }
            infix0 = { ["||"] }
            infix1 = { ["&&"] }
            infix2 = { ["=="] | ["!="] | [">="] | ["<="] | [">"] | ["<"] }
            infix3 = { ["+"] | ["-"] }
            infix4 = { ["*"] | ["/"] }
            infix5 = {< ["^"] }
        }

        block = { block_selector ~ ["{"] ~ (block_assignment | block_css | block_expression | block )* ~ ["}"] }

            block_expression = { (block_interpolated_expression | variable) ~ [";"] }
            block_interpolated_expression = { ["${"] ~ expression ~ ["}"] }
            block_assignment = { block_variable_assign ~ expression ~ [";"] }
                block_variable_assign = @{ variable ~ [":"] }

            block_css = { block_css_key ~ [":"] ~ block_css_value ~ [";"] }
            block_selector = { ( block_interpolated_expression | block_selector_char )* }
                block_selector_char = _{ !(["$"] | ["{"] | [";"] | ["}"]) ~ any }

            block_css_key = { (block_interpolated_expression | block_css_key_char)+ }
                block_css_key_char = _{ ['a'..'z'] | ["-"] }
            block_css_value = { (block_interpolated_expression | variable | block_css_value_chars)+ }
                block_css_value_chars = _{ !(block_interpolated_expression | variable | [";"]) ~ any }

        application = { prefix_application | function_application }

            prefix_application = @{ prefix_application_fn ~ prefix_application_arg }
            prefix_application_fn = { ["-"] | ["!"] }
            prefix_application_arg = !@{ paren_expression | function_application | variable_accessor }

            function_application = { function_application_fn ~ ["("] ~ function_application_args ~ [")"] }
            function_application_fn = _{ variable_accessor | paren_expression }
            function_application_args = _{ ( function_application_arg ~ ([","] ~ function_application_arg)* )? }
            function_application_arg = { expression }

        paren_expression = _{ ["("] ~ expression ~ [")"] }
        variable = @{ ["$"] ~ variable_name }
        variable_accessor = { variable ~ ( ["."] ~ variable_name )* }
        if_then_else = { ["if"] ~ expression ~ ["then"] ~ expression ~ ["else"] ~ expression }

        function = { ["("] ~ function_args? ~ [")"] ~ ["=>"] ~ expression }
            function_args = _{ variable ~ ( [","] ~ variable )* }

        variable_name = { (['a'..'z'] | ['A'..'Z'] | ["_"] | ['0'..'9'])+ }
        boolean = { boolean_true | boolean_false }
            boolean_true = { ["true"] }
            boolean_false = { ["false"] }

        string  = @{ ["\""] ~ string_contents ~ ["\""] }
            string_contents = { (escaped_char | !(["\""] | ["\\"]) ~ any)* }
            escaped_char  =  _{ ["\\"] ~ (["\""] | ["\\"] | ["/"] | ["b"] | ["f"] | ["n"] | ["r"] | ["t"]) }

        unit = @{ number ~ number_suffix }
            number = @{ ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) ~ ( ["."] ~ ['0'..'9']+ )? }
            number_suffix = @{ (['a'..'z']+ | ["%"])? }

        whitespace = _{ ([" "] | ["\n"] | ["\t"] | ["\r"])+ }
        comment = _{ block_comment | eol_comment }
            block_comment = _{ ["/*"] ~ (block_comment | !(block_comment | ["*/"]) ~ any)* ~ ["*/"] }
            eol_comment = _{ ["//"] ~ (!["\n"] ~ any)* ~ ["\n"] }

    }

    process!{
        main(&self) -> Expression {
            (rule:expression, expr:_expr()) => {
                Expression{
                    start: Position(rule.start),
                    end: Position(rule.end),
                    expr: expr
                }
            }
        }
        _expr(&self) -> Expr {
            (rule:expression, expr:_expr()) => expr,
            (rule:infix0, expr:_infix()) => expr,
            (rule:infix1, expr:_infix()) => expr,
            (rule:infix2, expr:_infix()) => expr,
            (rule:infix3, expr:_infix()) => expr,
            (rule:infix4, expr:_infix()) => expr,
            (rule:infix5, expr:_infix()) => expr,
            (rule:function, expr:_function()) => expr,
            (rule:if_then_else, expr:_if_then_else()) => expr,
            (rule:application, expr:_application()) => expr,
            (rule:variable_accessor, expr:_variable_accessor()) => expr,
            // primitives:
            (rule:string, &s:string_contents) => Expr::Prim(Primitive::Str(s.to_owned())),
            (rule:unit, &n:number, &s:number_suffix) => Expr::Prim(Primitive::Unit( n.parse::<f64>().unwrap(), s.to_owned() )),
            (rule:boolean, _:boolean_true) => Expr::Prim(Primitive::Bool(true)),
            (rule:boolean, _:boolean_false) => Expr::Prim(Primitive::Bool(false)),
        }
        _infix(&self) -> Expr {
            (left:main(), sign:_symbol(), right:main()) => {
                Expr::App{
                    expr: Box::new(sign),
                    args: vec![ left, right ]
                }
            }
        }
        _symbol(&self) -> Expression {
            (sign) => {
                let tok = self.input().slice(sign.start,sign.end);
                Expression{
                    start: Position(sign.start),
                    end: Position(sign.end),
                    expr: Expr::Var(tok.to_owned(), vec![])
                }
            }
        }
        _function(&self) -> Expr {
            (args:_function_args(), expr:main()) => {
                let arg_vec = args.into_iter().collect::<Vec<String>>();
                Expr::Func{
                    inputs: arg_vec,
                    output: Box::new(expr),
                    scope: Scope::new()
                }
            }
        }
        _function_args(&self) -> LinkedList<String> {
            (_: variable, &name:variable_name, mut tail: _function_args()) => {
                tail.push_front(name.to_owned());
                tail
            },
            () => {
                LinkedList::new()
            }
        }
        _if_then_else(&self) -> Expr {
            (cond: main(), then: main(), otherwise: main()) => {
                Expr::If{
                    cond: Box::new(cond),
                    then: Box::new(then),
                    otherwise: Box::new(otherwise)
                }
            }
        }
        _application(&self) -> Expr {
            (_:prefix_application, sign:_symbol(), _:prefix_application_arg, arg:main()) => {
                Expr::App{
                    expr: Box::new(sign),
                    args: vec![arg]
                }
            },
            (_:function_application, func:main(), args:_application_args()) => {
                Expr::App{
                    expr: Box::new(func),
                    args: args.into_iter().collect::<Vec<Expression>>()
                }
            }
        }
        _application_args(&self) -> LinkedList<Expression> {
            (_:function_application_arg, expr:main(), mut tail: _application_args()) => {
                tail.push_front(expr);
                tail
            },
            () => {
                LinkedList::new()
            }
        }
        _variable_accessor(&self) -> Expr {
            (_:variable, &var:variable_name, suffix: _variable_accessor_suffix()) => {
                Expr::Var(var.to_owned(), suffix.into_iter().collect::<Vec<String>>())
            }
        }
        _variable_accessor_suffix(&self) -> LinkedList<String> {
            (&piece:variable_name, mut tail: _variable_accessor_suffix()) => {
                tail.push_front(piece.to_owned());
                tail
            },
            () => {
                LinkedList::new()
            }
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    macro_rules! parse_test {

        // create test function:
        ( $func:ident; $($rest:tt)+ ) => (
            #[test]
            fn $func() {
                use std::fmt::Write;
                let mut errors = String::new();
                parse_test!{ __SINGLE (errors) $($rest)+ }
                if errors.len() > 0 {
                    assert!(false, "\n{}", errors);
                }
            }
        );

        ( __SINGLE ($errors:ident) ) => ();

        // str => variable[ child, child.. ]
        ( __SINGLE ($errors:ident) $input:expr => $token:ident[ $($inner:ident $children:tt),* ]; $($rest:tt)* ) => (
            {
                let s = $input.to_owned();
                let mut parser = Rdp::new(StringInput::new($input));

                if !parser.$token(){
                    writeln!(&mut $errors, "ERROR: could not parse!\n - wants: {:?}\n - input: \n{}\n - expected: {:?}\n - got: {:?}", parser.expected(), s, Rule::$token, parser.queue());
                }
                else if !parser.end() {
                    writeln!(&mut $errors, "ERROR: didn't end when expected!\n - wants: {:?}\n - input: \n{}\n - expected: {:?}\n - got: {:?}", parser.expected(), s, Rule::$token, parser.queue());
                }
                else {
                    assert_eq!(s.len(), parser.queue()[0].end);
                }
            }
            parse_test!{ __SINGLE ($errors) $($rest)* }
        );

    }

    parse_test!{test_variable;
        "$hello" => variable[];
        "$2ello" => variable[];
        "$_hello" => variable[];
        "$hello" => expression[];
        "$2ello" => expression[];
        "$_hello" => expression[];
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

    parse_test!{test_functions;
        "() => true" => function[];
        "($lark, $another) => $lark" => function[];
    }

    parse_test!{test_operators;
        "20 + 40" => expression[];
        "20 / 40" => expression[];
        "20 * 40" => expression[];
        "20 - 40" => expression[];
    }

    parse_test!{test_func_applications;
        "$hello(2px,true)" => expression[];
        "$hello(2px, true)" => expression[];
        "$hello(2px, true)" => application[];
        "!$hello" => expression[];
        "!($hello)" => expression[];
        "!$hello()" => expression[];
        "!($hello())" => expression[];
        "!$hello(2px)" => expression[];
        "!($hello(2px))" => expression[];
        "!($hello(2px, true))" => expression[];
        "!$hello(2px, true)" => expression[];
        "!$hello()" => application[];
        "!$hello(1px)" => application[];
        "!$hello(1px, true)" => application[];
        "!($hello)" => application[];
        "!($hello())" => application[];
        "!($hello(1px))" => application[];
        "!($hello(1px, true))" => application[];
    }

    parse_test!{test_block_css;
        "hello: there;" => block_css[];
        "${ $hello }: there;" => block_css[];
        "${ $hello }: the${ 2px }re;" => block_css[];
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
    }

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
            ${ $hello(2px, 5px) };
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
            // a comment till end of line
            // lark
            // woop
            & .a-sub-class .more.another, .sub-clas-two { color: red; $subThing: -2px + 4px; }
            -moz-background-color: 1px solid blue;
            border-radius: 10px;
        }" => block[];
        "{ $lark: {}; .stuff {} }" => block[];
        "{ .stuff {} $lark: {}; }" => block[];
    }

}