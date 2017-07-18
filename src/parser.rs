use pest::prelude::*;
use types::*;
use std::collections::{LinkedList,HashMap};

// parse a string into an expression
pub fn parse(input: &str) -> Result<Expression,ErrorType> {

    let mut parser = Rdp::new(StringInput::new(input));

    if !parser.expression() {
        return Err(ErrorType::NotAnExpression)
    }
    Ok(parser.main())

}

// turn a Token + Expr into an Expression:
fn expression(rule: Token<Rule>, expr: Expr) -> Expression {
    Expression{
        start: Position(rule.start),
        end: Position(rule.end),
        expr: expr
    }
}

// a quick struct to let us build up a linkedlist of block inner pieces:
#[derive(Clone,Debug,PartialEq)]
enum BlockInner {
    Scope(String,Expression),
    CSS(CSSEntry)
}

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

        block = { block_selector ~ ["{"] ~ (block_assignment | block_css | block_expression )* ~ ["}"] }

            block_expression = { (block_interpolated_expression | variable) ~ [";"] | block_nested }
            block_nested = { block }
            block_interpolated_expression = { ["${"] ~ expression ~ ["}"] }
            block_assignment = { block_variable_assign ~ expression ~ [";"] }
                block_variable_assign = @{ variable ~ [":"] }

            block_css = { block_css_key ~ [":"] ~ block_css_value ~ [";"] }
            block_selector = { ( block_interpolated_expression | block_selector_chars )* }
                block_selector_chars = @{ ( !(["$"] | ["{"] | [";"] | ["}"]) ~ any )+ }

            block_css_key = { (block_interpolated_expression | block_css_key_chars)+ }
                block_css_key_chars = @{ ( ['a'..'z'] | ["-"] )+ }
            block_css_value = { (block_interpolated_expression | variable | block_css_value_chars)+ }
                block_css_value_chars = @{ ( !(block_interpolated_expression | variable | [";"]) ~ any )+ }

        application = { prefix_application | function_application }

            prefix_application = @{ prefix_application_fn ~ prefix_application_arg }
            prefix_application_fn = { ["-"] | ["!"] }
            prefix_application_arg = !@{ paren_expression | function_application | variable_accessor }

            function_application = { function_application_fn ~ ["("] ~ function_application_args ~ [")"] }
            function_application_fn = { variable_accessor | paren_expression }
            function_application_args = { ( function_application_arg ~ ([","] ~ function_application_arg)* )? }
            function_application_arg = { expression }

        paren_expression = { ["("] ~ expression ~ [")"] }
        variable = @{ ["$"] ~ variable_name }
        variable_accessor = { variable ~ ( ["."] ~ variable_name )* }
        if_then_else = { ["if"] ~ expression ~ ["then"] ~ expression ~ ["else"] ~ expression }

        function = { ["("] ~ function_args? ~ [")"] ~ ["=>"] ~ expression }
            function_args = { variable ~ ( [","] ~ variable )* }
            function_expression = { expression }

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
            // recursing rules:
            (rule:expression, expression:main()) =>
                expression,
            (rule:paren_expression, expression: main()) =>
                expression,
            // infix precedence parsing:
            (rule:infix0, expr:_infix()) =>
                expression(rule, expr),
            (rule:infix1, expr:_infix()) =>
                expression(rule, expr),
            (rule:infix2, expr:_infix()) =>
                expression(rule, expr),
            (rule:infix3, expr:_infix()) =>
                expression(rule, expr),
            (rule:infix4, expr:_infix()) =>
                expression(rule, expr),
            (rule:infix5, expr:_infix()) =>
                expression(rule, expr),
            // exprs:
            (rule:function, expr:_function()) =>
                expression(rule, expr),
            (rule:if_then_else, expr:_if_then_else()) =>
                expression(rule, expr),
            (rule:application, expr:_application()) =>
                expression(rule, expr),
            (rule:variable_accessor, expr:_variable_accessor()) =>
                expression(rule, expr),
            (rule:block, expr:_block()) =>
                expression(rule, expr),
            // primitives:
            (rule:string, &s:string_contents) =>
                expression(rule, Expr::Prim(Primitive::Str(s.to_owned()))),
            (rule:unit, &n:number, &s:number_suffix) =>
                expression(rule, Expr::Prim(Primitive::Unit( n.parse::<f64>().unwrap(), s.to_owned() ))),
            (rule:boolean, _:boolean_true) =>
                expression(rule, Expr::Prim(Primitive::Bool(true))),
            (rule:boolean, _:boolean_false) =>
                expression(rule, Expr::Prim(Primitive::Bool(false))),
        }
        _infix(&self) -> Expr {
            (left:main(), sign:_variable_expression(), right:main()) => {
                Expr::App{
                    expr: Box::new(sign),
                    args: vec![ left, right ]
                }
            }
        }
        _variable_expression(&self) -> Expression {
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
            (_:function_args, args:_function_args(), _:function_expression, expr:main()) => {
                let arg_vec = args.into_iter().collect::<Vec<String>>();
                Expr::Func{
                    inputs: arg_vec,
                    output: Box::new(expr),
                    scope: Scope::new()
                }
            },
            (_:function_expression, expr:main()) => {
                Expr::Func{
                    inputs: vec![],
                    output: Box::new(expr),
                    scope: Scope::new()
                }
            },
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
            (_:prefix_application, sign:_variable_expression(), _:prefix_application_arg, arg:main()) => {
                Expr::App{
                    expr: Box::new(sign),
                    args: vec![arg]
                }
            },
            (_:function_application, _:function_application_fn, func:main(), _:function_application_args, args:_application_args()) => {
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
        _block(&self) -> Expr {
            (selector:_block_selector(), rest:_block_inner()) => {
                let selector = selector.into_iter().collect::<Vec<CSSBit>>();
                let mut scope = HashMap::new();
                let mut css = vec![];
                for val in rest.into_iter() {
                    match val {
                        BlockInner::Scope(key,val) => {
                            scope.insert(key,val);
                        },
                        BlockInner::CSS(entry) => {
                            css.push(entry);
                        }
                    }
                }
                Expr::Block(Block{scope:scope,css:css,selector:selector})
            }
        }
        _block_selector(&self) -> LinkedList<CSSBit> {
            (&chars:block_selector_chars, mut tail: _block_selector()) => {
                tail.push_front( CSSBit::Str(chars.to_owned()) );
                tail
            },
            (_:block_interpolated_expression, expr:main(), mut tail: _block_selector()) => {
                tail.push_front( CSSBit::Expr(expr) );
                tail
            },
            () => {
                LinkedList::new()
            }
        }
        _block_inner(&self) -> LinkedList<BlockInner> {
            (_:block_assignment, _:block_variable_assign, &var:variable, expr:main(), mut tail:_block_inner()) => {
                tail.push_front( BlockInner::Scope(var.to_owned(),expr) );
                tail
            },
            (_:block_expression, _:block_interpolated_expression, expr:main(), mut tail:_block_inner()) => {
                tail.push_front( BlockInner::CSS( CSSEntry::Expr(expr) ) );
                tail
            },
            (_:block_expression, _:variable, expr:_variable_expression(), mut tail:_block_inner()) => {
                tail.push_front( BlockInner::CSS( CSSEntry::Expr(expr) ) );
                tail
            },
            (_:block_expression, _:block_nested, expr:main(), mut tail:_block_inner()) => {
                tail.push_front( BlockInner::CSS( CSSEntry::Expr(expr) ) );
                tail
            },
            (_:block_css, entry:_block_css(), mut tail:_block_inner()) => {
                tail.push_front( BlockInner::CSS( entry ) );
                tail
            },
            () => {
                LinkedList::new()
            }
        }
        _block_css(&self) -> CSSEntry {
            (key:_block_css_key(), val:_block_css_val()) => {
                CSSEntry::KeyVal{
                    key: key.into_iter().collect::<Vec<CSSBit>>(),
                    val: val.into_iter().collect::<Vec<CSSBit>>()
                }
            }
        }
        _block_css_key(&self) -> LinkedList<CSSBit> {
            (_:block_interpolated_expression, expr:main(), mut tail:_block_css_key()) => {
                tail.push_front( CSSBit::Expr(expr) );
                tail
            },
            (&chars:block_css_key_chars, mut tail:_block_css_key()) => {
                tail.push_front( CSSBit::Str(chars.to_owned()) );
                tail
            },
            () => {
                LinkedList::new()
            }
        }
        _block_css_val(&self) -> LinkedList<CSSBit> { //TODO: FILL IN!
            (_:block_interpolated_expression, expr:main(), mut tail:_block_css_val()) => {
                tail.push_front( CSSBit::Expr(expr) );
                tail
            },
            (_:variable, var:_variable_expression(), mut tail:_block_css_val()) => {
                tail.push_front( CSSBit::Expr(var) );
                tail
            },
            (&chars:block_css_value_chars, mut tail:_block_css_val()) => {
                tail.push_front( CSSBit::Str(chars.to_owned()) );
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

    // test the parsing aspect:
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

    // test the processing aspect:
    macro_rules! process_test {

        // create test function:
        ( $func:ident; $($rest:tt)+ ) => (
            #[test]
            fn $func() {
                use std::fmt::Write;
                let mut errors = String::new();
                process_test!{ __SINGLE (errors) $($rest)+ }
                if errors.len() > 0 {
                    assert!(false, "\n{}", errors);
                }
            }
        );

        // run a test:
        ( __SINGLE ($errors:ident) $input:expr => $output:expr; $($rest:tt)* ) => (
            {
                match parse($input) {
                    Ok(expr) => {
                        //TODO: TestEquality trait that ignores expression positions but uses partialEq for everything else,
                        //      so that we can compare for testing
                        if expr != $output {
                            writeln!(&mut errors, "ERROR: exprs did not match!\n - expected: {:?}\n - got: {:?}", $output, expr);
                        }
                    }
                    Err(err) => writeln!(&mut errors, "ERROR: could not build expr!\n - input: {:?}\n - expected: {:?}", $input, $output);
                }
            }
            process_test!( __SINGLE ($errors) $($rest)*);
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