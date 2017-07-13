use pest::prelude::*;

impl_rdp! {
    grammar! {
        expression = {
            { if_then_else | function | boolean | unit | prefix_expression | function_application_prefix | paren_expression | variable }
            infix0 = { or }
            infix1 = { and }
            infix2 = { equal | not_equal | gt_or_equal | lt_or_equal | gt | lt }
            infix3 = { plus | minus }
            infix4 = { times | slash }
            infix5 = {< pow }
        }
        prefix_expression = @{ minus ~ expression | negate ~ expression }
        paren_expression = _{ ["("] ~ expression ~ [")"] }
        variable = { ["$"] ~ variable_name }
        if_then_else = { ["if"] ~ expression ~ ["then"] ~ expression ~ ["else"] ~ expression }

        function = { ["("] ~ function_args? ~ [")"] ~ ["=>"] ~ expression }
            function_args = _{ variable ~ ( [","] ~ variable )* }

        function_application_prefix = { (prefix_expression | variable | paren_expression) ~ ["("] ~ function_application_args? ~ [")"] }
            function_application_args = _{ expression ~ ( [","] ~ expression )* }

        variable_name = _{ (['a'..'z'] | ['A'..'Z'] | ["_"] | ['0'..'9'])+ }
        boolean = { ["true"] | ["false"] }

        string  = @{ ["\""] ~ string_contents ~ ["\""] }
        string_contents = _{ (escaped_char | !(["\""] | ["\\"]) ~ any)* }
        escaped_char  =  _{ ["\\"] ~ (["\""] | ["\\"] | ["/"] | ["b"] | ["f"] | ["n"] | ["r"] | ["t"]) }

        unit = @{ number ~ number_suffix? }
        number = @{ ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) ~ ( ["."] ~ ['0'..'9']+ )? }
        number_suffix = @{ (['a'..'z']+ | ["%"])? }

        whitespace = { ([" "] | ["\n"] | ["\t"] | ["\r"])+ }

        minus = { ["-"] }
        negate = { ["!"] }
        plus = { ["+"] }
        times = { ["*"] }
        slash = { ["/"] }
        pow = { ["^"] }
        equal = { ["=="] }
        not_equal = { ["!="] }
        gt = { [">"] }
        lt = { ["<"] }
        lt_or_equal = { ["<="] }
        gt_or_equal = { [">="] }
        and = { ["&&"] }
        or = { ["||"] }

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
                parse_test!{ __SINGLE $($rest)+ }
            }
        );

        ( __SINGLE ) => ();

        // str => [var1,var2] tests:
        ( __SINGLE $input:expr => [ $($token:ident),+ ]; $($rest:tt)* ) => (
            {

                let mut parser = Rdp::new(StringInput::new($input));
                let mut toks = vec![];

                $({
                    assert!(parser.$token(), "could not parse {:?}", Rule::$token);
                    toks.push(Rule::$token);
                })+
                assert!(parser.end(), "didn't end when expected!");

                assert_eq!(
                    parser.queue().iter().cloned().map(|i| i.rule).collect::<Vec<_>>(),
                    toks,
                    "tokens did not match those expected");

            }
            parse_test!{ __SINGLE $($rest)* }
        );

        // str => variable
        (__SINGLE $input:expr => $token:ident; $($rest:tt)* ) => (
            {

                let s = $input.to_owned();
                let mut parser = Rdp::new(StringInput::new($input));

                assert!(parser.$token(), "could not parse {:?}", Rule::$token);
                assert!(parser.end(), "didn't end when expected!");
                assert_eq!(s.len(), parser.queue()[0].end);

            }
            parse_test!{ __SINGLE $($rest)* }
        )

    }

    parse_test!{test_variable;
        "$hello" => variable;
        "$2ello" => variable;
        "$_hello" => variable;
    }

    parse_test!{test_numeric;
        "20" => number;
        "-20" => number;
        "0.12" => number;
        "100.0" => number;
        "100.0px" => unit;
        "100.0em" => unit;
        "0%" => unit;
    }

    parse_test!{test_if_then_else;
        "if true then true else true" => if_then_else;
        "if 20px then 10 else 100" => if_then_else;
    }

    parse_test!{test_strings;
        r#""hello there""# => string;
        r#""hello \"hello\" there""# => string;
    }

    parse_test!{test_functions;
        "() => true" => function;
        "($lark, $another) => $lark" => function;
    }

    parse_test!{test_operators;
        "20 + 40" => expression;
        "20 / 40" => expression;
        "20 * 40" => expression;
        "20 - 40" => expression;
    }

    parse_test!{test_func_applications;
        "$hello(2px, true)" => expression;
        "$hello(2px, true)" => function_application_prefix;
        "!$hello(2px, true)" => expression;
        "!$hello(2px, true)" => prefix_expression;
    }

}

//#[test]
fn pest_calculator_test() {

    impl_rdp! {
        grammar! {
            expression = _{
                { ["("] ~ expression ~ [")"] | number }
                addition       = { plus  | minus }
                multiplication = { times | slash }
                exponent       = {< pow }
            }
            number = @{ ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) }
            plus   =  { ["+"] }
            minus  =  { ["-"] }
            times  =  { ["*"] }
            slash  =  { ["/"] }
            pow    =  { ["^"] }

            whitespace = _{ [" "] }
        }

        process! {
            compute(&self) -> i32 {
                (&number: number) => number.parse::<i32>().unwrap(),
                (&addition: addition, left: compute(), sign, right: compute()) => {
                    println!("Adding: {} , SIGN: {:?}", addition, sign);
                    match sign.rule {
                        Rule::plus  => left + right,
                        Rule::minus => left - right,
                        _ => unreachable!()
                    }
                },
                (_: multiplication, left: compute(), sign, right: compute()) => {
                    match sign.rule {
                        Rule::times => left * right,
                        Rule::slash => left / right,
                        _ => unreachable!()
                    }
                },
                (_: exponent, left: compute(), _, right: compute()) => {
                    left.pow(right as u32)
                }
            }
        }
    }

    let mut parser = Rdp::new(StringInput::new("(3 + (9 + 3 * 4 + (3 + 1) / 2 - 4)) * 2 + (2^2^2^2)"));

    assert!(parser.expression());
    assert_eq!(parser.compute(), 44 + 65536);

}