use pest::prelude::*;

impl_rdp! {
    grammar! {
        expression = { paren_expression | variable | if_then_else | function }
        paren_expression = _{ ["("] ~ expression ~ [")"] }
        variable = { ["$"] ~ variable_name }
        if_then_else = { ["if"] ~ expression ~ ["then"] ~ expression ~ ["else"] ~ expression }

        function = { ["("] ~ function_args? ~ [")"] ~ ["=>"] ~ expression }
            function_args = _{ variable ~ ( [","] ~ variable )* }

        function_application_prefix = { (variable | paren_expression) ~ ["("] ~ function_application_args? ~ [")"] }
            function_application_args = _{ expression ~ ( [","] ~ expression )* }

        variable_name = _{ (['a'..'z'] | ['A'..'Z'] | ["_"] | ['0'..'9'])+ }
        boolean = { ["true"] | ["false"] }
        number = { ["-"]? ~ (["0"] | ['1'..'9'] ~ ['0'..'9']*) }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_variable() {

        let vars = vec![
            "$hello",
            "$2ello",
            "$_hello_"
        ];

        for var in vars {
            let mut parser = Rdp::new(StringInput::new(var.clone()));
            assert!(parser.variable());
            assert!(parser.end());
            assert_eq!(
                parser.queue().iter().cloned().map(|i| i.rule).collect::<Vec<_>>(),
                vec![Rule::variable],
                "expected '{}' to be parsed as a variable", var);
        }

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