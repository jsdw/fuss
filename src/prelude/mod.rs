pub mod ops;
pub mod casting;
pub mod import;
pub mod blocks;

use types::*;

fn get_prelude() -> Scope {
    scope!{

        "+" => ops::add;
        "-" => ops::subtract;
        "/" => ops::divide;
        "*" => ops::multiply;
        "^" => ops::pow;
        "!" => ops::not;
        "==" => ops::equal;
        "<=" => ops::less_than_or_equal;
        ">=" => ops::greater_than_or_equal;
        "<" => ops::less_than;
        ">" => ops::greater_than;
        "!=" => ops::not_equal;
        "&&" => ops::boolean_and;
        "||" => ops::boolean_or;
        "to_boolean" => casting::boolean;
        "to_string" => casting::string;
        "import" => import::import;
        "merge" => blocks::merge

    }
}