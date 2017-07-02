pub mod ops;
pub mod casting;
pub mod import;

use types::*;
use std::collections::HashMap;

/// define our prelude. This is the base set of
/// functions available to anything.
scope!{PRELUDE;

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

}