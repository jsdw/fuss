pub mod ops;
pub mod casting;

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
    "to_boolean" => casting::boolean;
    "to_string" => casting::string;

}