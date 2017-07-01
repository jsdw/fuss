mod ops;

use types::*;
use std::collections::HashMap;

/// define our prelude. This is the base set of
/// functions available to anything.
scope!{prelude;

    "+" => ops::Add;
    "-" => ops::Subtract;

}