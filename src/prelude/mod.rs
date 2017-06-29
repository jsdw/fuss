mod ops;

use types::*;
use std::collections::HashMap;

/// make it a little easier to build up our scope, below.
macro_rules! scope {
    ( $name:ident; $($key:expr => $func:expr);*; ) => (
        lazy_static!{
            pub static ref $name: HashMap<String,Expression> = {
                let mut map = HashMap::new();
                $(
                    map.insert($key.to_owned(), Expression{
                        start: Position::new(),
                        end: Position::new(),
                        expr: Expr::PrimFunc($func)
                    });
                )*
                map
            };
        }
    )
}

/// define our prelude. This is the base set of
/// functions available to anything.
scope!{prelude;

    "+" => ops::Add;
    "-" => ops::Subtract;

}