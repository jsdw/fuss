use std::collections::HashMap;
use types::list::{List};
use types::{EvaluatedExpression};

/// A stack that we can push values onto in order to represent the
/// current scope at any point in time.
#[derive(Clone,Debug,PartialEq)]
pub struct Scope(List<HashMap<String,EvaluatedExpression>>);
impl Scope {

    /// create a new, empty scope:
    pub fn new() -> Self {
        Scope(List::new().push(HashMap::new()))
    }
    pub fn from(map: HashMap<String,EvaluatedExpression>) -> Self {
        Scope(List::new().push(map))
    }

    /// lookup a value in the scope:
    pub fn find<'a>(&'a self, name: &str) -> Option<&'a EvaluatedExpression> {
        for map in self.0.iter() {
            if let Some(expr) = map.get(name) {
                return Some(expr)
            }
        }
        None
    }

    /// push some new values onto the scope, returning a new one and
    /// leaving the original unchanged:
    pub fn push(&self, values: HashMap<String,EvaluatedExpression>) -> Scope {
        Scope(self.0.push(values))
    }

    /*
    /// push one key and value onto the scope, returning a new one and
    /// leaving the original unchanged:
    pub fn push_one(&self, key: String, value: Expression) -> Scope {
        let mut map = HashMap::with_capacity(1);
        map.insert(key, value);
        self.push(map)
    }
    */
}