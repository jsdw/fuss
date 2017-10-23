use std::collections::HashMap;
use types::list::{List};
use types::{EvaluatedExpression, VarType};
use std::rc::Rc;

/// A stack that we can push values onto in order to represent the
/// current scope at any point in time.
#[derive(Clone,Debug,PartialEq)]
pub struct Scope{
    user: List<HashMap<String,EvaluatedExpression>>,
    builtin: Rc<HashMap<String,EvaluatedExpression>>
}

impl Scope {

    // /// create a new, empty scope:
    // pub fn new() -> Self {
    //     Scope {
    //         user: List::new(),
    //         builtin: Rc::new(HashMap::new())
    //     }
    // }

    /// Initialise a scope with a set of builtin variables (this is the main approach)
    pub fn from(map: HashMap<String,EvaluatedExpression>) -> Self {
        Scope {
            user: List::new(),
            builtin: Rc::new(map)
        }
    }

    /// lookup a value in the scope:
    pub fn find<'a>(&'a self, name: &str, ty: VarType) -> Option<&'a EvaluatedExpression> {
        match ty {
            VarType::User => self.find_user(name),
            VarType::Builtin => self.find_builtin(name),
            //VarType::Unknown => self.find_user(name).or_else(|| self.find_builtin(name))
        }
    }
    fn find_user<'a>(&'a self, name: &str) -> Option<&'a EvaluatedExpression> {
        for map in self.user.iter() {
            if let Some(expr) = map.get(name) {
                return Some(expr)
            }
        }
        None
    }
    fn find_builtin<'a>(&'a self, name: &str) -> Option<&'a EvaluatedExpression> {
        self.builtin.get(name)
    }

    /// push some new user values onto the scope, returning a new one and
    /// leaving the original unchanged:
    pub fn push(&self, values: HashMap<String,EvaluatedExpression>) -> Scope {
        Scope{
            user: self.user.push(values),
            builtin: self.builtin.clone()
        }
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