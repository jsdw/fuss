use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::hash::Hash;

/// A cache to be used in contexts:
#[derive(Clone)]
pub struct Cache<K,V>( Rc<RefCell<HashMap<K,V>>> );

impl <K: Hash + Eq,V: Clone> Cache<K,V> {
    pub fn new() -> Cache<K,V> {
        Cache( Rc::new( RefCell::new(HashMap::new()) ) )
    }
    pub fn set(&self, key: K, entry: V) {
        self.0.borrow_mut().insert(key, entry);
    }
    pub fn exists(&self, key: &K) -> bool {
        self.0.borrow().contains_key(key)
    }
    pub fn get(&self, key: &K) -> Option<V> {
        self.0.borrow_mut().get(key).map(|o| o.clone())
    }
}