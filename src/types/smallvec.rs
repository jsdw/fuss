
use std::mem;
use SmallVec::*;

/// A struct for storing items that will store one item allocation free on the stack,
/// and transparently expand into a vector if more items need to be stored.
#[derive(Debug,Clone)]
pub enum SmallVec<T> {
    Empty,
    Stack(T),
    Heap(Vec<T>)
}

impl <T> SmallVec<T> {
    pub fn one(item: T) -> SmallVec<T> {
        Stack(item)
    }
    pub fn push(&mut self, item: T) {
        match mem::replace(self, Empty) {
            Empty => {
                *self = Stack(item);
            }
            Stack(curr) => {
                *self = Heap(vec![curr,item]);
            },
            Heap(ref mut items) => {
                items.push(item);
            }
        }
    }
    pub fn into_vec(self) -> Vec<T> {
        match self {
            Empty => vec![],
            Stack(item) => vec![item],
            Heap(items) => items
        }
    }
}