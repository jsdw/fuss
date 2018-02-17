
use std::mem;
use std::convert::From;
use SmallVec::*;

/// A struct for storing items that will store one or two items allocation free on the stack,
/// and transparently expand into a vector if more items need to be stored.
#[derive(Debug,Clone,PartialEq)]
pub enum SmallVec<T> {
    Empty,
    One(T),
    Two(T,T),
    Heap(Vec<T>)
}

impl <T> SmallVec<T> {
    pub fn one(item: T) -> SmallVec<T> {
        One(item)
    }
    pub fn first(&self) -> &T {
        match *self {
            Empty => unreachable!(),
            One(ref item) => item,
            Two(ref item, ..) => item,
            Heap(ref items) => items.first().expect("one item always expected in smallvec")
        }
    }
    pub fn last(&self) -> &T {
        match *self {
            Empty => unreachable!(),
            One(ref item) => item,
            Two(_, ref item) => item,
            Heap(ref items) => items.last().expect("one item always expected in smallvec")
        }
    }
    pub fn push(&mut self, item: T) {
        let next = match mem::replace(self, Empty) {
            Empty => {
                One(item)
            }
            One(curr) => {
                Two(curr,item)
            },
            Two(curr1,curr2) => {
                Heap(vec![curr1,curr2,item])
            },
            Heap(mut items) => {
                items.push(item);
                Heap(items)
            }
        };
        *self = next;
    }
    fn get(&self, pos: usize) -> Option<&T> {
        match *self {
            Empty => None,
            One(ref item) => {
                if pos == 0 {
                    Some(item)
                } else {
                    None
                }
            },
            Two(ref item1, ref item2) => {
                if pos == 0 {
                    Some(item1)
                } else if pos == 1 {
                    Some(item2)
                } else {
                    None
                }
            },
            Heap(ref items) => {
                items.get(pos)
            }
        }
    }
    pub fn len(&self) -> usize {
        match *self {
            Empty => 0,
            One{..} => 1,
            Two{..} => 2,
            Heap(ref items) => items.len()
        }
    }
    pub fn iter(&self) -> Iter<T> {
        let len = self.len();
        Iter { vec: self, pos: 0, seen_back: len }
    }
}

impl <T: Clone> SmallVec<T> {
    pub fn with_item(&self, item: T) -> SmallVec<T> {
        match *self {
            Empty => One(item),
            One(ref curr) => Two(curr.clone(),item),
            Two(ref curr1, ref curr2) => Heap(vec![curr1.clone(),curr2.clone(),item]),
            Heap(ref items) => {
                let mut items = items.clone();
                items.push(item);
                Heap(items)
            }
        }
    }
}

impl <T> From<T> for SmallVec<T> {
    fn from(item: T) -> SmallVec<T> {
        SmallVec::one(item)
    }
}

/// An iterator over a SmallVec yielding references to items contained within
pub struct Iter<'a, T: 'a> {
    vec: &'a SmallVec<T>,
    pos: usize,
    seen_back: usize
}

impl <'a,T> Iterator for Iter<'a,T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.pos == self.seen_back {
            return None
        }
        let ret = self.vec.get(self.pos);
        self.pos += 1;
        ret
    }
}

impl <'a,T> DoubleEndedIterator for Iter<'a,T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.seen_back == 0 || self.pos == self.seen_back {
            return None
        }
        self.seen_back -= 1;
        let ret = self.vec.get(self.seen_back);
        ret
    }
}

#[cfg(test)]
mod test {

    use super::*;

    fn into_vec<T: Clone>(v: SmallVec<T>) -> Vec<T> {
        v.iter().cloned().collect()
    }
    fn into_vec_rev<T: Clone>(v: SmallVec<T>) -> Vec<T> {
        v.iter().rev().cloned().collect()
    }

    #[test]
    fn one() {
        let v = SmallVec::one('a');
        assert_eq!(into_vec(v), vec!['a']);
    }

    #[test]
    fn two() {
        let mut v = SmallVec::one('a');
        v.push('b');
        assert_eq!(into_vec(v), vec!['a', 'b']);
    }

    #[test]
    fn three() {
        let mut v = SmallVec::one('a');
        v.push('b');
        v.push('c');
        assert_eq!(into_vec(v), vec!['a', 'b', 'c']);
    }

    #[test]
    fn multiple() {
        let mut v = SmallVec::one('a');
        v.push('b');
        v.push('c');
        v.push('d');
        assert_eq!(into_vec(v), vec!['a', 'b', 'c', 'd']);
    }

    #[test]
    fn two_with() {
        let v = SmallVec::one('a');
        let v2 = v.with_item('b');
        assert_eq!(into_vec(v), vec!['a']);
        assert_eq!(into_vec(v2), vec!['a', 'b']);
    }

    #[test]
    fn multiple_with() {
        let v = SmallVec::one('a');
        let v2 = v.with_item('b');
        let v3 = v2.with_item('c');
        let v4 = v3.with_item('d');
        assert_eq!(into_vec(v), vec!['a']);
        assert_eq!(into_vec(v2), vec!['a', 'b']);
        assert_eq!(into_vec(v3), vec!['a', 'b', 'c']);
        assert_eq!(into_vec(v4), vec!['a', 'b', 'c', 'd']);
    }

    #[test]
    fn one_rev() {
        let v = SmallVec::one('a');
        assert_eq!(into_vec_rev(v), vec!['a']);
    }

    #[test]
    fn two_rev() {
        let mut v = SmallVec::one('a');
        v.push('b');
        assert_eq!(into_vec_rev(v), vec!['b','a']);
    }

    #[test]
    fn three_rev() {
        let mut v = SmallVec::one('a');
        v.push('b');
        v.push('c');
        assert_eq!(into_vec_rev(v), vec!['c','b','a']);
    }

    #[test]
    fn four_rev() {
        let mut v = SmallVec::one('a');
        v.push('b');
        v.push('c');
        v.push('d');
        assert_eq!(into_vec_rev(v), vec!['d','c','b','a']);
    }

    #[test]
    fn both_ways_one() {
        let v = SmallVec::one('a');
        let mut iter = v.iter();
        assert_eq!(iter.next_back(), Some(&'a'));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next_back(), None);
    }

    #[test]
    fn both_ways_two() {
        let mut v = SmallVec::one('a');
        v.push('b');
        let mut iter = v.iter();
        assert_eq!(iter.next_back(), Some(&'b'));
        assert_eq!(iter.next(), Some(&'a'));
        assert_eq!(iter.next_back(), None);
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn both_ways_multiple() {
        let mut v = SmallVec::one('a');
        v.push('b');
        v.push('c');
        v.push('d');
        let mut iter = v.iter();
        assert_eq!(iter.next_back(), Some(&'d'));
        assert_eq!(iter.next(), Some(&'a'));
        assert_eq!(iter.next_back(), Some(&'c'));
        assert_eq!(iter.next(), Some(&'b'));
        assert_eq!(iter.next_back(), None);
        assert_eq!(iter.next(), None);
    }

}