
use std::mem;
use SmallVec::*;

/// A struct for storing items that will store one or two items allocation free on the stack,
/// and transparently expand into a vector if more items need to be stored.
#[derive(Debug,Clone)]
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
    pub fn iter(&self) -> Iter<T> {
        Iter { vec: self, pos: 0 }
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

/// An iterator over a SmallVec yielding references to items contained within
pub struct Iter<'a, T: 'a> {
    vec: &'a SmallVec<T>,
    pos: usize
}

impl <'a,T> Iterator for Iter<'a,T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        let ret = match *self.vec {
            Empty => None,
            One(ref item) => {
                if self.pos == 0 {
                    Some(item)
                } else {
                    None
                }
            },
            Two(ref item1, ref item2) => {
                if self.pos == 0 {
                    Some(item1)
                } else if self.pos == 1 {
                    Some(item2)
                } else {
                    None
                }
            },
            Heap(ref items) => {
                items.get(self.pos)
            }
        };
        self.pos += 1;
        ret
    }
}

#[cfg(test)]
mod test {

    use super::*;

    fn into_vec<T: Clone>(v: SmallVec<T>) -> Vec<T> {
        v.iter().cloned().collect()
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

}