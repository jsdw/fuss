use std::sync::Arc;

#[derive(Clone,Debug,PartialEq)]
struct Item<T> {
    item: T,
    next: Arc<Link<T>>
}

#[derive(Clone,Debug,PartialEq)]
enum Link<T> {
    Empty,
    Elem(Item<T>)
}

/// An immutable singly linked list.
#[derive(Clone,Debug,PartialEq)]
pub struct List<T> {
    head: Arc<Link<T>>
}

impl<T> List<T> {

    /// create a new, empty linked list.
    pub fn new() -> List<T> {
        List{
            head: Arc::new(Link::Empty)
        }
    }

    /// return a new linked list with the provided
    /// item added to the front.
    pub fn push(&self, item: T) -> List<T> {
        List{
            head: Arc::new(Link::Elem(Item{
                item: item,
                next: self.head.clone()
            }))
        }
    }

    // /// return a new linked list with the front
    // /// item removed.
    // pub fn pop(&self) -> List<T> {
    //     match *self.head {
    //         Link::Empty => List{
    //             head: self.head.clone()
    //         },
    //         Link::Elem(ref item) => List{
    //             head: item.next.clone()
    //         }
    //     }
    // }

    /// iterate over the items in the list
    pub fn iter(&self) -> ListIter<T> {
        ListIter{
            remaining: &self.head
        }
    }

}

pub struct ListIter<'a, T: 'a> {
    remaining: &'a Link<T>
}

impl<'a, T> Iterator for ListIter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        match *self.remaining {
            Link::Empty => None,
            Link::Elem(ref item) => {
                self.remaining = &*item.next;
                Some(&item.item)
            }
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_push_pop() {

        let a = List::new();
        let b = a.push(1);
        let c = b.push(2);
        let d = c.push(3);

        assert_eq!(d.iter().cloned().collect::<Vec<_>>(), vec![3,2,1]);
        assert_eq!(c.iter().cloned().collect::<Vec<_>>(), vec![2,1]);
        assert_eq!(b.iter().cloned().collect::<Vec<_>>(), vec![1]);
        assert_eq!(a.iter().cloned().collect::<Vec<_>>(), vec![]);

        // let c2 = d.pop();
        // let b2 = c.pop();
        // let a2 = a.pop();
        // let a3 = a.pop();

        // assert_eq!(c, c2);
        // assert_eq!(b, b2);
        // assert_eq!(a, a2);
        // assert_eq!(a, a3);

    }

}