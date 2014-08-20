use std;

struct Node<T> {
    elem: T,
    next: Option<Box<Node<T>>>,
}

impl<T> Node<T> {
    fn new(elem: T) -> Node<T> {
        Node { elem: elem, next: None }
    }
}

pub struct Items<'a, T> {
    head: Option<&'a Node<T>>,
    nelem: uint,
}

pub struct MutItems<'a, T> {
    head: Option<&'a mut Node<T>>,
    nelem: uint,
}

#[deriving(Clone)]
pub struct MoveItems<T> {
    list: SinglyLinkedList<T>,
}

pub struct SinglyLinkedList <T> {
    front: Option<Box<Node<T>>>,
    back: *mut Node<T>,
    length: uint,
}

impl <T> SinglyLinkedList <T> {
    pub fn new () -> SinglyLinkedList <T> {
        SinglyLinkedList{ front: None, back: RawPtr::null(), length: 0 }
    }

    pub fn push_back (&mut self, elem: T) {
        let mut new_node = box Node::new(elem);
        self.back = &mut *new_node as *mut _;

        if self.back.is_null() {
            self.front = Some(new_node);
        } else {
            let back = unsafe { &mut *self.back };
            back.next = Some(new_node);
        }

        self.length += 1;
    }

    pub fn push_front (&mut self, elem: T) {
        let mut new_node = box Node::new(elem);

        if self.length == 0 {
            self.back = &mut *new_node as *mut _;
        }

        new_node.next = self.front.take();
        self.front = Some(new_node);

        self.length += 1;
    }

    pub fn pop_front (&mut self) -> Option<T> {
        self.length -= 1;
        match self.front.take() {
            None => None,
            Some(box front) => {
                self.front = front.next;
                if self.length == 0 {
                    self.back = RawPtr::null();
                }
                Some(front.elem)
            }
        }
    }

    pub fn peek_front <'a> (&'a self) -> Option<&'a T> {
        self.front.as_ref().map(|front| &front.elem)
    }

    pub fn peek_front_mut <'a> (&'a mut self) -> Option<&'a mut T> {
        self.front.as_mut().map(|front| &mut front.elem)
    }

    pub fn peek_back <'a> (&'a self) -> Option<&'a T> {
        unsafe { self.back.to_option().map(|back| &back.elem) }
    }

    pub fn peek_back_mut <'a> (&'a mut self) -> Option<&'a mut T> {
        if self.back.is_null() {
            None
        } else {
            unsafe { Some(&mut (*self.back).elem) }
        }
    }

    pub fn iter <'a> (&'a self) -> Items<'a, T> {
        Items{ head: self.front.as_ref().map(|x| &**x), nelem: self.len() }
    }

    pub fn mut_iter <'a> (&'a mut self) -> MutItems<'a, T> {
        let len = self.len();
        MutItems{ head: self.front.as_mut().map(|x| &mut **x), nelem: len }
    }

    pub fn move_iter (self) -> MoveItems<T> {
        MoveItems { list: self }
    }
}

impl <T> Collection for SinglyLinkedList <T> {
    fn len (&self) -> uint {
        self.length
    }
}

impl <T> Mutable for SinglyLinkedList <T> {
    fn clear (&mut self) {
        // don't want to blow the stack with destructors!
        while !self.is_empty() {
            self.pop_front();
        }
    }
}

#[unsafe_destructor]
impl <T> Drop for SinglyLinkedList <T> {
    fn drop (&mut self) {
        self.clear();
    }
}

impl<'a, T> Iterator<&'a T> for Items<'a, T> {
    #[inline]
    fn next(&mut self) -> Option<&'a T> {
        match self.head.take() {
            None => None,
            Some(head) => {
                self.nelem -= 1;
                self.head = head.next.as_ref().map(|next| &**next);
                Some(&head.elem)
            }
        }
    }

    #[inline]
    fn size_hint(&self) -> (uint, Option<uint>) {
        (self.nelem, Some(self.nelem))
    }
}

// FIXME #11820: the &'a Option<> of the Link stops clone working.
impl<'a, T> Clone for Items<'a, T> {
    fn clone(&self) -> Items<'a, T> { *self }
}

impl<'a, T> Iterator<&'a mut T> for MutItems<'a, T> {
    #[inline]
    fn next(&mut self) -> Option<&'a mut T> {
        match self.head.take() {
            None => None,
            Some(head) => {
                self.nelem -= 1;
                self.head = head.next.as_mut().map(|next| &mut **next);
                Some(&mut head.elem)
            }
        }
    }

    #[inline]
    fn size_hint(&self) -> (uint, Option<uint>) {
        (self.nelem, Some(self.nelem))
    }
}

impl <T> Iterator<T> for MoveItems<T> {
    #[inline]
    fn next(&mut self) -> Option<T> {
        self.list.pop_front()
    }

    #[inline]
    fn size_hint(&self) -> (uint, Option<uint>) {
        let len = self.list.len();
        (len, Some(len))
    }
}

impl<T> FromIterator<T> for SinglyLinkedList<T> {
    fn from_iter<I: Iterator<T>>(iterator: I) -> SinglyLinkedList<T> {
        let mut ret = SinglyLinkedList::new();
        ret.extend(iterator);
        ret
    }
}

impl<T> Extendable<T> for SinglyLinkedList<T> {
    fn extend<I: Iterator<T>>(&mut self, mut iterator: I) {
        for elt in iterator { self.push_back(elt); }
    }
}

impl<T: PartialEq> PartialEq for SinglyLinkedList<T> {
    fn eq(&self, other: &SinglyLinkedList<T>) -> bool {
        self.len() == other.len() &&
            std::iter::order::eq(self.iter(), other.iter())
    }

    fn ne(&self, other: &SinglyLinkedList<T>) -> bool {
        self.len() != other.len() ||
            std::iter::order::ne(self.iter(), other.iter())
    }
}

impl<T: PartialOrd> PartialOrd for SinglyLinkedList<T> {
    fn partial_cmp(&self, other: &SinglyLinkedList<T>) -> Option<Ordering> {
        std::iter::order::partial_cmp(self.iter(), other.iter())
    }
}

impl<T: Clone> Clone for SinglyLinkedList<T> {
    fn clone(&self) -> SinglyLinkedList<T> {
        self.iter().map(|x| x.clone()).collect()
    }
}

impl<T: std::fmt::Show> std::fmt::Show for SinglyLinkedList<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        try!(write!(f, "["));

        for (i, e) in self.iter().enumerate() {
            if i != 0 { try!(write!(f, ", ")); }
            try!(write!(f, "{}", *e));
        }

        write!(f, "]")
    }
}

fn main(){}