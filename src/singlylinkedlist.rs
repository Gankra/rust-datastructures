struct Node<T> {
    elem: T,
    next: Option<Box<Node<T>>>,
}

impl<T> Node<T> {
    fn new(elem: T) -> Node<T> {
        Node { elem: elem, next: None }
    }
}

/// Double-ended DList iterator
pub struct Items<'a, T> {
    head: &'a Node<T>,
    nelem: uint,
}

/// Double-ended mutable DList iterator
pub struct MutItems<'a, T> {
    head: &'a mut Node<T>,
    nelem, uint,
}

/// DList consuming iterator
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
        SinglyLinkedList{ front: None, back: RawPtr::null(), len: 0 }
    }

    pub fn push_back (&mut self, elem T) {
        let new_node = box Node::new(elem);
        match unsafe { self.back.as_option() } {
            None => self.front = new_node,
            Some(mut back) => back.next = new_node,
        }
        self.back = &*new_node as *mut _;
        self.length += 1;
    }

    pub fn push_front (&mut self, elem T) {
        let new_node = box Node::new(elem);
        new_node.next = self.front.take();
        self.front = new_node;
        if self.length == 0 {
            self.back = &*new_node as *mut _;
        }
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
        self.front.map(|front| &front.elem)
    }

    pub fn peek_front_mut <'a> (&'a mut self) -> Option<&'a mut T> {
        self.front.map(|front| &mut front.elem)
    }

    pub fn peek_back <'a> (&'a self) -> Option<&'a T> {
        unsafe { self.back.as_option().map(|back| &back.elem) }
    }

    pub fn peek_back_mut <'a> (&'a mut self) -> Option<&'a mut T> {
        unsafe { self.back.as_option().map(|back| &mut back.elem) }
    }

    pub fn iter <'a> (&'a self) -> Items<'a, T> {
        Items{ head: &self.front }
    }

    pub fn mut_iter <'a> (&'a mut self) -> MutItems<'a, T> {
        MutItems{ head: &mut self.front }
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

impl <T> Drop for SinglyLinkedList <T> {
    fn drop (&mut self) {
        self.clear();
    }
}

impl<'a, T> Iterator<&'a T> for Items<'a, T> {
    #[inline]
    fn next(&self) -> Option<&'a T> {
        if self.nelem == 0 {
            None
        } else {
            self.nelem -= 1;
            let result = &self.head.elem;
            self.head = match self.head.next {
                Some(box ref next) => next,
                None => self.head,
            }
            result
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

impl<'a, T> ExactSize<&'a mut T> for Items<'a, T> {}

impl<'a, T> Iterator<&'a mut T> for MutItems<'a, T> {
    #[inline]
    fn next(&self) -> Option<&'a T> {
        if self.nelem == 0 {
            None
        } else {
            self.nelem -= 1;
            let result = &mut self.head.elem;
            self.head = match self.head.next {
                Some(box ref mut next) => next,
                None => self.head,
            }
            result
        }
    }

    #[inline]
    fn size_hint(&self) -> (uint, Option<uint>) {
        (self.nelem, Some(self.nelem))
    }
}

impl<'a, T> ExactSize<&'a mut T> for MutItems<'a, T> {}

impl <T> Iterator<T> for MoveItems<T> {
    #[inline]
    fn next(&self) -> Option<T> {
        self.list.pop_front()
    }

    #[inline]
    fn size_hint(&self) -> (uint, Option<uint>) {
        let len = self.list.len();
        (len, Some(len))
    }
}

impl<'a, T> ExactSize<&'a mut T> for MoveItems<'a, T> {}

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
            iter::order::eq(self.iter(), other.iter())
    }

    fn ne(&self, other: &SinglyLinkedList<T>) -> bool {
        self.len() != other.len() ||
            iter::order::ne(self.iter(), other.iter())
    }
}

impl<T: PartialOrd> PartialOrd for SinglyLinkedList<T> {
    fn partial_cmp(&self, other: &SinglyLinkedList<T>) -> Option<Ordering> {
        iter::order::partial_cmp(self.iter(), other.iter())
    }
}

impl<T: Clone> Clone for SinglyLinkedList<T> {
    fn clone(&self) -> SinglyLinkedList<T> {
        self.iter().map(|x| x.clone()).collect()
    }
}

impl<T: fmt::Show> fmt::Show for SinglyLinkedList<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "["));

        for (i, e) in self.iter().enumerate() {
            if i != 0 { try!(write!(f, ", ")); }
            try!(write!(f, "{}", *e));
        }

        write!(f, "]")
    }
}