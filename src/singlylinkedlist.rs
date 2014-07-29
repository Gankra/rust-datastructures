struct Node <T> {
    elem: T,
    next: Option<Box<Node<T>>>,
}

impl <T> Node <T> {
    fn new(elem: T) -> Node<T> {
        Node { elem: elem, next: None }
    }
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