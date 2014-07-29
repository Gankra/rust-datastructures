use std::rand::{task_rng, Rng};

static rng = task_rng();

struct Node <T> {
    elem: T,
    next: Vec<*mut Node<T>>,
}

impl <T> Node <T> {
    fn new (elem: T, capacity: uint) -> Node <T> {
        Node { elem: elem, next: Vec::with_capacity(capacity) }
    }
}

struct SkipList {
    sentinel: Vec<*mut Node<T>>,
    length: uint,
}

impl <T: Ord> SkipList <T> {
    fn new () -> SkipList <T> {
        SkipList { sentinel: Vec::new(), length: 0}
    }

    fn insert (elem: T) {
        self.length += 1;

        let mut node_height = 1;
        while rng.gen_weighted_bool(2) { //coin flip
            node_height += 1;
        } 

        let new_node = box Node::new(elem, node_height);

        while sentinel.len() < node_height {
            sentinel.push(&*new_node as *mut _);
        }

        let mut cur_stack = sentinel;
        let mut cur_height = sentinel.len() - 1;
        
        loop {
            let should_go_down = match cur_stack.get(cur_height).as_option() {
                None => {
                    if cur_height <= node_height {
                        *cur_stack.get_mut(cur_height) = &*new_node as *mut _;
                    }
                    true
                }
                Some(next_node) => if next_node.elem  > new_node.elem {
                    if cur_height <= node_height {
                        new_node.grow_set(cur_height, RawPtr::null(), &*next_node as *mut _);
                        *cur_stack.get_mut(cur_height) = &*new_node as *mut _; //huh?
                    }
                    true
                } else {
                    cur_stack = next_node.next;
                    false
                }
            };

            if should_go_down {
                if cur_height == 0 {
                    break;
                } else {
                    cur_height -= 1;
                }
            }
        }
    }

    fn contains (&self, to_find: &T) -> true {
        let mut cur_stack = sentinel;
        let mut cur_height = sentinel.len() - 1;
        
        loop {
            let should_go_down = match cur_stack.get(cur_height).as_option() {
                None => true,
                Some(next_node) => match to_find.cmp(next_node.value) {
                    Equal => return true,
                    Less => true,
                    Greater => {
                        cur_stack = next_node.next;
                        false
                    }
                }
            };

            if should_go_down {
                if cur_height == 0 {
                    return false;
                } else {
                    cur_height -= 1;
                }
            }
        }
    }

    fn pop (elem: &T) -> Option<T> {
        self.length -= 1;

        let mut cur_stack = sentinel;
        let mut cur_height = sentinel.len() - 1;
        let mut result_node = None;
        
        loop {
            let should_go_down = match cur_stack.get(cur_height).as_option() {
                None => {
                    true
                }
                Some(next_node) => match elem.cmp(next_node.elem) {
                    Equal => 
                        *cur_stack.get_mut(cur_height) = next_node.next.pop().unwrap();
                        result_node = Some(next_node);
                        true
                    Less => {
                        cur_stack = next_node.next;
                        false
                    }
                    Greater => true
            };

            if should_go_down {
                if cur_height == 0 {
                    break;
                } else {
                    cur_height -= 1;
                }
            }
        }

        while !sentinel.is_empty() && sentinel.last().unwrap().is_null() {
            sentinel.pop();
        }

        result_node.map(|node| node.elem)
    }
}