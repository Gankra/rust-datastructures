use std::rand::{task_rng, Rng, TaskRng};
use std::mem::transmute;

fn main () {} //TODO: remove this when production ready

struct Node <T> {
    elem: T,
    next: Vec<*mut Node<T>>,
}

impl <T> Node <T> {
    fn new (elem: T, capacity: uint) -> Node <T> {
        Node { elem: elem, next: Vec::with_capacity(capacity) }
    }
}

//Euuugh this is harder to implement than I expected
pub struct SkipList<T> {
    sentinel: Vec<*mut Node<T>>,
    length: uint,
    rng: TaskRng,
}

impl <T: Ord> SkipList <T> {
    pub fn new () -> SkipList <T> {
        SkipList { sentinel: Vec::new(), length: 0, rng: task_rng()}
    }

    pub fn insert (&mut self, elem: T) {
        self.length += 1;

        let mut node_height = 1;
        while self.rng.gen_weighted_bool(2) { //coin flip
            node_height += 1;
        }

        let new_node: &mut Node<T> = unsafe { transmute(box Node::new(elem, node_height)) };
        let new_node_ptr = new_node as *mut _;

        while self.sentinel.len() < node_height {
            self.sentinel.push(new_node);
        }

        let mut cur_stack = &mut self.sentinel;
        let mut cur_height = cur_stack.len() - 1;

        loop {
            let next_ptr = (*cur_stack)[cur_height];
            let should_go_down = if next_ptr.is_null() {
                if cur_height <= node_height {
                    *cur_stack.get_mut(cur_height) = new_node_ptr;
                }
                true
            } else {
                let next_node = unsafe{ &mut*next_ptr };
                if next_node.elem  > new_node.elem {
                    if cur_height <= node_height {
                        new_node.next.grow_set(cur_height, &RawPtr::null(), next_node as *mut _);
                        *cur_stack.get_mut(cur_height) = new_node_ptr;
                    }
                    true
                } else {
                    cur_stack = &mut next_node.next;
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

    pub fn contains (&self, to_find: &T) -> bool {
        let mut cur_stack = &self.sentinel;
        let mut cur_height = cur_stack.len() - 1;

        loop {
            let next_ptr = (*cur_stack)[cur_height];
            let should_go_down = if next_ptr.is_null() {
                true
            } else {
                let next_node = unsafe { &mut *next_ptr };
                match to_find.cmp(&next_node.elem) {
                    Equal => return true,
                    Less => true,
                    Greater => {
                        cur_stack = &next_node.next;
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

    pub fn pop (&mut self, elem: &T) -> Option<T> {
        self.length -= 1;

        let result_node = {
            let mut cur_stack = &mut self.sentinel;
            let mut cur_height = cur_stack.len() - 1;
            let mut result_node:Option<Box<Node<T>>> = None;

            loop {
                let next_ptr = (*cur_stack)[cur_height];
                let should_go_down = if next_ptr.is_null() {
                    true
                } else {
                    let next_node = unsafe{ &mut *next_ptr };
                    match elem.cmp(&next_node.elem) {
                        Equal => {
                            *cur_stack.get_mut(cur_height) = next_node.next.pop().unwrap();
                            if cur_height == 0 {
                                result_node = unsafe { Some(transmute(next_ptr)) };
                            }
                            true
                        }
                        Less => {
                            cur_stack = &mut next_node.next;
                            false
                        }
                        Greater => true
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

            result_node
        };

        while !self.sentinel.is_empty() && self.sentinel.last().unwrap().is_null() {
            self.sentinel.pop();
        }

        result_node.map(|node| node.elem)
    }
}