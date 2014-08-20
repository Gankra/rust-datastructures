use std::collections::dlist;
use std::collections::ringbuf;

use std::collections::{RingBuf, DList, Deque, MutableSeq};

pub struct BlockedList <T> {
    list: DList<RingBuf<T>>,
    block_size: uint,
    length: uint,
}

pub struct Items <'a, T> {
    list_iter: dlist::Items<'a, RingBuf<T>>,
    block_iter: Option<ringbuf::Items<'a, T>>,
}

impl <T> BlockedList<T> {
    pub fn new() -> BlockedList<T> {
        BlockedList::with_block_size(100)
    }

    pub fn with_block_size(block_size: uint) -> BlockedList<T> {
        if block_size == 0 { fail!("Blocks must have positive size") }
        BlockedList { list: DList::new(), block_size: block_size, length: 0 }
    }
}

impl <T> BlockedList<T> {
    pub fn iter<'a> (&'a self) -> Items<'a, T> {
        let mut list_iter = self.list.iter();
        let block_iter = list_iter.next().map(|x| x.iter());
        Items{ list_iter: list_iter, block_iter: block_iter }
    }

    // TODO intelligently iterate forwards or backwards
    pub fn get<'a>(&'a self, index: uint) -> Option<&'a T> {
        if index >= self.length {
            None
        } else {
            let mut cur_index = 0;
            for block in self.list.iter() {
                let block_len = block.len();
                if cur_index + block_len > index {
                    return Some(block.get(index - cur_index));
                } else {
                    cur_index += block_len;
                }
            }
            unreachable!();
        }
    }

    pub fn get_mut<'a>(&'a mut self, index: uint) -> Option<&'a mut T> {
        if index >= self.length {
            None
        } else {
            let mut cur_index = 0;
            for block in self.list.iter() {
                let block_len = block.len();
                if cur_index + block_len > index {
                    return Some(block.get_mut(index - cur_index));
                } else {
                    cur_index += block_len;
                }
            }
            unreachable!();
        }
    }

    pub fn insert<'a>(&mut self, index: uint, value: T) {
        if index > self.length { fail!("Index out of bounds"); }

        let mut cur_index = 0;
        for block in self.list.iter() {
            let block_len = block.len();
            if cur_index + block_len > index {
                return Some(block.insert(index - cur_index));
            } else {
                cur_index += block_len;
            }
        }

        unreachable!();
    }

}

fn make_block <T> (size: uint) -> RingBuf<T> {
    RingBuf::with_capacity(size)
}

impl <T> Deque <T> for BlockedList<T> {
    fn front <'a> (&'a self) -> Option<&'a T> {
        self.list.front().and_then(|x| x.front())
    }

    fn front_mut <'a> (&'a mut self) -> Option<&'a mut T> {
        self.list.front_mut().and_then(|x| x.front_mut())
    }

    fn back <'a> (&'a self) -> Option<&'a T> {
        self.list.back().and_then(|x| x.back())
    }

    fn back_mut <'a> (&'a mut self) -> Option<&'a mut T> {
        self.list.back_mut().and_then(|x| x.back_mut())
    }

    fn push_front(&mut self, elt: T) {
        self.length += 1;
        let block_size = self.block_size;
        let should_grow = match self.list.front_mut() {
            None => true,
            Some(block) => block.len() == block_size
        };

        if should_grow {
            let mut block = make_block(block_size);
            block.push_front(elt);
            self.list.push_front(block);
        } else {
            self.list.front_mut().unwrap().push_front(elt);
        }
    }

    fn pop_front(&mut self) -> Option<T> {
        let (result, should_pop) = match self.list.front_mut() {
            None => (None, false),
            Some(block) => {
                self.length -= 1;
                let result = block.pop_front();
                if block.is_empty() {
                    (result, true)
                } else {
                    (result, false)
                }
            }
        };

        if should_pop {
            self.list.pop_front();
        }

        result
    }
}

impl <T> MutableSeq<T> for BlockedList<T> {
    fn push(&mut self, elt: T) {
        self.length += 1;
        let block_size = self.block_size;
        let should_grow = match self.list.back_mut() {
            None => true,
            Some(block) => block.len() == block_size
        };

        if should_grow {
            let mut block = make_block(block_size);
            block.push(elt);
            self.list.push(block);
        } else {
            self.list.back_mut().unwrap().push(elt);
        }
    }

    fn pop(&mut self) -> Option<T> {
        let (result, should_pop) = match self.list.front_mut() {
            None => (None, false),
            Some(block) => {
                self.length -= 1;
                let result = block.pop();
                if block.is_empty() {
                    (result, true)
                } else {
                    (result, false)
                }
            }
        };

        if should_pop {
            self.list.pop();
        }

        result
    }
}

impl <T> Collection for BlockedList<T> {
    fn len(&self) -> uint {
        self.length
    }
}

impl <T> Mutable for BlockedList<T> {
    fn clear (&mut self) {
        self.list.clear();
    }
}

// Note: DoubleEndedIterator would have serious ownership issues to do effeciently and safely
impl <'a, T> Iterator<&'a T> for Items<'a, T> {
    fn next(&mut self) -> Option<&'a T> {
        self.block_iter.take().and_then(|mut block_iter| {
            match block_iter.next() {
                None => {
                    self.block_iter = self.list_iter.next().map(|x| x.iter());
                    self.block_iter.and_then(|mut iter| iter.next())
                }
                result @ Some(_) => result
            }
        })
    }
}

fn main () {

}