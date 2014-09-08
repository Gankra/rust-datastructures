// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::{mem, ptr};
use std::slice::Items;
use super::{CAPACITY, EDGE_CAPACITY, MIN_LOAD, SPLIT_LEN};

/// Generate an array of None<$typ>'s of size $count
macro_rules! nones(
    ($typ: ty, $count: expr) => (
        unsafe {
            let mut tmp: [Option<$typ>, .. $count] = mem::uninitialized();
            for i in tmp.as_mut_slice().mut_iter() {
                ptr::write(i, None);
            }
            tmp
        }
    );
)

/// Represents the result of an Insertion: either the item fit, or the node had to split
pub enum InsertionResult<K,V>{
    Fit,
    Split(K, V, Box<Node<K,V>>),
}

/// Represents the result of a search for a key in a single node
pub enum SearchResult {
    Found(uint), GoDown(uint),
}

/// Represents a search path for mutating
pub type SearchStack<K,V> = Vec<(*mut Node<K,V>, uint)>;

/// A B-Tree Node. We keep keys/edges/values separate to optimize searching for keys.
pub struct Node<K,V> {
    pub length: uint,
    // FIXME(Gankro): We use Options here because there currently isn't a safe way to deal
    // with partially initialized [T, ..n]'s. #16998 is one solution to this. Other alternatives
    // include Vec's or heap-allocating a raw buffer of bytes, similar to HashMap's RawTable.
    // However, those solutions introduce an unfortunate extra of indirection (unless the whole
    // node is inlined into this one mega-buffer). We consider this solution to be sufficient for a
    // first-draft, and it has the benefit of being a nice safe starting point to optimize from.
    pub keys: [Option<K>, ..CAPACITY],
    pub edges: [Option<Box<Node<K,V>>>, ..EDGE_CAPACITY],
    pub vals: [Option<V>, ..CAPACITY],
}

impl<K: Ord, V> Node<K,V> {
    /// Searches for the given key in the node. If it finds an exact match,
    /// `Found` will be yielded with the matching index. If it fails to find an exact match,
    /// `Bound` will be yielded with the index of the subtree the key must lie in.
    pub fn search(&self, key: &K) -> SearchResult {
        // FIXME(Gankro): Tune when to search linear or binary when B becomes configurable.
        // For the B configured as of this writing (B = 5), binary search was *singnificantly*
        // worse.
        self.search_linear(key)
    }

    fn search_linear(&self, key: &K) -> SearchResult {
        for (i, k) in self.keys().enumerate() {
            match k.cmp(key) {
                Less => {}, // keep walkin' son, she's too small
                Equal => return Found(i),
                Greater => return GoDown(i),
            }
        }
        GoDown(self.length)
    }
}

impl <K,V> Node<K,V> {
    /// Make a new node
    pub fn new() -> Node<K,V> {
        Node {
            length: 0,
            keys: nones!(K, CAPACITY),
            vals: nones!(V, CAPACITY),
            edges: nones!(Box<Node<K,V>>, CAPACITY + 1),
        }
    }

    /// Make a leaf root from scratch
    pub fn make_leaf_root(key: K, value: V) -> Box<Node<K,V>> {
        let mut node = box Node::new();
        node.insert_fit_as_leaf(0, key, value);
        node
    }

    /// Make an internal root from scratch
    pub fn make_internal_root(key: K, value: V, left: Box<Node<K,V>>, right: Box<Node<K,V>>)
            -> Box<Node<K,V>> {
        let mut node = box Node::new();
        node.keys[0] = Some(key);
        node.vals[0] = Some(value);
        node.edges[0] = Some(left);
        node.edges[1] = Some(right);
        node.length = 1;
        node
    }

    /// Try to insert this key-value pair at the given index in this internal node
    /// If the node is full, we have to split it.
    pub fn insert_as_leaf(&mut self, index: uint, key: K, value: V) -> InsertionResult<K,V> {
        let len = self.length;
        if len < CAPACITY {
            // The element can fit, just insert it
            self.insert_fit_as_leaf(index, key, value);
            Fit
        } else {
            // The element can't fit, this node is full. Split it into two nodes.
            let (new_key, new_val, mut new_right) = self.split();
            let left_len = self.length;

            if index <= left_len {
                self.insert_fit_as_leaf(index, key, value);
            } else {
                new_right.insert_fit_as_leaf(index - left_len - 1, key, value);
            }

            Split(new_key, new_val, new_right)
        }
    }

    /// Try to insert this key-value pair at the given index in this internal node
    /// If the node is full, we have to split it.
    pub fn insert_as_internal(&mut self, index: uint, key: K, value: V, right: Box<Node<K,V>>)
            -> InsertionResult<K,V> {
        let len = self.length;
        if len < CAPACITY {
            // The element can fit, just insert it
            self.insert_fit_as_internal(index, key, value, right);
            Fit
        } else {
            // The element can't fit, this node is full. Split it into two nodes.
            let (new_key, new_val, mut new_right) = self.split();
            let left_len = self.length;

            if index <= left_len {
                self.insert_fit_as_internal(index, key, value, right);
            } else {
                new_right.insert_fit_as_internal(index - left_len - 1, key, value, right);
            }

            Split(new_key, new_val, new_right)
        }
    }

    /// Remove the key-value pair at the given index
    pub fn remove_as_leaf(&mut self, index: uint) -> (K, V) {
        let len = self.length;
        let key = remove_and_shift(self.keys.mut_slice_to(len), index).unwrap();
        let value = remove_and_shift(self.vals.mut_slice_to(len), index).unwrap();
        self.length -= 1;
        (key, value)
    }

    /// Handle an underflow in this node's child. We favour handling "to the left" because we know
    /// we're empty, but our neighbour can be full. Handling to the left means when we choose to
    /// steal, we pop off the end of our neighbour (always fast) and "unshift" ourselves
    /// (always slow, but at least faster since we know we're half-empty).
    /// Handling "to the right" reverses these roles. Of course, we merge whenever possible
    /// because we want dense nodes, and merging is about equal work regardless of direction.
    pub fn handle_underflow(&mut self, underflowed_child_index: uint) {
        if underflowed_child_index > 0 {
            self.handle_underflow_to_left(underflowed_child_index);
        } else {
            self.handle_underflow_to_right(underflowed_child_index);
        }
    }
}

impl<K,V> Node<K,V> {
    /// We have somehow verified that this key-value pair will fit in this internal node,
    /// so insert under that assumption.
    fn insert_fit_as_leaf(&mut self, index: uint, key: K, value: V) {
        let len = self.length;
        shift_and_insert(self.keys.mut_slice_to(len + 1), index, Some(key));
        shift_and_insert(self.vals.mut_slice_to(len + 1), index, Some(value));
        self.length += 1;
    }

    /// We have somehow verified that this key-value pair will fit in this internal node,
    /// so insert under that assumption
    fn insert_fit_as_internal(&mut self, index: uint, key: K, value: V, right: Box<Node<K,V>>) {
        let len = self.length;
        shift_and_insert(self.keys.mut_slice_to(len + 1), index, Some(key));
        shift_and_insert(self.vals.mut_slice_to(len + 1), index, Some(value));
        shift_and_insert(self.edges.mut_slice_to(len + 2), index + 1, Some(right));
        self.length += 1;
    }

    /// Node is full, so split it into two nodes, and yield the middle-most key-value pair
    /// because we have one too many, and our parent now has one too few
    fn split(&mut self) -> (K, V, Box<Node<K, V>>) {
        let mut right = box Node::new();

        steal_last(self.vals.as_mut_slice(), right.vals.as_mut_slice(), SPLIT_LEN);
        steal_last(self.keys.as_mut_slice(), right.keys.as_mut_slice(), SPLIT_LEN);
        // FIXME(Gankro): This isn't necessary for leaf nodes
        steal_last(self.edges.as_mut_slice(), right.edges.as_mut_slice(), SPLIT_LEN + 1);

        // How much each node got
        let left_len = CAPACITY - SPLIT_LEN;
        let right_len = SPLIT_LEN;

        // But we're gonna pop one off the end of the left one, so subtract one
        self.length = left_len - 1;
        right.length = right_len;

        // Pop it
        let key = self.keys[left_len - 1].take().unwrap();
        let val = self.vals[left_len - 1].take().unwrap();

        (key, val, right)
    }

    /// Right is underflowed. Try to steal from left,
    /// but merge left and right if left is low too.
    fn handle_underflow_to_left(&mut self, underflowed_child_index: uint) {
        let left = self.edges[underflowed_child_index - 1].take().unwrap();
        if left.length > MIN_LOAD {
            self.steal_to_left(underflowed_child_index, left);
        } else {
            self.merge_to_left(underflowed_child_index, left);
        }
    }

    /// Left is underflowed. Try to steal from the right,
    /// but merge left and right if right is low too.
    fn handle_underflow_to_right(&mut self, underflowed_child_index: uint) {
        let right = self.edges[underflowed_child_index + 1].take().unwrap();
        if right.length > MIN_LOAD {
            self.steal_to_right(underflowed_child_index, right);
        } else {
            self.merge_to_right(underflowed_child_index, right);
        }
    }

    /// Steal! Stealing is roughly analagous to a binary tree rotation.
    /// In this case, we're "rotating" right.
    fn steal_to_left(&mut self, underflowed_child_index: uint, mut left: Box<Node<K,V>>) {
        let left_len = left.length;
        // Take the biggest stuff off left
        let mut key = remove_and_shift(left.keys.mut_slice_to(left_len), left_len - 1);
        let mut val = remove_and_shift(left.vals.mut_slice_to(left_len), left_len - 1);
        let edge = remove_and_shift(left.edges.mut_slice_to(left_len + 1), left_len);
        left.length -= 1;

        // Swap the parent's seperating key-value pair with left's
        mem::swap(&mut self.keys[underflowed_child_index - 1], &mut key);
        mem::swap(&mut self.vals[underflowed_child_index - 1], &mut val);

        // Put them at the start of right
        {
            let right = self.edges[underflowed_child_index].as_mut().unwrap();
            let right_len = right.length;
            shift_and_insert(right.keys.mut_slice_to(right_len + 1), 0, key);
            shift_and_insert(right.vals.mut_slice_to(right_len + 1), 0, val);
            shift_and_insert(right.edges.mut_slice_to(right_len + 2), 0, edge);
            right.length += 1;
        }

        // Put left back where we found it
        self.edges[underflowed_child_index - 1] = Some(left);
    }

    /// Steal! Stealing is roughly analagous to a binary tree rotation.
    /// In this case, we're "rotating" left.
    fn steal_to_right(&mut self, underflowed_child_index: uint, mut right: Box<Node<K,V>>) {
        let right_len = right.length;
        // Take the smallest stuff off right
        let mut key = remove_and_shift(right.keys.mut_slice_to(right_len), 0);
        let mut val = remove_and_shift(right.vals.mut_slice_to(right_len), 0);
        let edge = remove_and_shift(right.edges.mut_slice_to(right_len + 1), 0);
        right.length -= 1;

        // Swap the parent's seperating key-value pair with right's
        mem::swap(&mut self.keys[underflowed_child_index], &mut key);
        mem::swap(&mut self.vals[underflowed_child_index], &mut val);

        // Put them at the end of left
        {
            let left = self.edges[underflowed_child_index].as_mut().unwrap();
            let left_len = left.length;
            shift_and_insert(left.keys.mut_slice_to(left_len + 1), left_len, key);
            shift_and_insert(left.vals.mut_slice_to(left_len + 1), left_len, val);
            shift_and_insert(left.edges.mut_slice_to(left_len + 2), left_len + 1, edge);
            left.length += 1;
        }

        // Put right back where we found it
        self.edges[underflowed_child_index + 1] = Some(right);
    }

    /// Merge! Left and right will be smooshed into one node, along with the key-value
    /// pair that seperated them in their parent.
    fn merge_to_left(&mut self, underflowed_child_index: uint, mut left: Box<Node<K,V>>) {
        let len = self.length;

        // Permanently remove left's index, and the key-value pair that seperates
        // left and right
        let key = remove_and_shift(self.keys.mut_slice_to(len), underflowed_child_index - 1);
        let val = remove_and_shift(self.vals.mut_slice_to(len), underflowed_child_index - 1);
        remove_and_shift(self.edges.mut_slice_to(len + 1), underflowed_child_index - 1);

        self.length -= 1;

        // Give left right's stuff, and put left where right was. Note that all the indices
        // in the parent have been shifted left at this point.
        let right = self.edges[underflowed_child_index - 1].take().unwrap();
        left.absorb(key, val, right);
        self.edges[underflowed_child_index - 1] = Some(left);
    }

    /// Merge! Left and right will be smooshed into one node, along with the key-value
    /// pair that seperated them in their parent.
    fn merge_to_right(&mut self, underflowed_child_index: uint, right: Box<Node<K,V>>) {
        let len = self.length;

        // Permanently remove right's index, and the key-value pair that seperates
        // left and right
        let key = remove_and_shift(self.keys.mut_slice_to(len), underflowed_child_index);
        let val = remove_and_shift(self.vals.mut_slice_to(len), underflowed_child_index);
        remove_and_shift(self.edges.mut_slice_to(len + 1), underflowed_child_index + 1);

        self.length -= 1;

        // Give left right's stuff. Note that unlike handle_underflow_to_left, we don't need
        // to compensate indices, and we don't need to put left "back".
        let left = self.edges[underflowed_child_index].as_mut().unwrap();
        left.absorb(key, val, right);
    }

    /// Take all the values from right, seperated by the given key and value
    fn absorb(&mut self, key: Option<K>, value: Option<V>, mut right: Box<Node<K,V>>) {
        let len = self.length;
        let r_len = right.length;

        self.keys[len] = key;
        self.vals[len] = value;

        merge(self.keys.mut_slice_to(len + r_len + 1), right.keys.mut_slice_to(r_len));
        merge(self.vals.mut_slice_to(len + r_len + 1), right.vals.mut_slice_to(r_len));
        merge(self.edges.mut_slice_to(len + r_len +  2), right.edges.mut_slice_to(r_len + 1));

        self.length += r_len + 1;
    }

    /// An iterator for the keys of this node
    fn keys<'a>(&'a self) -> Keys<'a, K> {
        Keys{ it: self.keys.iter() }
    }
}

/// Subroutine for removal. Takes a search stack for a key that terminates at an
/// internal node, and makes it mutates the tree and search stack to make it a search
/// stack for that key that terminates at a leaf. This leaves the tree in an inconsistent
/// state that must be repaired by the caller by removing the key in question.
pub fn leafify_stack<K,V>(stack: &mut SearchStack<K,V>) {
    let (node_ptr, index) = stack.pop().unwrap();
    unsafe {
        // First, get ptrs to the found key-value pair
        let node = &mut *node_ptr;
        let (key_ptr, val_ptr) = {
            (&mut node.keys[index] as *mut _, &mut node.vals[index] as *mut _)
        };

        // Go into the right subtree of the found key
        stack.push((node_ptr, index + 1));
        let mut temp_node = &mut **node.edges[index + 1].as_mut().unwrap();

        loop {
            // Walk into the smallest subtree of this
            let node = temp_node;
            let node_ptr = node as *mut _;
            stack.push((node_ptr, 0));
            let next = node.edges[0].as_mut();
            if next.is_some() {
                // This node is internal, go deeper
                temp_node = &mut **next.unwrap();
            } else {
                // This node is a leaf, do the swap and return
                mem::swap(&mut *key_ptr, &mut node.keys[0]);
                mem::swap(&mut *val_ptr, &mut node.vals[0]);
                break;
            }
        }
    }
}

/// Basically `Vec.insert(index)`. Assumes that the last element in the slice is
/// Somehow "empty" and can be overwritten.
fn shift_and_insert<T>(slice: &mut [T], index: uint, elem: T) {
    unsafe {
        let start = slice.as_mut_ptr().offset(index as int);
        let len = slice.len();
        if index < len - 1 {
            ptr::copy_memory(start.offset(1), start as *const _, len - index - 1);
        }
        ptr::write(start, elem);
    }
}

/// Basically `Vec.remove(index)`.
fn remove_and_shift<T>(slice: &mut [Option<T>], index: uint) -> Option<T> {
    unsafe {
        let first = slice.as_mut_ptr();
        let start = first.offset(index as int);
        let result = ptr::read(start as *const _);
        let len = slice.len();
        if len > 1 && index < len - 1 {
            ptr::copy_memory(start, start.offset(1) as *const _, len - index - 1);
        }
        ptr::write(first.offset((len - 1) as int), None);
        result
    }
}

/// Subroutine for splitting a node. Put the `SPLIT_LEN` last elements from left,
/// (which should be full) and put them at the start of right (which should be empty)
fn steal_last<T>(left: &mut[T], right: &mut[T], amount: uint) {
    // Is there a better way to do this?
    // Maybe copy_nonoverlapping_memory and then bulk None out the old Location?
    let offset = left.len() - amount;
    for (a,b) in left.mut_slice_from(offset).mut_iter()
            .zip(right.mut_slice_to(amount).mut_iter()) {
        mem::swap(a, b);
    }
}

/// Subroutine for merging the contents of right into left
/// Assumes left has space for all of right
fn merge<T>(left: &mut[T], right: &mut[T]) {
    let offset = left.len() - right.len();
    for (a,b) in left.mut_slice_from(offset).mut_iter()
            .zip(right.mut_iter()) {
        mem::swap(a, b);
    }
}


struct Keys<'a, K>{
    it: Items<'a, Option<K>>
}

impl<'a, K> Iterator<&'a K> for Keys<'a, K> {
    fn next(&mut self) -> Option<&'a K> {
        self.it.next().and_then(|x| x.as_ref())
    }
}