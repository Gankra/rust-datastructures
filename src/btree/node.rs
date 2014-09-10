// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::mem;

/// Represents the result of an Insertion: either the item fit, or the node had to split
pub enum InsertionResult<K, V>{
    Fit,
    Split(K, V, Node<K, V>),
}

/// Represents the result of a search for a key in a single node
pub enum SearchResult {
    Found(uint), GoDown(uint),
}

/// Represents a search path for mutating
pub type SearchStack<K, V> = Vec<(*mut Node<K, V>, uint)>;

/// A B-Tree Node. We keep keys/edges/values separate to optimize searching for keys.
pub struct Node<K, V> {
    pub keys: Vec<K>,
    pub edges: Vec<Node<K, V>>,
    pub vals: Vec<V>,
}

impl<K: Ord, V> Node<K, V> {
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
        for (i, k) in self.keys.iter().enumerate() {
            match k.cmp(key) {
                Less => {}, // keep walkin' son, she's too small
                Equal => return Found(i),
                Greater => return GoDown(i),
            }
        }
        GoDown(self.len())
    }
}

impl <K, V> Node<K, V> {
    /// Make a new node
    pub fn new_internal(capacity: uint) -> Node<K, V> {
        Node {
            keys: Vec::with_capacity(capacity),
            vals: Vec::with_capacity(capacity),
            edges: Vec::with_capacity(capacity + 1),
        }
    }

    pub fn new_leaf(capacity: uint) -> Node<K, V> {
        Node {
            keys: Vec::with_capacity(capacity),
            vals: Vec::with_capacity(capacity),
            edges: Vec::new(),
        }
    }

    pub fn from_vecs(keys: Vec<K>, vals: Vec<V>, edges: Vec<Node<K, V>>) -> Node<K, V> {
        Node {
            keys: keys,
            vals: vals,
            edges: edges,
        }
    }

    /// Make a leaf root from scratch
    pub fn make_leaf_root(b:uint, key: K, value: V) -> Node<K, V> {
        let mut node = Node::new_leaf(capacity_from_b(b));
        node.insert_fit_as_leaf(0, key, value);
        node
    }

    /// Make an internal root from scratch
    pub fn make_internal_root(b: uint, key: K, value: V, left: Node<K, V>, right: Node<K, V>)
            -> Node<K, V> {
        let mut node = Node::new_internal(capacity_from_b(b));
        node.keys.push(key);
        node.vals.push(value);
        node.edges.push(left);
        node.edges.push(right);
        node
    }

    pub fn len(&self) -> uint {
        self.keys.len()
    }

    pub fn capacity(&self) -> uint {
        self.keys.capacity()
    }

    pub fn is_leaf(&self) -> bool {
        self.edges.is_empty()
    }

    pub fn is_underfull(&self) -> bool {
        self.len() < min_load_from_capacity(self.capacity())
    }

    pub fn is_full(&self) -> bool {
        self.len() == self.capacity()
    }

    /// Try to insert this key-value pair at the given index in this internal node
    /// If the node is full, we have to split it.
    pub fn insert_as_leaf(&mut self, index: uint, key: K, value: V) -> InsertionResult<K, V> {
        if !self.is_full() {
            // The element can fit, just insert it
            self.insert_fit_as_leaf(index, key, value);
            Fit
        } else {
            // The element can't fit, this node is full. Split it into two nodes.
            let (new_key, new_val, mut new_right) = self.split();
            let left_len = self.len();

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
    pub fn insert_as_internal(&mut self, index: uint, key: K, value: V, right: Node<K, V>)
            -> InsertionResult<K, V> {
        if !self.is_full() {
            // The element can fit, just insert it
            self.insert_fit_as_internal(index, key, value, right);
            Fit
        } else {
            // The element can't fit, this node is full. Split it into two nodes.
            let (new_key, new_val, mut new_right) = self.split();
            let left_len = self.len();

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
        match (self.keys.remove(index), self.vals.remove(index)) {
            (Some(k), Some(v)) => (k, v),
            _ => unreachable!(),
        }
    }

    /// Handle an underflow in this node's child. We favour handling "to the left" because we know
    /// we're empty, but our neighbour can be full. Handling to the left means when we choose to
    /// steal, we pop off the end of our neighbour (always fast) and "unshift" ourselves
    /// (always slow, but at least faster since we know we're half-empty).
    /// Handling "to the right" reverses these roles. Of course, we merge whenever possible
    /// because we want dense nodes, and merging is about equal work regardless of direction.
    pub fn handle_underflow(&mut self, underflowed_child_index: uint) {
        unsafe {
            if underflowed_child_index > 0 {
                self.handle_underflow_to_left(underflowed_child_index);
            } else {
                self.handle_underflow_to_right(underflowed_child_index);
            }
        }
    }
}

impl<K, V> Node<K, V> {
    /// We have somehow verified that this key-value pair will fit in this internal node,
    /// so insert under that assumption.
    fn insert_fit_as_leaf(&mut self, index: uint, key: K, val: V) {
        self.keys.insert(index, key);
        self.vals.insert(index, val);
    }

    /// We have somehow verified that this key-value pair will fit in this internal node,
    /// so insert under that assumption
    fn insert_fit_as_internal(&mut self, index: uint, key: K, val: V, right: Node<K, V>) {
        self.keys.insert(index, key);
        self.vals.insert(index, val);
        self.edges.insert(index + 1, right);
    }

    /// Node is full, so split it into two nodes, and yield the middle-most key-value pair
    /// because we have one too many, and our parent now has one too few
    fn split(&mut self) -> (K, V, Node<K, V>) {
        let split_len = split_len_from_capacity(self.capacity());
        let r_keys = split(&mut self.keys, split_len);
        let r_vals = split(&mut self.vals, split_len);
        let r_edges = if self.edges.is_empty() {
            Vec::new()
        } else {
            split(&mut self.edges, split_len + 1)
        };

        let right = Node::from_vecs(r_keys, r_vals, r_edges);
        // Pop it
        let key = self.keys.pop().unwrap();
        let val = self.vals.pop().unwrap();

        (key, val, right)
    }

    /// Right is underflowed. Try to steal from left,
    /// but merge left and right if left is low too.
    unsafe fn handle_underflow_to_left(&mut self, underflowed_child_index: uint) {
        let left_len = self.edges[underflowed_child_index - 1].len();
        if left_len > min_load_from_capacity(self.capacity()) {
            self.steal_to_left(underflowed_child_index);
        } else {
            self.merge_children(underflowed_child_index - 1);
        }
    }

    /// Left is underflowed. Try to steal from the right,
    /// but merge left and right if right is low too.
    unsafe fn handle_underflow_to_right(&mut self, underflowed_child_index: uint) {
        let right_len = self.edges[underflowed_child_index + 1].len();
        if right_len > min_load_from_capacity(self.capacity()) {
            self.steal_to_right(underflowed_child_index);
        } else {
            self.merge_children(underflowed_child_index);
        }
    }

    /// Steal! Stealing is roughly analagous to a binary tree rotation.
    /// In this case, we're "rotating" right.
    unsafe fn steal_to_left(&mut self, underflowed_child_index: uint) {
        // Take the biggest stuff off left
        let (mut key, mut val, edge) = {
            let left = self.edges.as_mut_slice().unsafe_mut_ref(underflowed_child_index - 1);
            match (left.keys.pop(), left.vals.pop(), left.edges.pop()) {
                (Some(k), Some(v), e) => (k, v, e),
                _ => unreachable!(),
            }
        };

        // Swap the parent's seperating key-value pair with left's
        mem::swap(self.keys.as_mut_slice().unsafe_mut_ref(underflowed_child_index - 1), &mut key);
        mem::swap(self.vals.as_mut_slice().unsafe_mut_ref(underflowed_child_index - 1), &mut val);

        // Put them at the start of right
        {
            let right = self.edges.as_mut_slice().unsafe_mut_ref(underflowed_child_index);
            right.keys.insert(0, key);
            right.vals.insert(0, val);
            match edge {
                None => {}
                Some(e) => right.edges.insert(0, e)
            }
        }
    }

    /// Steal! Stealing is roughly analagous to a binary tree rotation.
    /// In this case, we're "rotating" left.
    unsafe fn steal_to_right(&mut self, underflowed_child_index: uint) {
        // Take the smallest stuff off right
        let (mut key, mut val, edge) = {
            let right = self.edges.as_mut_slice().unsafe_mut_ref(underflowed_child_index + 1);
            match (right.keys.remove(0), right.vals.remove(0), right.edges.remove(0)) {
                (Some(k), Some(v), e) => (k, v, e),
                _ => unreachable!(),
            }
        };

        // Swap the parent's seperating key-value pair with right's
        mem::swap(self.keys.as_mut_slice().unsafe_mut_ref(underflowed_child_index), &mut key);
        mem::swap(self.vals.as_mut_slice().unsafe_mut_ref(underflowed_child_index), &mut val);

        // Put them at the end of left
        {
            let left = self.edges.as_mut_slice().unsafe_mut_ref(underflowed_child_index);
            left.keys.push(key);
            left.vals.push(val);
            match edge {
                None => {}
                Some(e) => left.edges.push(e)
            }
        }
    }

    /// Merge! Left and right will be smooshed into one node, along with the key-value
    /// pair that seperated them in their parent.
    unsafe fn merge_children(&mut self, left_index: uint) {
        // Permanently remove right's index, and the key-value pair that seperates
        // left and right
        let (key, val, right) = {
            match (self.keys.remove(left_index),
                self.vals.remove(left_index),
                self.edges.remove(left_index + 1)) {
                (Some(k), Some(v), Some(e)) => (k, v, e),
                _ => unreachable!(),
            }
        };

        // Give left right's stuff.
        let left = self.edges.as_mut_slice().unsafe_mut_ref(left_index);
        left.absorb(key, val, right);
    }

    /// Take all the values from right, seperated by the given key and value
    fn absorb(&mut self, key: K, val: V, right: Node<K, V>) {
        self.keys.push(key);
        self.vals.push(val);
        self.keys.extend(right.keys.move_iter());
        self.vals.extend(right.vals.move_iter());
        self.edges.extend(right.edges.move_iter());
    }
}

fn split<T>(left: &mut Vec<T>, amount: uint) -> Vec<T> {
    // Fixme(Gankro): gross gross gross gross
    let mut right = Vec::with_capacity(left.capacity());
    for _ in range(0, amount) {
        right.push(left.pop().unwrap());
    }
    right.reverse();
    right
}

fn capacity_from_b(b: uint) -> uint {
    2 * b - 1
}

fn min_load_from_capacity(cap: uint) -> uint {
    // B - 1
    (cap + 1) / 2 - 1
}

fn split_len_from_capacity(cap: uint) -> uint {
    // B - 1
    (cap + 1) / 2 - 1
}
