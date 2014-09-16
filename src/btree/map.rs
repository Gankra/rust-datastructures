// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// This implementation is largely based on the high-level description and analysis of B-Trees
// found in *Open Data Structures* (ODS). Although our implementation does not use any of
// the source found in ODS, if one wishes to review the high-level design of this structure, it
// can be freely downloaded at http://opendatastructures.org/. Its contents are as of this
// writing (August 2014) freely licensed under the following Creative Commons Attribution
// License: [CC BY 2.5 CA](http://creativecommons.org/licenses/by/2.5/ca/).

use super::node::*;
use std::hash::{Writer, Hash};
use std::default::Default;
use std::collections::{Deque, RingBuf};
use std::iter;
use std::fmt;
use std::fmt::Show;
use std::ptr;

/// Represents a search path for mutating
type SearchStack<K, V> = Vec<(*mut Node<K, V>, uint)>;

/// A B-Tree
pub struct BTreeMap<K, V> {
    root: Node<K, V>,
    length: uint,
    depth: uint,
    b: uint,
}

pub struct Entries<'a, K, V> {
    lca: Traversal<'a, K, V>,
    left: RingBuf<Traversal<'a, K, V>>,
    right: RingBuf<Traversal<'a, K, V>>,
    size: uint,
}

pub struct MutEntries<'a, K, V> {
    lca: MutTraversal<'a, K, V>,
    left: RingBuf<MutTraversal<'a, K, V>>,
    right: RingBuf<MutTraversal<'a, K, V>>,
    size: uint,
}

pub struct MoveEntries<K, V> {
    lca: MoveTraversal<K, V>,
    left: RingBuf<MoveTraversal<K, V>>,
    right: RingBuf<MoveTraversal<K, V>>,
    size: uint,
}


impl<K, V> BTreeMap<K, V> {
    /// Make a new empty BTreeMap with a reasonable choice for B
    pub fn new() -> BTreeMap<K, V> {
        //FIXME(Gankro): Tune this as a function of size_of<K/V>?
        BTreeMap::with_b(6)
    }

    /// Make a new empty BTreeMap with the given B
    pub fn with_b(b: uint) -> BTreeMap<K, V> {
        BTreeMap {
            length: 0,
            depth: 1,
            root: Node::make_leaf_root(b),
            b: b,
        }
    }

    /// Get an iterator over the entries of the map
    fn iter<'a>(&'a self) -> Entries<'a, K, V> {
        let depth = self.depth;
        let len = self.len();
        let root = &self.root;
        let is_leaf = root.is_leaf();
        let mut lca = root.iter();

        if is_leaf {
            // root is a leaf, iterator is just the root's iterator
            Entries {
                lca: lca,
                left: RingBuf::new(),
                right: RingBuf::new(),
                size: len,
            }
        } else {
            // root is internal, search for the min and max elements in the tree
            let mut left = RingBuf::with_capacity(depth);
            let mut right = RingBuf::with_capacity(depth);
            match (lca.next(), lca.next_back()) {
                (Some(Edge(mut left_child)), Some(Edge(mut right_child))) => {
                    // Walk to the smallest element in the tree
                    loop {
                        let is_leaf = left_child.is_leaf();
                        let mut iter = left_child.iter();
                        if is_leaf {
                            left.push(iter);
                            break;
                        } else {
                            left_child = match iter.next() {
                                Some(Edge(next)) => next,
                                _ => unreachable!(),
                            };
                            left.push(iter);
                        }
                    }
                    // Walk to the largest element in the tree
                    loop {
                        let is_leaf = right_child.is_leaf();
                        let mut iter = right_child.iter();
                        if is_leaf {
                            right.push(iter);
                            break;
                        } else {
                            right_child = match iter.next_back() {
                                Some(Edge(next)) => next,
                                _ => unreachable!(),
                            };
                            right.push(iter);
                        }
                    }
                    Entries {
                        lca: lca,
                        left: left,
                        right: right,
                        size: len,
                    }
                }
                _ => unreachable!()
            }
        }
    }

    /// Get a mutable iterator over the entries of the map
    pub fn mut_iter<'a>(&'a mut self) -> MutEntries<'a, K, V> {
        let depth = self.depth;
        let len = self.len();
        let root = &mut self.root;
        let is_leaf = root.is_leaf();
        let mut lca = root.mut_iter();

        if is_leaf {
            // root is a leaf, iterator is just the root's iterator
            MutEntries {
                lca: lca,
                left: RingBuf::new(),
                right: RingBuf::new(),
                size: len,
            }
        } else {
            // root is internal, search for the min and max elements in the tree
            let mut left = RingBuf::with_capacity(depth);
            let mut right = RingBuf::with_capacity(depth);
            match (lca.next(), lca.next_back()) {
                (Some(Edge(mut temp_left_child)), Some(Edge(mut temp_right_child))) => {
                    // Walk to the smallest element in the tree
                    loop {
                        let left_child = temp_left_child;
                        let is_leaf = left_child.is_leaf();
                        let mut iter = left_child.mut_iter();
                        if is_leaf {
                            left.push(iter);
                            break;
                        } else {
                            temp_left_child = match iter.next() {
                                Some(Edge(next)) => next,
                                _ => unreachable!(),
                            };
                            left.push(iter);
                        }
                    }
                    // Walk to the largest element in the tree
                    loop {
                        let right_child = temp_right_child;
                        let is_leaf = right_child.is_leaf();
                        let mut iter = right_child.mut_iter();
                        if is_leaf {
                            right.push(iter);
                            break;
                        } else {
                            temp_right_child = match iter.next_back() {
                                Some(Edge(next)) => next,
                                _ => unreachable!(),
                            };
                            right.push(iter);
                        }
                    }
                    MutEntries {
                        lca: lca,
                        left: left,
                        right: right,
                        size: len,
                    }
                }
                _ => unreachable!()
            }
        }
    }

    /// Get an iterator for moving the entries out of the map
    pub fn move_iter<'a>(self) -> MoveEntries<K, V> {
        let depth = self.depth;
        let len = self.len();
        let root = self.root;
        let is_leaf = root.is_leaf();
        let mut lca = root.move_iter();

        if is_leaf {
            // root is a leaf, iterator is just the root's iterator
            MoveEntries {
                lca: lca,
                left: RingBuf::new(),
                right: RingBuf::new(),
                size: len,
            }
        } else {
            // root is internal, search for the min and max elements in the tree
            let mut left = RingBuf::with_capacity(depth);
            let mut right = RingBuf::with_capacity(depth);
            match (lca.next(), lca.next_back()) {
                (Some(Edge(mut left_child)), Some(Edge(mut right_child))) => {
                    // Walk to the smallest element in the tree
                    loop {
                        let is_leaf = left_child.is_leaf();
                        let mut iter = left_child.move_iter();
                        if is_leaf {
                            left.push(iter);
                            break;
                        } else {
                            left_child = match iter.next() {
                                Some(Edge(next)) => next,
                                _ => unreachable!(),
                            };
                            left.push(iter);
                        }
                    }
                    // Walk to the largest element in the tree
                    loop {
                        let is_leaf = right_child.is_leaf();
                        let mut iter = right_child.move_iter();
                        if is_leaf {
                            right.push(iter);
                            break;
                        } else {
                            right_child = match iter.next_back() {
                                Some(Edge(next)) => next,
                                _ => unreachable!(),
                            };
                            right.push(iter);
                        }
                    }
                    MoveEntries {
                        lca: lca,
                        left: left,
                        right: right,
                        size: len,
                    }
                }
                _ => unreachable!()
            }
        }
    }

    /// Get an iterator over the keys of the map
    pub fn keys<'a>(&'a self) -> iter::Map<'a, (&'a K, &'a V), &'a K, Entries<'a, K, V>> {
        self.iter().map(|(k, _)| k)
    }

    /// Get an iterator over the values of the map
    pub fn values<'a>(&'a self) -> iter::Map<'a, (&'a K, &'a V), &'a V, Entries<'a, K, V>> {
        self.iter().map(|(_, v)| v)
    }
}

impl<K: Ord, V> Map<K, V> for BTreeMap<K, V> {
    // Searching in a B-Tree is pretty straightforward.
    //
    // Start at the root. Try to find the key in the current node. If we find it, return it.
    // If it's not in there, follow the edge *before* the smallest key larger than
    // the search key. If no such key exists (they're *all* smaller), then just take the last
    // edge in the node. If we're in a leaf and we don't find our key, then it's not
    // in the tree.
    fn find(&self, key: &K) -> Option<&V> {
        let mut cur_node = &self.root;
        loop {
            match cur_node.search(key) {
                Found(i) => return cur_node.val(i),
                GoDown(i) => match cur_node.edge(i) {
                    None => return None,
                    Some(next_node) => {
                        cur_node = next_node;
                        continue;
                    }
                }
            }
        }
    }
}

impl<K: Ord, V> MutableMap<K, V> for BTreeMap<K, V> {
    // See `find` for implementation notes, this is basically a copy-paste with mut's added
    fn find_mut(&mut self, key: &K) -> Option<&mut V> {
        // temp_node is a Borrowck hack for having a mutable value outlive a loop iteration
        let mut temp_node = &mut self.root;
        loop {
            let cur_node = temp_node;
            match cur_node.search(key) {
                Found(i) => return cur_node.val_mut(i),
                GoDown(i) => match cur_node.edge_mut(i) {
                    None => return None,
                    Some(next_node) => {
                        temp_node = next_node;
                        continue;
                    }
                }
            }
        }
    }

    // Insertion in a B-Tree is a bit complicated.
    //
    // First we do the same kind of search described in
    // `find`. But we need to maintain a stack of all the nodes/edges in our search path.
    // If we find a match for the key we're trying to insert, just swap the.vals and return the
    // old ones. However, when we bottom out in a leaf, we attempt to insert our key-value pair
    // at the same location we would want to follow another edge.
    //
    // If the node has room, then this is done in the obvious way by shifting elements. However,
    // if the node itself is full, we split node into two, and give its median
    // key-value pair to its parent to insert the new node with. Of course, the parent may also be
    // full, and insertion can propogate until we reach the root. If we reach the root, and
    // it is *also* full, then we split the root and place the two nodes under a newly made root.
    //
    // Note that we subtly deviate from Open Data Structures in our implementation of split.
    // ODS describes inserting into the node *regardless* of its capacity, and then
    // splitting *afterwards* if it happens to be overfull. However, this is inefficient.
    // Instead, we split beforehand, and then insert the key-value pair into the appropriate
    // result node. This has two consequences:
    //
    // 1) While ODS produces a left node of size B-1, and a right node of size B,
    // we may potentially reverse this. However, this shouldn't effect the analysis.
    //
    // 2) While ODS may potentially return the pair we *just* inserted after
    // the split, we will never do this. Again, this shouldn't effect the analysis.

    fn swap(&mut self, mut key: K, mut value: V) -> Option<V> {
        // visit_stack is a stack of rawptrs to nodes paired with indices, respectively
        // representing the nodes and edges of our search path. We have to store rawptrs
        // because as far as Rust is concerned, we can mutate aliased data with such a
        // stack. It is of course correct, but what it doesn't know is that we will only
        // be popping and using these ptrs one at a time in `insert_stack`. The alternative
        // to doing this is to take the Node boxes from their parents. This actually makes
        // borrowck *really* happy and everything is pretty smooth. However, this creates
        // *tons* of pointless writes, and requires us to always walk all the way back to
        // the root after an insertion, even if we only needed to change a leaf. Therefore,
        // we accept this potential unsafety and complexity in the name of performance.
        let mut visit_stack = Vec::with_capacity(self.depth);

        {
            // We need this temp_node for borrowck wrangling
            let mut temp_node = &mut self.root;

            loop {
                let cur_node = temp_node;
                let cur_node_ptr = cur_node as *mut _;

                // See `find` for a description of this search
                match cur_node.search(&key) {
                    Found(i) => unsafe {
                        // Perfect match, swap the contents and return the old ones
                        cur_node.unsafe_swap(i, &mut key, &mut value);
                        return Some(value);
                    },
                    GoDown(i) => {
                        visit_stack.push((cur_node_ptr, i));
                        match cur_node.edge_mut(i) {
                            None => {
                                // We've found where to insert this key/value pair
                                break;
                            }
                            Some(next_node) => {
                                // We've found the subtree to insert this key/value pair in
                                temp_node = next_node;
                                continue;
                            }
                        }
                    }
                }
            }
        }

        // If we get here then we need to insert a new element
        self.insert_stack(visit_stack, key, value);
        None
    }

    // Deletion is the most complicated operation for a B-Tree.
    //
    // First we do the same kind of search described in
    // `find`. But we need to maintain a stack of all the nodes/edges in our search path.
    // If we don't find the key, then we just return `None` and do nothing. If we do find the
    // key, we perform two operations: remove the item, and then possibly handle underflow.
    //
    // # removing the item
    //      If the node is a leaf, we just remove the item, and shift
    //      any items after it back to fill the hole.
    //
    //      If the node is an internal node, we *swap* the item with the smallest item in
    //      in its right subtree (which must reside in a leaf), and then revert to the leaf
    //      case
    //
    // # handling underflow
    //      After removing an item, there may be too few items in the node. We want nodes
    //      to be mostly full for efficiency, although we make an exception for the root, which
    //      may have as few as one item. If this is the case, we may first try to steal
    //      an item from our left or right neighbour.
    //
    //      To steal from the left (right) neighbour,
    //      we take the largest (smallest) item and child from it. We then swap the taken item
    //      with the item in their mutual parent that seperates them, and then insert the
    //      parent's item and the taken child into the first (last) index of the underflowed node.
    //
    //      However, stealing has the possibility of underflowing our neighbour. If this is the
    //      case, we instead *merge* with our neighbour. This of course reduces the number of
    //      children in the parent. Therefore, we also steal the item that seperates the now
    //      merged nodes, and insert it into the merged node.
    //
    //      Merging may cause the parent to underflow. If this is the case, then we must repeat
    //      the underflow handling process on the parent. If merging merges the last two children
    //      of the root, then we replace the root with the merged node.

    fn pop(&mut self, key: &K) -> Option<V> {
        // See `pop` for a discussion of these gross variables
        let mut visit_stack = Vec::with_capacity(self.depth);

        {
            let mut temp_node = &mut self.root;

            loop {
                let cur_node = temp_node;
                let cur_node_ptr = cur_node as *mut _;

                // See `find` for a description of this search
                match cur_node.search(key) {
                    Found(i) => {
                        // Perfect match. Terminate the stack here, and move to the
                        // next phase (remove_stack).
                        visit_stack.push((cur_node_ptr, i));

                        if !cur_node.is_leaf() {
                            // We found the key in an internal node, but that's annoying,
                            // so let's swap it with a leaf key and pretend we *did* find
                            // it in a leaf. Note that after calling this, the tree is in
                            // an inconsistent state, but will be consistent after we
                            // remove the swapped value in `remove_stack`
                            leafify_stack(&mut visit_stack);
                        }
                        break;
                    },
                    GoDown(i) => match cur_node.edge_mut(i) {
                        None => return None, // We're at a leaf; the key isn't in this tree
                        Some(next_node) => {
                            // We've found the subtree the key must be in
                            visit_stack.push((cur_node_ptr, i));
                            temp_node = next_node;
                            continue;
                        }
                    }
                }
            }
        }

        // If we get here then we found the key, let's remove it
        Some(self.remove_stack(visit_stack))
    }
}

impl<K: Ord, V> BTreeMap<K, V> {
    /// insert the key and value into the top element in the stack, and if that node has to split
    /// recursively insert the split contents into the stack until splits stop. Then replace the
    /// stack back into the tree.
    ///
    /// Assumes that the stack represents a search path from the root to a leaf, and that the
    /// search path is non-empty
    fn insert_stack(&mut self, mut stack: SearchStack<K, V>, key: K, val: V) {
        self.length += 1;

        // Insert the key and value into the leaf at the top of the stack
        let (node, index) = stack.pop().unwrap();
        let mut insertion = unsafe {
            (*node).insert_as_leaf(index, key, val)
        };

        loop {
            match insertion {
                Fit => {
                    // The last insertion went off without a hitch, no splits! We can stop
                    // inserting now.
                    return;
                }
                Split(key, val, right) => match stack.pop() {
                    // The last insertion triggered a split, so get the next element on the
                    // stack to revursively insert the split node into.
                    None => {
                        // The stack was empty; we've split the root, and need to make a new one.
                        unsafe {
                            // The unsafe optionless option dance
                            let root_ptr = &mut self.root as *mut _;
                            let root = ptr::read(root_ptr as *const _);
                            ptr::write(root_ptr,
                                Node::make_internal_root(self.b, key, val, root, right));
                        }
                        self.depth += 1;
                        return;
                    }
                    Some((node, index)) => {
                        // The stack wasn't empty, do the insertion and recurse
                        unsafe {
                            insertion = (*node).insert_as_internal(index, key, val, right);
                        }
                        continue;
                    }
                }
            }
        }
    }

    /// Remove the key and value in the top element of the stack, then handle underflows.
    /// Assumes the stack represents a search path from the root to a leaf.
    fn remove_stack(&mut self, mut stack: SearchStack<K, V>) -> V {
        self.length -= 1;

        // Remove the key-value pair from the leaf, check if the node is underfull, and then
        // promptly forget the leaf and ptr to avoid ownership issues
        let (value, mut underflow) = unsafe {
            let (node_ptr, index) = stack.pop().unwrap();
            let node = &mut *node_ptr;
            let (_key, value) = node.remove_as_leaf(index);
            let underflow = node.is_underfull();
            (value, underflow)
        };

        loop {
            match stack.pop() {
                None => {
                    // We've reached the root, so no matter what, we're done. We manually access
                    // the root via the tree itself to avoid creating any dangling pointers.
                    if self.root.len() == 0 && !self.root.is_leaf() {
                        // We've emptied out the root, so make its only child the new root.
                        // If the root is a leaf, this will set the root to `None`
                        self.depth -= 1;
                        self.root = self.root.pop_edge().unwrap();
                    }
                    return value;
                }
                Some((parent_ptr, index)) => {
                    if underflow {
                        // Underflow! Handle it!
                        unsafe {
                            let parent = &mut *parent_ptr;
                            parent.handle_underflow(index);
                            underflow = parent.is_underfull();
                        }
                    } else {
                        // All done!
                        return value;
                    }
                }
            }
        }
    }
}

impl<K, V> Collection for BTreeMap<K, V>{
    fn len(&self) -> uint {
        self.length
    }
}

impl<K, V> Mutable for BTreeMap<K, V> {
    fn clear(&mut self) {
        // Note that this will trigger a lot of recursive destructors, but BTreeMaps can't get
        // very deep, so we won't worry about it for now.
        self.root = Node::make_leaf_root(self.b);
        self.length = 0;
        self.depth = 1;
    }
}

impl<K: Ord, V> FromIterator<(K, V)> for BTreeMap<K, V> {
    fn from_iter<T: Iterator<(K, V)>>(iter: T) -> BTreeMap<K, V> {
        let mut map = BTreeMap::new();
        map.extend(iter);
        map
    }
}

impl<K: Ord, V> Extendable<(K, V)> for BTreeMap<K, V> {
    #[inline]
    fn extend<T: Iterator<(K, V)>>(&mut self, mut iter: T) {
        for (k, v) in iter {
            self.insert(k, v);
        }
    }
}

impl<S: Writer, K: Ord + Hash<S>, V: Hash<S>> Hash<S> for BTreeMap<K, V> {
    fn hash(&self, state: &mut S) {
        for elt in self.iter() {
            elt.hash(state);
        }
    }
}

impl<K: Ord, V> Default for BTreeMap<K, V> {
    fn default() -> BTreeMap<K, V> {
        BTreeMap::new()
    }
}

impl<K: PartialEq + Ord, V: PartialEq> PartialEq for BTreeMap<K, V> {
    fn eq(&self, other: &BTreeMap<K, V>) -> bool {
        self.len() == other.len() &&
            self.iter().zip(other.iter()).all(|(a, b)| a == b)
    }
}

impl<K: Eq + Ord, V: Eq> Eq for BTreeMap<K, V> {}

impl<K: Ord, V: PartialOrd> PartialOrd for BTreeMap<K, V> {
    #[inline]
    fn partial_cmp(&self, other: &BTreeMap<K, V>) -> Option<Ordering> {
        iter::order::partial_cmp(self.iter(), other.iter())
    }
}

impl<K: Ord, V: Ord> Ord for BTreeMap<K, V> {
    #[inline]
    fn cmp(&self, other: &BTreeMap<K, V>) -> Ordering {
        iter::order::cmp(self.iter(), other.iter())
    }
}

impl<K: Ord + Show, V: Show> Show for BTreeMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{{"));

        for (i, (k, v)) in self.iter().enumerate() {
            if i != 0 { try!(write!(f, ", ")); }
            try!(write!(f, "{}: {}", *k, *v));
        }

        write!(f, "}}")
    }
}

impl<K: Clone, V: Clone> Clone for BTreeMap<K, V> {
    fn clone(&self) -> BTreeMap<K, V> {
        BTreeMap {
            length: self.length,
            depth: self.depth,
            root: self.root.clone(),
            b: self.b,
        }
    }
}

impl<K: Ord, V> Index<K, V> for BTreeMap<K, V> {
    fn index(&self, key: &K) -> &V {
        self.find(key).expect("no entry found for key")
    }
}

/// Subroutine for removal. Takes a search stack for a key that terminates at an
/// internal node, and mutates the tree and search stack to make it a search
/// stack for that key that terminates at a leaf. This leaves the tree in an inconsistent
/// state that must be repaired by the caller by removing the key in question.
fn leafify_stack<K, V>(stack: &mut SearchStack<K, V>) {
    let (node_ptr, index) = stack.pop().unwrap();
    unsafe {
        // First, get ptrs to the found key-value pair
        let node = &mut *node_ptr;
        let (key_ptr, val_ptr) = {
            (node.unsafe_key_mut(index) as *mut _,
             node.unsafe_val_mut(index) as *mut _)
        };

        // Go into the right subtree of the found key to find its successor
        stack.push((node_ptr, index + 1));
        let mut temp_node = node.unsafe_edge_mut(index + 1);

        loop {
            // Walk into the smallest subtree of this node
            let node = temp_node;
            let node_ptr = node as *mut _;
            stack.push((node_ptr, 0));
            if node.is_leaf() {
                // This node is a leaf, do the swap and return
                node.unsafe_swap(0, &mut *key_ptr, &mut *val_ptr);
                break;
            } else {
                // This node is internal, go deeper
                temp_node = node.unsafe_edge_mut(0);
            }
        }
    }
}

enum StackOp<T> {
    Push(T),
    Pop,
}

impl<'a, K, V> Iterator<(&'a K, &'a V)> for Entries<'a, K, V> {
    // This function is pretty long, but only because there's a lot of cases to consider.
    // Our iterator represents two search paths, left and right, to the smallest and largest
    // elements we have yet to yield. lca represents the least common ancestor of these two paths,
    // above-which we should never walk.
    //
    // Note that the design of these iterators permits an *arbitrary* initial pair of min and max,
    // making these arbitrary sub-range iterators. However the logic to construct these paths
    // effeciently is fairly involved, so this is TODO. The sub-range iterators also wouldn't be
    // able to accurately predict size, so the iterators would have to be newtyped to not
    // implement ExactSize.
    fn next(&mut self) -> Option<(&'a K, &'a V)> {
        loop {
            // We want the smallest element, so try to get the top of the left stack
            let op = match self.left.back_mut() {
                // The left stack is empty, so try to get the next element of the two paths
                // LCAs (the left search path is currently a subpath of the right one)
                None => match self.lca.next() {
                    // The lca has been exhausted, walk further down the right path
                    None => match self.right.pop_front() {
                        // The right path is exhausted, so we're done
                        None => return None,
                        // The right path had something, make that the new LCA
                        // and restart the whole process
                        Some(right) => {
                            self.lca = right;
                            continue;
                        }
                    },
                    // The lca yielded an edge, make that the new head of the left path
                    Some(Edge(next)) => {
                        Push(next.iter())
                    },
                    // The lca yielded an entry, so yield that
                    Some(Elem(k, v)) => {
                        self.size -= 1;
                        return Some((k, v))
                    }
                },
                // The left stack wasn't empty, so continue along the node in its head
                Some(iter) => match iter.next() {
                    // The head of the left path is empty, so Pop it off and restart the process
                    None => {
                        Pop
                    },
                    // The head of the left path yielded an edge, so make that the new head
                    // of the left path
                    Some(Edge(next)) => {
                        Push(next.iter())
                    },
                    // The head of the left path yielded entry, so yield that
                    Some(Elem(k, v)) => {
                        self.size -= 1;
                        return Some((k, v))
                    }
                }
            };

            // Handle any operation on the left stack as necessary
            match op {
                Push(item) => {
                    self.left.push(item);
                },
                Pop => {
                    self.left.pop();
                }
            }
        }
    }

    fn size_hint(&self) -> (uint, Option<uint>) {
        (self.size, Some(self.size))
    }
}

impl<'a, K, V> DoubleEndedIterator<(&'a K, &'a V)> for Entries<'a, K, V> {
    // See Entry's impl for details, next_back is totally symmetric to next
    fn next_back(&mut self) -> Option<(&'a K, &'a V)> {
        loop {
            let op = match self.right.back_mut() {
                None => match self.lca.next_back() {
                    None => match self.left.pop_front() {
                        None => return None,
                        Some(left) => {
                            self.lca = left;
                            continue;
                        }
                    },
                    Some(Edge(next)) => {
                        Push(next.iter())
                    },
                    Some(Elem(k, v)) => {
                        self.size -= 1;
                        return Some((k, v))
                    }
                },
                Some(iter) => match iter.next_back() {
                    None => {
                        Pop
                    },
                    Some(Edge(next)) => {
                        Push(next.iter())
                    },
                    Some(Elem(k, v)) => {
                        self.size -= 1;
                        return Some((k, v))
                    }
                }
            };

            match op {
                Push(item) => {
                    self.right.push(item);
                },
                Pop => {
                    self.right.pop();
                }
            }
        }
    }
}

impl<'a, K, V> ExactSize<(&'a K, &'a V)> for Entries<'a, K, V> {}

impl<'a, K, V> Iterator<(&'a K, &'a mut V)> for MutEntries<'a, K, V> {
    // See Entry's impl for details
    fn next(&mut self) -> Option<(&'a K, &'a mut V)> {
        loop {
            let op = match self.left.back_mut() {
                None => match self.lca.next() {
                    None => match self.right.pop_front() {
                        None => return None,
                        Some(right) => {
                            self.lca = right;
                            continue;
                        }
                    },
                    Some(Edge(next)) => {
                        Push(next.mut_iter())
                    },
                    Some(Elem(k, v)) => {
                        self.size -= 1;
                        return Some((k, v))
                    }
                },
                Some(iter) => match iter.next() {
                    None => {
                        Pop
                    },
                    Some(Edge(next)) => {
                        Push(next.mut_iter())
                    },
                    Some(Elem(k, v)) => {
                        self.size -= 1;
                        return Some((k, v))
                    }
                }
            };

            match op {
                Push(item) => {
                    self.left.push(item);
                },
                Pop => {
                    self.left.pop();
                }
            }
        }
    }

    fn size_hint(&self) -> (uint, Option<uint>) {
        (self.size, Some(self.size))
    }
}

impl<'a, K, V> DoubleEndedIterator<(&'a K, &'a mut V)> for MutEntries<'a, K, V> {
    // See Entry's impl for details
    fn next_back(&mut self) -> Option<(&'a K, &'a mut V)> {
        loop {
            let op = match self.right.back_mut() {
                None => match self.lca.next_back() {
                    None => match self.left.pop_front() {
                        None => return None,
                        Some(left) => {
                            self.lca = left;
                            continue;
                        }
                    },
                    Some(Edge(next)) => {
                        Push(next.mut_iter())
                    },
                    Some(Elem(k, v)) => {
                        self.size -= 1;
                        return Some((k, v))
                    }
                },
                Some(iter) => match iter.next_back() {
                    None => {
                        Pop
                    },
                    Some(Edge(next)) => {
                        Push(next.mut_iter())
                    },
                    Some(Elem(k, v)) => {
                        self.size -= 1;
                        return Some((k, v))
                    }
                }
            };

            match op {
                Push(item) => {
                    self.right.push(item);
                },
                Pop => {
                    self.right.pop();
                }
            }
        }
    }
}

impl<'a, K, V> ExactSize<(&'a K, &'a mut V)> for MutEntries<'a, K, V> {}

impl<K, V> Iterator<(K, V)> for MoveEntries<K, V> {
    // See Entry's impl for details
    fn next(&mut self) -> Option<(K, V)> {
        loop {
            let op = match self.left.back_mut() {
                None => match self.lca.next() {
                    None => match self.right.pop_front() {
                        None => return None,
                        Some(right) => {
                            self.lca = right;
                            continue;
                        }
                    },
                    Some(Edge(next)) => {
                        Push(next.move_iter())
                    },
                    Some(Elem(k, v)) => {
                        self.size -= 1;
                        return Some((k, v))
                    }
                },
                Some(iter) => match iter.next() {
                    None => {
                        Pop
                    },
                    Some(Edge(next)) => {
                        Push(next.move_iter())
                    },
                    Some(Elem(k, v)) => {
                        self.size -= 1;
                        return Some((k, v))
                    }
                }
            };

            match op {
                Push(item) => {
                    self.left.push(item);
                },
                Pop => {
                    self.left.pop();
                }
            }
        }
    }

    fn size_hint(&self) -> (uint, Option<uint>) {
        (self.size, Some(self.size))
    }
}

impl<K, V> DoubleEndedIterator<(K, V)> for MoveEntries<K, V> {
    // See Entry's impl for details
    fn next_back(&mut self) -> Option<(K, V)> {
        loop {
            let op = match self.right.back_mut() {
                None => match self.lca.next_back() {
                    None => match self.left.pop_front() {
                        None => return None,
                        Some(left) => {
                            self.lca = left;
                            continue;
                        }
                    },
                    Some(Edge(next)) => {
                        Push(next.move_iter())
                    },
                    Some(Elem(k, v)) => {
                        self.size -= 1;
                        return Some((k, v))
                    }
                },
                Some(iter) => match iter.next_back() {
                    None => {
                        Pop
                    },
                    Some(Edge(next)) => {
                        Push(next.move_iter())
                    },
                    Some(Elem(k, v)) => {
                        self.size -= 1;
                        return Some((k, v))
                    }
                }
            };

            match op {
                Push(item) => {
                    self.right.push(item);
                },
                Pop => {
                    self.right.pop();
                }
            }
        }
    }
}

impl<K, V> ExactSize<(K, V)> for MoveEntries<K, V> {}

#[cfg(test)]
mod test {
    use super::BTreeMap;

    #[test]
    fn test_basic() {
        let mut map = BTreeMap::new();
        let size = 10000u;
        assert_eq!(map.len(), 0);

        for i in range(0, size) {
            assert_eq!(map.swap(i, 10*i), None);
            assert_eq!(map.len(), i + 1);
        }

        for i in range(0, size) {
            assert_eq!(map.find(&i).unwrap(), &(i*10));
        }

        for i in range(size, size*2) {
            assert_eq!(map.find(&i), None);
        }

        for i in range(0, size) {
            assert_eq!(map.swap(i, 100*i), Some(10*i));
            assert_eq!(map.len(), size);
        }

        for i in range(0, size) {
            assert_eq!(map.find(&i).unwrap(), &(i*100));
        }

        for i in range(0, size/2) {
            assert_eq!(map.pop(&(i*2)), Some(i*200));
            assert_eq!(map.len(), size - i - 1);
        }

        for i in range(0, size/2) {
            assert_eq!(map.find(&(2*i)), None);
            assert_eq!(map.find(&(2*i+1)).unwrap(), &(i*200 + 100));
        }

        for i in range(0, size/2) {
            assert_eq!(map.pop(&(2*i)), None);
            assert_eq!(map.pop(&(2*i+1)), Some(i*200 + 100));
            assert_eq!(map.len(), size/2 - i - 1);
        }
    }

    #[test]
    fn test_iter() {
        let size = 10000u;

        let mut map: BTreeMap<uint, uint> = Vec::from_fn(size, |i| (i, i)).move_iter().collect();

        for (i, x) in map.iter().enumerate() {
            assert_eq!(x, (&i, &i));
        }

        for (i, x) in map.iter().rev().enumerate() {
            assert_eq!(x, (&(size - 1 - i), &(size - i - 1)));
        }

        for (i, x) in map.mut_iter().enumerate() {
            assert_eq!(x, (&i, &mut (i + 0)));
        }

        for (i, x) in map.mut_iter().rev().enumerate() {
            assert_eq!(x, (&(size - 1 - i), &mut (size - i - 1)));
        }

        // TODO: move iter
    }
}


#[cfg(test)]
mod bench {
    use test::Bencher;
    use super::BTreeMap;

    use std::rand;
    use std::rand::Rng;

    pub fn insert_rand_n<M: MutableMap<uint, uint>>(n: uint,
                                                    map: &mut M,
                                                    b: &mut Bencher) {
        // setup
        let mut rng = rand::weak_rng();

        map.clear();
        for _ in range(0, n) {
            map.insert(rng.gen::<uint>() % n, 1);
        }

        // measure
        b.iter(|| {
            let k = rng.gen::<uint>() % n;
            map.insert(k, 1);
            map.remove(&k);
        })
    }

    pub fn insert_seq_n<M: MutableMap<uint, uint>>(n: uint,
                                                   map: &mut M,
                                                   b: &mut Bencher) {
        // setup
        map.clear();
        for i in range(0u, n) {
            map.insert(i*2, 1);
        }

        // measure
        let mut i = 1;
        b.iter(|| {
            map.insert(i, 1);
            map.remove(&i);
            i = (i + 2) % n;
        })
    }

    pub fn find_rand_n<M:MutableMap<uint,uint>>(n: uint,
                                                map: &mut M,
                                                b: &mut Bencher) {
        // setup
        let mut rng = rand::weak_rng();
        let mut keys = Vec::from_fn(n, |_| rng.gen::<uint>() % n);

        for k in keys.iter() {
            map.insert(*k, 1);
        }

        rng.shuffle(keys.as_mut_slice());

        // measure
        let mut i = 0;
        b.iter(|| {
            map.find(&keys[i]);
            i = (i + 1) % n;
        })
    }

    pub fn find_seq_n<M:MutableMap<uint,uint>>(n: uint,
                                               map: &mut M,
                                               b: &mut Bencher) {
        // setup
        for i in range(0u, n) {
            map.insert(i, 1);
        }

        // measure
        let mut i = 0;
        b.iter(|| {
            let x = map.find(&i);
            i = (i + 1) % n;
            x
        })
    }

    // Find seq
    #[bench]
    pub fn insert_rand_100(b: &mut Bencher) {
        let mut m : BTreeMap<uint,uint> = BTreeMap::new();
        insert_rand_n(100, &mut m, b);
    }

    #[bench]
    pub fn insert_rand_10_000(b: &mut Bencher) {
        let mut m : BTreeMap<uint,uint> = BTreeMap::new();
        insert_rand_n(10_000, &mut m, b);
    }

    // Insert seq
    #[bench]
    pub fn insert_seq_100(b: &mut Bencher) {
        let mut m : BTreeMap<uint,uint> = BTreeMap::new();
        insert_seq_n(100, &mut m, b);
    }

    #[bench]
    pub fn insert_seq_10_000(b: &mut Bencher) {
        let mut m : BTreeMap<uint,uint> = BTreeMap::new();
        insert_seq_n(10_000, &mut m, b);
    }

    // Find rand
    #[bench]
    pub fn find_rand_100(b: &mut Bencher) {
        let mut m : BTreeMap<uint,uint> = BTreeMap::new();
        find_rand_n(100, &mut m, b);
    }

    #[bench]
    pub fn find_rand_10_000(b: &mut Bencher) {
        let mut m : BTreeMap<uint,uint> = BTreeMap::new();
        find_rand_n(10_000, &mut m, b);
    }

    // Find seq
    #[bench]
    pub fn find_seq_100(b: &mut Bencher) {
        let mut m : BTreeMap<uint,uint> = BTreeMap::new();
        find_seq_n(100, &mut m, b);
    }

    #[bench]
    pub fn find_seq_10_000(b: &mut Bencher) {
        let mut m : BTreeMap<uint,uint> = BTreeMap::new();
        find_seq_n(10_000, &mut m, b);
    }
}