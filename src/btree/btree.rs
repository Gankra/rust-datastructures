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

use std::mem;
use super::node::*;
use super::MIN_LOAD;

/// A B-Tree of Order 6
pub struct BTree<K,V>{
    root: Option<Node<K,V>>,
    length: uint,
    depth: uint,
}

impl<K,V> BTree<K,V> {
    /// Make a new empty BTree
    pub fn new() -> BTree<K,V> {
        BTree {
            length: 0,
            depth: 0,
            root: None,
        }
    }
}

impl<K: Ord, V> Map<K,V> for BTree<K,V> {
    // Searching in a B-Tree is pretty straightforward.
    //
    // Start at the root. Try to find the key in the current node. If we find it, return it.
    // If it's not in there, follow the edge *before* the smallest key larger than
    // the search key. If no such key exists (they're *all* smaller), then just take the last
    // edge in the node. If we're in a leaf and we don't find our key, then it's not
    // in the tree.
    fn find(&self, key: &K) -> Option<&V> {
        match self.root.as_ref() {
            None => None,
            Some(root) => {
                let mut cur_node = root;
                loop {
                    match cur_node.search(key) {
                        Found(i) => return cur_node.vals.as_slice().get(i),
                        GoDown(i) => match cur_node.edges.as_slice().get(i) {
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
    }
}

impl<K: Ord, V> MutableMap<K,V> for BTree<K,V> {
    // See `find` for implementation notes, this is basically a copy-paste with mut's added
    fn find_mut(&mut self, key: &K) -> Option<&mut V> {
        match self.root.as_mut() {
            None => None,
            Some(root) => {
                // temp_node is a Borrowck hack for having a mutable value outlive a loop iteration
                let mut temp_node = root;
                loop {
                    let cur_node = temp_node;
                    match cur_node.search(key) {
                        Found(i) => return cur_node.vals.as_mut_slice().get_mut(i),
                        GoDown(i) => match cur_node.edges.as_mut_slice().get_mut(i) {
                            None => return None,
                            Some(next_node) => {
                                temp_node = next_node;
                                continue;
                            }
                        }
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
        // FIXME(Gankro): this is gross because of lexical borrows.
        // If pcwalton's work pans out, this can be made much better!
        // See `find` for a more idealized structure
        if self.root.is_none() {
            self.root = Some(Node::make_leaf_root(key, value));
            self.length += 1;
            self.depth += 1;
            None
        } else {
            let visit_stack = {
                // We need this temp_node for borrowck wrangling
                let mut temp_node = self.root.as_mut().unwrap();
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

                loop {
                    let cur_node = temp_node;
                    let cur_node_ptr = cur_node as *mut _;

                    // See `find` for a description of this search
                    match cur_node.search(&key) {
                        Found(i) => unsafe {
                            // Perfect match, swap the contents and return the old ones
                            mem::swap(cur_node.vals.as_mut_slice().unsafe_mut_ref(i), &mut value);
                            mem::swap(cur_node.keys.as_mut_slice().unsafe_mut_ref(i), &mut key);
                            return Some(value);
                        },
                        GoDown(i) => {
                            visit_stack.push((cur_node_ptr, i));
                            match cur_node.edges.as_mut_slice().get_mut(i) {
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
                visit_stack
            };

            // If we get here then we need to insert a new element
            self.insert_stack(visit_stack, key, value);
            None
        }
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
        // See `pop` for a discussion of why this is gross
        if self.root.is_none() {
            // We're empty, get lost!
            None
        } else {
            let visit_stack = {
                // We need this temp_node for borrowck wrangling
                let mut temp_node = self.root.as_mut().unwrap();
                // See `pop` for a description of this variable
                let mut visit_stack = Vec::with_capacity(self.depth);

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
                        GoDown(i) => match cur_node.edges.as_mut_slice().get_mut(i) {
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
                visit_stack
            };

            // If we get here then we found the key, let's remove it
            Some(self.remove_stack(visit_stack))
        }
    }
}

impl<K: Ord, V> BTree<K,V> {
    /// insert the key and value into the top element in the stack, and if that node has to split
    /// recursively insert the split contents into the stack until splits stop. Then replace the
    /// stack back into the tree.
    ///
    /// Assumes that the stack represents a search path from the root to a leaf, and that the
    /// search path is non-empty
    fn insert_stack(&mut self, mut stack: SearchStack<K,V>, key: K, value: V) {
        self.length += 1;

        // Insert the key and value into the leaf at the top of the stack
        let (node, index) = stack.pop().unwrap();
        let mut insertion = unsafe {
            (*node).insert_as_leaf(index, key, value)
        };

        loop {
            match insertion {
                Fit => {
                    // The last insertion went off without a hitch, no splits! We can stop
                    // inserting now.
                    return;
                }
                Split(key, value, right) => match stack.pop() {
                    // The last insertion triggered a split, so get the next element on the
                    // stack to revursively insert the split node into.
                    None => {
                        // The stack was empty; we've split the root, and need to make a new one.
                        let left = self.root.take().unwrap();
                        self.root = Some(Node::make_internal_root(key, value, left, right));
                        self.depth += 1;
                        return;
                    }
                    Some((node, index)) => {
                        // The stack wasn't empty, do the insertion and recurse
                        unsafe {
                            insertion = (*node).insert_as_internal(index, key, value, right);
                        }
                        continue;
                    }
                }
            }
        }
    }

    /// Remove the key and value in the top element of the stack, then handle underflows.
    /// Assumes the stack represents a search path from the root to a leaf.
    fn remove_stack(&mut self, mut stack: SearchStack<K,V>) -> V {
        self.length -= 1;

        // Remove the key-value pair from the leaf, check if the node is underfull, and then
        // promptly forget the leaf and ptr to avoid ownership issues
        let (value, mut underflow) = unsafe {
            let (node_ptr, index) = stack.pop().unwrap();
            let node = &mut *node_ptr;
            let (_key, value) = node.remove_as_leaf(index);
            let underflow = node.len() < MIN_LOAD;
            (value, underflow)
        };

        loop {
            match stack.pop() {
                None => {
                    // We've reached the root, so no matter what, we're done. We manually access
                    // the root via the tree itself to avoid creating any dangling pointers.
                    if self.root.as_ref().unwrap().len() == 0 {
                        // We've emptied out the root, so make its only child the new root.
                        // If the root is a leaf, this will set the root to `None`
                        self.depth -= 1;
                        self.root = self.root.take().unwrap().edges.pop();
                    }
                    return value;
                }
                Some((parent_ptr, index)) => {
                    if underflow {
                        // Underflow! Handle it!
                        unsafe {
                            let parent = &mut *parent_ptr;
                            parent.handle_underflow(index);
                            underflow = parent.len() < MIN_LOAD;
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

impl<K,V> Collection for BTree<K,V>{
    fn len(&self) -> uint {
        self.length
    }
}

impl<K,V> Mutable for BTree<K,V> {
    fn clear(&mut self) {
        // Note that this will trigger a lot of recursive destructors, but BTrees can't get
        // very deep, so we won't worry about it for now.
        self.root = None;
        self.length = 0;
        self.depth = 0;
    }
}





#[cfg(test)]
mod test {
    use super::BTree;

    #[test]
    fn test_basic() {
        let mut map = BTree::new();
        let size = 10000u;
        assert_eq!(map.len(), 0);

        for i in range(0, size) {
            assert_eq!(map.swap(i, 10*i), None);
            assert_eq!(map.len(), i + 1);
        }

        for i in range(0, size) {
            assert_eq!(map.find(&i).expect(format!("f {}", i).as_slice()), &(i*10));
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
}




#[cfg(test)]
mod bench {
    use test::Bencher;
    use super::BTree;

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
        let mut m : BTree<uint,uint> = BTree::new();
        insert_rand_n(100, &mut m, b);
    }

    #[bench]
    pub fn insert_rand_10_000(b: &mut Bencher) {
        let mut m : BTree<uint,uint> = BTree::new();
        insert_rand_n(10_000, &mut m, b);
    }

    // Insert seq
    #[bench]
    pub fn insert_seq_100(b: &mut Bencher) {
        let mut m : BTree<uint,uint> = BTree::new();
        insert_seq_n(100, &mut m, b);
    }

    #[bench]
    pub fn insert_seq_10_000(b: &mut Bencher) {
        let mut m : BTree<uint,uint> = BTree::new();
        insert_seq_n(10_000, &mut m, b);
    }

    // Find rand
    #[bench]
    pub fn find_rand_100(b: &mut Bencher) {
        let mut m : BTree<uint,uint> = BTree::new();
        find_rand_n(100, &mut m, b);
    }

    #[bench]
    pub fn find_rand_10_000(b: &mut Bencher) {
        let mut m : BTree<uint,uint> = BTree::new();
        find_rand_n(10_000, &mut m, b);
    }

    // Find seq
    #[bench]
    pub fn find_seq_100(b: &mut Bencher) {
        let mut m : BTree<uint,uint> = BTree::new();
        find_seq_n(100, &mut m, b);
    }

    #[bench]
    pub fn find_seq_10_000(b: &mut Bencher) {
        let mut m : BTree<uint,uint> = BTree::new();
        find_seq_n(10_000, &mut m, b);
    }
}