// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// This is all pretty trivial

use super::btreemap::BTreeMap;
use std::hash::{Writer, Hash};
use std::slice::Items;

/// A Set based on a B-Tree
pub struct BTreeSet<T>{
    map: BTreeMap<T, ()>,
}

impl<T: Ord> BTreeSet<T> {
    pub fn new() -> BTreeSet<T> {
        BTreeSet { map: BTreeMap::new() }
    }

    pub fn with_b(b: uint) -> BTreeSet<T> {
        BTreeSet { map: BTreeMap::with_b(b) }
    }

    /// stub
    pub fn iter<'a>(&'a self) -> Items<'a, T> {
        unreachable!(); //TODO
    }
}

impl<T> Collection for BTreeSet<T> {
    fn len(&self) -> uint {
        self.map.len()
    }
}

impl<T> Mutable for BTreeSet<T> {
    fn clear(&mut self) {
        self.map.clear()
    }
}

impl<T: Ord> Set<T> for BTreeSet<T> {
    fn contains(&self, value: &T) -> bool {
        self.map.find(value).is_some()
    }

    fn is_disjoint(&self, other: &BTreeSet<T>) -> bool {
        unreachable!() //TODO
    }

    fn is_subset(&self, other: &BTreeSet<T>) -> bool {
        // Stolen from TreeMap
        let mut x = self.iter();
        let mut y = other.iter();
        let mut a = x.next();
        let mut b = y.next();
        while a.is_some() {
            if b.is_none() {
                return false;
            }

            let a1 = a.unwrap();
            let b1 = b.unwrap();

            match b1.cmp(a1) {
                Less => (),
                Greater => return false,
                Equal => a = x.next(),
            }

            b = y.next();
        }
        true
    }
}

impl<T: Ord> MutableSet<T> for BTreeSet<T>{
    fn insert(&mut self, value: T) -> bool {
        self.map.insert(value, ())
    }

    fn remove(&mut self, value: &T) -> bool {
        self.map.remove(value)
    }
}

impl<T: Ord> FromIterator<T> for BTreeSet<T> {
    fn from_iter<Iter: Iterator<T>>(iter: Iter) -> BTreeSet<T> {
        let mut set = BTreeSet::new();
        set.extend(iter);
        set
    }
}

impl<T: Ord> Extendable<T> for BTreeSet<T> {
    #[inline]
    fn extend<Iter: Iterator<T>>(&mut self, mut iter: Iter) {
        for elem in iter {
            self.insert(elem);
        }
    }
}

impl<S: Writer, T: Ord + Hash<S>> Hash<S> for BTreeSet<T> {
    fn hash(&self, state: &mut S) {
        for elt in self.iter() {
            elt.hash(state);
        }
    }
}