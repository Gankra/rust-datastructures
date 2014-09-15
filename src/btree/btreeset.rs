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

use super::btreemap::*;
use std::hash::{Writer, Hash};
use std::default::Default;
use std::iter;
use std::iter::Peekable;
use std::fmt;
use std::fmt::Show;

/// A Set based on a B-Tree
pub struct BTreeSet<T>{
    map: BTreeMap<T, ()>,
}

pub type Items<'a, T> = iter::Map<'a, (&'a T, &'a ()), &'a T, Entries<'a, T, ()>>;
pub type MoveItems<T> = iter::Map<'static, (T, ()), T, MoveEntries<T, ()>>;

impl<T: Ord> BTreeSet<T> {
    pub fn new() -> BTreeSet<T> {
        BTreeSet { map: BTreeMap::new() }
    }

    pub fn with_b(b: uint) -> BTreeSet<T> {
        BTreeSet { map: BTreeMap::with_b(b) }
    }

    pub fn iter<'a>(&'a self) -> Items<'a, T> {
        self.map.keys()
    }

    pub fn move_iter(self) -> MoveItems<T> {
        self.map.move_iter().map(|(k, _)| k)
    }

    /// Visits the values representing the difference, in ascending order.
    pub fn difference<'a>(&'a self, other: &'a BTreeSet<T>) -> DifferenceItems<'a, T> {
        DifferenceItems{a: self.iter().peekable(), b: other.iter().peekable()}
    }

    /// Visits the values representing the symmetric difference, in ascending order.
    pub fn symmetric_difference<'a>(&'a self, other: &'a BTreeSet<T>)
        -> SymDifferenceItems<'a, T> {
        SymDifferenceItems{a: self.iter().peekable(), b: other.iter().peekable()}
    }

    /// Visits the values representing the intersection, in ascending order.
    pub fn intersection<'a>(&'a self, other: &'a BTreeSet<T>)
        -> IntersectionItems<'a, T> {
        IntersectionItems{a: self.iter().peekable(), b: other.iter().peekable()}
    }

    /// Visits the values representing the union, in ascending order.
    pub fn union<'a>(&'a self, other: &'a BTreeSet<T>) -> UnionItems<'a, T> {
        UnionItems{a: self.iter().peekable(), b: other.iter().peekable()}
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
        self.intersection(other).next().is_none()
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

impl<T: Ord> Default for BTreeSet<T> {
    fn default() -> BTreeSet<T> {
        BTreeSet::new()
    }
}

impl<T: PartialEq + Ord> PartialEq for BTreeSet<T> {
    #[inline]
    fn eq(&self, other: &BTreeSet<T>) -> bool { self.map == other.map }
}

impl<T: Eq + Ord> Eq for BTreeSet<T> {}

impl<T: Ord> PartialOrd for BTreeSet<T> {
    #[inline]
    fn partial_cmp(&self, other: &BTreeSet<T>) -> Option<Ordering> {
        self.map.partial_cmp(&other.map)
    }
}

impl<T: Ord> Ord for BTreeSet<T> {
    #[inline]
    fn cmp(&self, other: &BTreeSet<T>) -> Ordering {
        iter::order::cmp(self.iter(), other.iter())
    }
}

impl<T: Ord + Show> Show for BTreeSet<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{{"));

        for (i, x) in self.iter().enumerate() {
            if i != 0 { try!(write!(f, ", ")); }
            try!(write!(f, "{}", *x));
        }

        write!(f, "}}")
    }
}

/// A lazy iterator producing elements in the set difference (in-order).
pub struct DifferenceItems<'a, T> {
    a: Peekable<&'a T, Items<'a, T>>,
    b: Peekable<&'a T, Items<'a, T>>,
}

/// A lazy iterator producing elements in the set symmetric difference (in-order).
pub struct SymDifferenceItems<'a, T> {
    a: Peekable<&'a T, Items<'a, T>>,
    b: Peekable<&'a T, Items<'a, T>>,
}

/// A lazy iterator producing elements in the set intersection (in-order).
pub struct IntersectionItems<'a, T> {
    a: Peekable<&'a T, Items<'a, T>>,
    b: Peekable<&'a T, Items<'a, T>>,
}

/// A lazy iterator producing elements in the set union (in-order).
pub struct UnionItems<'a, T> {
    a: Peekable<&'a T, Items<'a, T>>,
    b: Peekable<&'a T, Items<'a, T>>,
}

/// Compare `x` and `y`, but return `short` if x is None and `long` if y is None
fn cmp_opt<T: Ord>(x: Option<&T>, y: Option<&T>,
                        short: Ordering, long: Ordering) -> Ordering {
    match (x, y) {
        (None    , _       ) => short,
        (_       , None    ) => long,
        (Some(x1), Some(y1)) => x1.cmp(y1),
    }
}

impl<'a, T: Ord> Iterator<&'a T> for DifferenceItems<'a, T> {
    fn next(&mut self) -> Option<&'a T> {
        loop {
            match cmp_opt(self.a.peek(), self.b.peek(), Less, Less) {
                Less    => return self.a.next(),
                Equal   => { self.a.next(); self.b.next(); }
                Greater => { self.b.next(); }
            }
        }
    }
}

impl<'a, T: Ord> Iterator<&'a T> for SymDifferenceItems<'a, T> {
    fn next(&mut self) -> Option<&'a T> {
        loop {
            match cmp_opt(self.a.peek(), self.b.peek(), Greater, Less) {
                Less    => return self.a.next(),
                Equal   => { self.a.next(); self.b.next(); }
                Greater => return self.b.next(),
            }
        }
    }
}

impl<'a, T: Ord> Iterator<&'a T> for IntersectionItems<'a, T> {
    fn next(&mut self) -> Option<&'a T> {
        loop {
            let o_cmp = match (self.a.peek(), self.b.peek()) {
                (None    , _       ) => None,
                (_       , None    ) => None,
                (Some(a1), Some(b1)) => Some(a1.cmp(b1)),
            };
            match o_cmp {
                None          => return None,
                Some(Less)    => { self.a.next(); }
                Some(Equal)   => { self.b.next(); return self.a.next() }
                Some(Greater) => { self.b.next(); }
            }
        }
    }
}

impl<'a, T: Ord> Iterator<&'a T> for UnionItems<'a, T> {
    fn next(&mut self) -> Option<&'a T> {
        loop {
            match cmp_opt(self.a.peek(), self.b.peek(), Greater, Less) {
                Less    => return self.a.next(),
                Equal   => { self.b.next(); return self.a.next() }
                Greater => return self.b.next(),
            }
        }
    }
}