// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.



/// "Order" of the B-tree, from which all other properties are derived. In experiments with
/// different values of B on a BTree<uint, uint> on 64-bit linux, `B = 5` struck the best
/// balance between search and mutation time. Lowering B improves mutation time (less array
/// shifting), and raising B improves search time (less depth and pointer following).
/// However, increasing B higher than 5 had marginal search gains compared to mutation costs.
/// This value should be re-evaluated whenever the tree is significantly refactored.
static B: uint = 5;
/// Maximum number of elements in a node
static CAPACITY: uint = 2 * B - 1;
/// Minimum number of elements in a node
static MIN_LOAD: uint = B - 1;
/// Maximum number of children in a node
static EDGE_CAPACITY: uint = CAPACITY + 1;
/// Amount to take off the tail of a node being split
static SPLIT_LEN: uint = B - 1;

pub mod btree;
pub mod node;