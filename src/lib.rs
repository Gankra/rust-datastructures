#![feature(phase)]
#![feature(unsafe_destructor)]
#![feature(default_type_params)]

//! A test bed for experimenting with data structures in Rust.
//!
//! Publicly exported structures are usable, although perhaps
//! impractical. However if they do not have tests, they are probably
//! unfinished, and almost certainly incorrect.

#[phase(plugin, link)]
extern crate coltests;
extern crate test;



// Ephemeral

// Maps
pub mod bst;
pub mod splaytree;
pub mod btree;

// Heaps
pub mod binaryheap;
pub mod binomialheap;
pub mod pairingheap;




// Persistent

// Lists
pub mod immutslist;