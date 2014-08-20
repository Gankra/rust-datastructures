#![feature(phase)]
#![feature(unsafe_destructor)]
#![feature(default_type_params)]

//! This is a test

#[phase(plugin, link)]
extern crate coltests;
extern crate test;



// Ephemeral

// Maps
pub mod bst;
pub mod splaytree;

// Heaps
pub mod binaryheap;
pub mod binomialheap;
pub mod pairingheap;




// Persistent

// Lists
pub mod immutslist;