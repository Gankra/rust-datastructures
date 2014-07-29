#![feature(phase)]

#[phase(plugin, link)]
extern crate coltests;
extern crate test;

//lists
pub mod singlylinkedlist;

//maps
pub mod bst;
pub mod splaytree;

//heaps
pub mod binaryheap;
pub mod binomialheap;
pub mod pairingheap;
