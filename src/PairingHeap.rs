use coltests::priorityqueue::PriorityQueue;
use std::default::Default;

enum NextRef<T>{
	Parent(*mut Node<T>),
	RightSibling(Box<Node<T>>),
}

struct Node<T> {
	value: T,
	leftChild: Option<Box<Node<T>>>,
	next: Option<NextRef<T>>,
}

impl <T> Node<T> {
	fn new(value:T) -> Node<T> {
		Node{value: value, leftChild:None, next:None }
	}

	fn add_child(&mut self, mut node: Box<Node<T>>) {
		node.next = match self.leftChild.take() {
			None => Some(Parent(self as *mut _)),
			Some(child) => Some(RightSibling(child)),
		};
		self.leftChild = Some(node);
	}

	fn take_child(&mut self) -> Option<Box<Node<T>>>{
		self.leftChild.take()
	}

	fn take_sibling(&mut self) -> Option<Box<Node<T>>> {
		match self.next.take() {
			None => None,
			parent@Some(Parent(_)) => {
				self.next = parent;
				None
			},
			Some(RightSibling(mut sibling)) => {
				self.next = sibling.next.take();
				Some(sibling)
			}
		}
	}
}

pub struct PairingHeap <T> {
	root: Option<Box<Node<T>>>,
	length: uint,
}

impl <T: Ord> PairingHeap<T> {
	pub fn new() -> PairingHeap<T> {
		PairingHeap { root: None, length: 0 }
	}
}

impl <T: Ord> PriorityQueue <T> for PairingHeap <T> {
	fn peek <'a> (&'a self) -> Option<&'a T> {
		self.root.as_ref().map(|node| &node.value)
	}

	fn pop(&mut self) -> Option<T> {
		match self.root.take() {
			None => None,
			Some(mut root) => {
				self.length -= 1;
				self.root = merge_all_children(&mut *root);
				Some(root.value)
			}
		}
	}

	fn push(&mut self, value: T) {
		self.length += 1;
		let newNode = box Node::new(value);
		self.root = match self.root.take() {
			None => Some(newNode),
			Some(root) => Some(merge(root, newNode)),
		};
	}
}

impl <T> Collection for PairingHeap <T> {
	fn len(&self) -> uint {
		self.length
	}
}

impl <T: Ord> Default for PairingHeap <T> {
	fn default() -> PairingHeap <T> {
		PairingHeap::new()
	}
}

impl <T: Ord> Extendable<T> for PairingHeap<T> {
    fn extend <I: Iterator<T>> (&mut self, mut iter: I) {
        for value in iter {
            self.push(value)
        }
    }
}

impl <T: Ord> FromIterator<T> for PairingHeap<T> {
    fn from_iter <I: Iterator<T>> (iter: I) -> PairingHeap<T> {
        let mut heap = PairingHeap::new();
        heap.extend(iter);
        heap
    }
}

impl <T> Mutable for PairingHeap<T> {
	fn clear (&mut self) {
		self.root = None;
		self.length = 0;
	}
}

fn merge <T: Ord> (mut tree1: Box<Node<T>>, mut tree2: Box<Node<T>>) -> Box<Node<T>> {
	if tree1.value < tree2.value {
		tree1.add_child(tree2);
		tree1
	} else {
		tree2.add_child(tree1);
		tree2
	}
}

fn merge_all_children <T: Ord> (node: &mut Node<T>) -> Option<Box<Node<T>>>{
	match node.take_child() {
		None => None,
		Some(first_child) => Some(merge_all_siblings(first_child))
	}
}

fn merge_all_siblings <T: Ord> (mut first_child: Box<Node<T>>) -> Box<Node<T>> {
	match first_child.take_sibling() {
		None => first_child,
		Some(mut second_child) => match second_child.take_sibling() {
			None => merge(first_child, second_child),
			Some(third_child) => merge(merge(first_child, second_child), merge_all_siblings(third_child))
		}
	}
}

#[cfg(test)]
mod test{
    use super::PairingHeap;
    use coltests::collection;
    use coltests::priorityqueue;

    type ToTest = PairingHeap<uint>;
    
    use_test!(empty, collection::test_empty::<ToTest>())
    use_test!(clear, collection::test_clear::<ToTest, _>())
    use_test!(from_iter, collection::test_from_iter::<ToTest, _>())
    use_test!(extend, collection::test_extend::<ToTest, _>())
    use_test!(push, priorityqueue::test_push::<ToTest>())
    use_test!(pop, priorityqueue::test_pop::<ToTest>())
    use_test!(peek, priorityqueue::test_peek::<ToTest>())
}

#[cfg(test)]
mod bench {
    use super::PairingHeap;

    use coltests::collection;
    use coltests::priorityqueue;
    use coltests::utils;
    use test::Bencher;

    bench_priorityqueue!(PairingHeap<uint>)
}