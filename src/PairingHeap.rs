use coltests::priorityqueue::PriorityQueue;
use std::default::Default;

struct Node<T> {
	value: T,
	children: Vec<Node<T>>
}

impl<T> Node<T> {
	fn new(value:T) -> Node<T> {
		Node{value: value, children: Vec::new() }
	}

	fn add_child(&mut self, node: Node<T>) {
		self.children.push(node);
	}
}

/// A stupid implementation of a pairing heap, not useful
pub struct PairingHeap<T> {
	root: Option<Node<T>>,
	length: uint,
}

impl<T: Ord> PairingHeap<T> {
	pub fn new() -> PairingHeap<T> {
		PairingHeap { root: None, length: 0 }
	}
}

impl<T: Ord> PriorityQueue<T> for PairingHeap<T> {
	fn peek <'a> (&'a self) -> Option<&'a T> {
		self.root.as_ref().map(|node| &node.value)
	}

	fn pop(&mut self) -> Option<T> {
		match self.root.take() {
			None => None,
			Some(mut root) => {
				self.length -= 1;
				self.root = merge_all_children(&mut root);
				Some(root.value)
			}
		}
	}

	fn push(&mut self, value: T) {
		self.length += 1;
		let newNode = Node::new(value);
		self.root = match self.root.take() {
			None => Some(newNode),
			Some(root) => Some(merge(root, newNode)),
		};
	}
}

impl<T> Collection for PairingHeap<T> {
	fn len(&self) -> uint {
		self.length
	}
}

impl<T: Ord> Default for PairingHeap<T> {
	fn default() -> PairingHeap<T> {
		PairingHeap::new()
	}
}

impl<T: Ord> Extendable<T> for PairingHeap<T> {
    fn extend <I: Iterator<T>> (&mut self, mut iter: I) {
        for value in iter {
            self.push(value)
        }
    }
}

impl<T: Ord> FromIterator<T> for PairingHeap<T> {
    fn from_iter <I: Iterator<T>> (iter: I) -> PairingHeap<T> {
        let mut heap = PairingHeap::new();
        heap.extend(iter);
        heap
    }
}

impl<T> Mutable for PairingHeap<T> {
	fn clear (&mut self) {
		self.root = None;
		self.length = 0;
	}
}

fn merge<T: Ord> (mut tree1: Node<T>, mut tree2: Node<T>) -> Node<T> {
	if tree1.value < tree2.value {
		tree1.add_child(tree2);
		tree1
	} else {
		tree2.add_child(tree1);
		tree2
	}
}

fn merge_all_children<T: Ord> (node: &mut Node<T>) -> Option<Node<T>>{
	while node.children.len() > 1 {
		let mut iter = range(0, node.children.len()).rev();
		iter.next(); // go back one
		loop {
			match iter.next() {
				None => break,
				Some(i) => {
					let len =  node.children.len();
					node.children.as_mut_slice().swap(i, len - 1);
					node.children.as_mut_slice().swap(i + 1, len - 2);
					let a = node.children.pop().unwrap();
					let b = node.children.pop().unwrap();
					node.children.push(merge(a, b));

					if iter.next().is_none() { break; }
				}
			}
		}
	}
	node.children.pop()
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