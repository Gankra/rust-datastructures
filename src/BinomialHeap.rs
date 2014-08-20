use coltests::priorityqueue::PriorityQueue;
use std::default::Default;

struct Node <T> {
	elem: T,
	children: Vec<Node<T>>,
}

impl <T> Node <T> {
	fn new(elem: T) -> Node<T> {
		Node{ elem: elem, children: Vec::new() }
	}
}

struct BinomialHeap <T> {
	trees: Vec<Option<Node<T>>>,
	min_index: uint,
	length: uint,
}

impl <T:Ord> BinomialHeap <T> {
	pub fn new () -> BinomialHeap<T> {
		BinomialHeap{ trees: Vec::new(), min_index:0, length: 0}
	}

	fn insert_tree_at (&mut self, mut tree: Node<T>, mut index: uint, fix_indexes: bool) {
		let mut is_min = fix_indexes && self.is_min(&tree.elem);
		loop {
			if index >= self.trees.len() {
				self.trees.push(Some(tree));
				if is_min { self.min_index = index };
				return;
			} else {
				match self.trees.get_mut(index).take() {
					None => {
						self.trees.as_mut_slice()[index] = Some(tree);
						if is_min { self.min_index = index };
						return;
					}
					Some(otherTree) => {
						//we're eating min, so we are min
						if fix_indexes && self.min_index == index {
							is_min = true
						}
						tree = merge(tree, otherTree);
						index += 1;
					}
				}
			}
		}
	}

	fn insert_children (&mut self, children: Vec<Node<T>>) {
		let mut index = 0;
		for child in children.move_iter() {
			self.insert_tree_at(child, index, false);
			index += 1;
		}
		self.fix_min_index();
	}

	fn is_min (&self, elem: &T) -> bool {
		match self.peek() {
			None => true,
			Some(min) => *elem < *min
		}
	}

	fn fix_min_index(&mut self) {
		let mut min_index = -1;
		for index in range(0, self.trees.len()) {
			match self.trees[index] {
				None => {}
				Some(ref node) => {
					if min_index == -1 || node.elem < self.trees[min_index].as_ref().unwrap().elem {
						min_index = index;
					}
				}
			}
		}
		self.min_index = min_index;
	}
}

impl <T:Ord> PriorityQueue <T> for BinomialHeap <T> {
	fn push (&mut self, elem: T) {
		self.insert_tree_at(Node::new(elem), 0, true);
		self.length += 1;
	}

	fn pop (&mut self) -> Option<T> {
		if self.is_empty() {
			None
		} else {
			let min_tree = self.trees.get_mut(self.min_index).take_unwrap();
			let result = Some(min_tree.elem);
			self.insert_children(min_tree.children);
			self.length -= 1;
			result
		}

	}

	fn peek <'a> (&'a self) -> Option<&'a T> {
		if self.is_empty() {
			None
		} else {
			Some(&self.trees[self.min_index].as_ref().unwrap().elem)
		}
	}
}

impl <T> Collection for BinomialHeap <T> {
	fn len(&self) -> uint {
		self.length
	}
}

impl <T:Ord> Default for BinomialHeap <T> {
	fn default() -> BinomialHeap <T> {
		BinomialHeap::new()
	}
}

impl <T:Ord> Extendable<T> for BinomialHeap<T> {
    fn extend <I: Iterator<T>> (&mut self, mut iter: I) {
        for value in iter {
            self.push(value)
        }
    }
}

impl <T:Ord> FromIterator<T> for BinomialHeap<T> {
    fn from_iter <I: Iterator<T>> (iter: I) -> BinomialHeap<T> {
        let mut heap = BinomialHeap::new();
        heap.extend(iter);
        heap
    }
}

impl <T> Mutable for BinomialHeap<T> {
	fn clear (&mut self) {
		self.trees = Vec::new();
		self.min_index = 0;
		self.length = 0;
	}
}

fn merge <T: Ord> (mut tree1: Node<T>, mut tree2: Node<T>) -> Node<T> {
	if tree1.elem < tree2.elem {
		tree1.children.push(tree2);
		tree1
	} else {
		tree2.children.push(tree1);
		tree2
	}
}

#[cfg(test)]
mod test{
    use super::BinomialHeap;
    use coltests::collection;
    use coltests::priorityqueue;

    type ToTest = BinomialHeap<uint>;

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
    use super::BinomialHeap;

    use coltests::collection;
    use coltests::priorityqueue;
    use coltests::utils;
    use test::Bencher;

    bench_priorityqueue!(BinomialHeap<uint>)
}