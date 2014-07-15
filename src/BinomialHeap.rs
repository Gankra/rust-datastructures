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
	fn new () -> BinomialHeap<T> {
		BinomialHeap{ trees: Vec::new(), min_index:0, length: 0}
	}

	fn insert (&mut self, elem: T) {
		self.insert_tree_at(Node::new(elem), 0);
	}

	fn insert_tree_at (&mut self, mut tree: Node<T>, mut index: uint) {
		let is_min = self.is_min(&tree.elem);
		self.length += 1;
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
			self.insert_tree_at(child, index);
			index += 1;
		}
		self.fix_min_index();
	}

	fn pop (&mut self) -> Option<T> {
		if self.is_empty() {
			None
		} else {
			let mut min_tree = self.trees.get_mut(self.min_index).take_unwrap();
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
			Some(&self.trees.get(self.min_index).as_ref().unwrap().elem)
		}
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
			match *self.trees.get(index) {
				None => {}
				Some(ref node) => {
					if min_index == -1 || node.elem < self.trees.get(min_index).as_ref().unwrap().elem {
						min_index = index;
					}
				}
			}
		}
		self.min_index = min_index;
	}
}

impl <T> Collection for BinomialHeap <T> {
	fn len(&self) -> uint {
		self.length
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

fn main() {}