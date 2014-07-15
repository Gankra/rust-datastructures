enum NextRef<K,V>{
	Parent(*mut Node<K,V>),
	RightSibling(Box<Node<K,V>>),
}

struct Node<K,V> {
	key: K,
	value: V,
	leftChild: Option<Box<Node<K,V>>>,
	next: Option<NextRef<K,V>>,
}

impl <K,V> Node<K,V> {
	fn new(key: K, value: V) -> Node<K,V> {
		Node{ key: key, value: value, leftChild:None, next:None }
	}

	fn add_child(&mut self, mut node: Box<Node<K,V>>) {
		node.next = match self.leftChild.take() {
			None => Some(Parent(self as *mut _)),
			Some(child) => Some(RightSibling(child)),
		};
		self.leftChild = Some(node);
	}

	fn take_child(&mut self) -> Option<Box<Node<K,V>>>{
		let mut result = self.leftChild.take();
		match &mut result{
			&None => {},
			&Some(ref mut child) => match child.next.take() {
				None => unreachable!(), //only a root can have no next
				Some(Parent(_)) => {}, //takes already None'd out everything
				Some(RightSibling(sibling)) => self.leftChild = Some(sibling),
			}
		}
		result
	}

	fn take_sibling(&mut self) -> Option<Box<Node<K,V>>> {
		match self.next.take() {
			None => None,
			Some(Parent(_)) => None,
			Some(RightSibling(mut sibling)) => {
				self.next = sibling.next.take();
				Some(sibling)
			}
		}
	}
}

struct PairingHeap <K,V> {
	root: Option<Box<Node<K,V>>>,
	length: uint,
}

impl <K:Ord, V> PairingHeap<K,V> {
	fn new() -> PairingHeap<K,V> {
		PairingHeap { root: None, length: 0 }
	}

	fn peek <'a> (&'a self) -> Option<&'a V> {
		self.root.as_ref().map(|node| &node.value)
	}

	fn pop(&mut self) -> Option<V> {
		match self.root.take() {
			None => None,
			Some(mut root) => {
				self.root = merge_all_children(&mut *root);
				Some(root.value)
			}
		}
	}

	fn insert(&mut self, key: K, value: V) {
		let newNode = box Node::new(key, value);
		self.root = match self.root.take() {
			None => Some(newNode),
			Some(root) => Some(merge(root, newNode)),
		}
	}

	fn decrease_key(){

	}
}

fn merge <K:Ord, V> (mut tree1: Box<Node<K,V>>, mut tree2: Box<Node<K,V>>) -> Box<Node<K,V>> {
	if tree1.key < tree2.key {
		tree1.add_child(tree2);
		tree1
	} else {
		tree2.add_child(tree1);
		tree2
	}
}

fn merge_all_children <K:Ord, V> (node: &mut Node<K,V>) -> Option<Box<Node<K,V>>>{
	None //TODO
}

fn main(){}