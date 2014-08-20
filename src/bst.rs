use std::ptr::RawPtr;
use std::default::Default;
use std::mem;

pub type NodeRef<K,V> = Option<Box<Node<K,V>>>;
pub type NodeBackRef<K,V> = *mut Node<K,V>;

/// A simple Node type with raw parent pointer
/// To preserve safety and sanity, only the methods
/// set_left, set_right should be used to set references
/// they ensure parent pointers are correctly fixed
/// similarly the root should only be set with set_root
pub struct Node<Key, Value> {
    pub key: Key,
    pub value: Value,
        parent: NodeBackRef<Key, Value>,
    pub left: NodeRef<Key, Value>,
    pub right: NodeRef<Key, Value>,
}

impl <K,V> Node<K,V> {
    pub fn new (key: K, value: V) -> Node<K, V> {
        Node {
            left: None,
            right: None,
            parent: RawPtr::null(),
            value: value,
            key: key,
        }
    }

    pub fn get_parent <'a> (&'a self) -> Option<&'a Node<K,V>> {
        unsafe{self.parent.to_option()}
    }

    pub fn get_parent_mut <'a> (&'a mut self) -> Option<&'a mut Node<K,V>> {
        if self.parent.is_null() {
            None
        } else {
            unsafe{Some(&mut*self.parent)}
        }
    }

    // Am I my parent's left child?
    pub fn is_left (&self) -> bool {
        match self.get_parent() {
            None => false,
            Some(ref parent) => match &parent.left {
                &None => false,
                &Some(ref leftChild) => (self as *const _) == (&**leftChild as *const _)
            }
        }
    }

    // Do I have any children?
    pub fn is_leaf (&self) -> bool {
        self.left.is_none() && self.right.is_none()
    }

    // Do I have a parent?
    pub fn is_root (&self) -> bool {
        self.get_parent().is_none()
    }

    pub fn set_left(&mut self, node:Option<Box<Node<K,V>>>) {
        self.left = node;
        match &mut self.left {
            &None => {}
            &Some(box ref mut node) => {node.parent = self as *mut _}
        };
    }

    pub fn set_right(&mut self, node:Option<Box<Node<K,V>>>) {
        self.right = node;
        match &mut self.right {
            &None => {}
            &Some(box ref mut node) => {node.parent = self as *mut _}
        };
    }
}

/// NodeRefToken is a mechanism to allow Tree
/// to internally pass references to its contents to itself
/// or to give access to anyone who would extend Tree.
///
/// If a method on Tree returns a NodeRefToken, it is asserting
/// that dereferencing the token is safe, and the reference will
/// be valid at least until the tree is mutated again. After any
/// other mutatation all bets are off, and tokens made before
/// should be discarded. Tokens should never be stored long-term.
///
/// Methods for unwraping NodeRefToken are on the Tree itself
/// to prevent tokens leaking references. The mutability of the
/// reference is tied to the mutability of the tree to prevent mutability
/// escalation. Tokens self destruct by nulling out their reference when used.
/// Any attempt to derefernce a token again will result in hard failure.

pub struct NodeRefToken<K,V> {
    node: *const Node<K,V>
}

impl <K,V> NodeRefToken<K,V> {
    pub fn new(node: &Node<K,V>) -> NodeRefToken<K,V> {
        NodeRefToken{node: node as *const _}
    }
}

/// A simple binary search tree, that does nothing to stay balanced.
/// This structure is not intended for direct use, but for extension by
/// other tree-based structures, such as a splay tree or treap. Consequently,
/// much of its internals are made public to allow easy reuse and composition
/// of simple operations. The BST provides sane defaults and utilities
/// for many operations that an extender should leverage when possible.
///
/// In contrast to trees found in the std lib, these trees use parent pointers.
/// This of course has a memory, time, and safety overhead. Regardless,
/// some motivation for this includes:
///
/// # Familiarity
/// Most tree-based structures and algorithms are designed with parent pointers
/// taken for granted. While many of them can be adapted to avoid parent pointers,
/// it can increase the complexity of the implementation, or force the usage
/// of a more *obscure* design.
///
/// # Composability
/// With parent pointers, one can easily perform a rotation or other similar
/// operation with only one reference into the tree. Otherwise, we made need
/// to track more values. The NodeRefToken system is built around this principle,
/// at the cost of safety.
///
/// # Amortization of memory costs
/// While the memory usage of parent pointers significantly increases the cost
/// of storing the tree, having them allows some algorithms, such as traversals,
/// to operate with significantly less working memory (O(1) vs O(n) in the case of
/// traverals). By incuring a small memory overhead per-node, we avoid large memory
/// overheads on operations. This makes memory usage more stable, and avoids
/// costly heap allocations (or explosive stacks) in performance-critical sections.
/// Nodes must be heap allocated regardless of their size, and allocating a slightly
/// largely node should generally be faster than allocating two smaller objects.

pub struct Tree<Key, Value> {
    pub root: NodeRef<Key, Value>,
    pub len: uint,
}

impl <K, V> Tree<K,V> {
    /// Get a reference tied to the lifetime of the tree, for exporting in e.g. get methods
    pub unsafe fn take_unwrap_token_exportable <'a> (&'a self, token: &mut NodeRefToken<K,V>) -> &'a Node<K,V> {
        if token.node.is_null() { fail!("cannot be dereffed twice!") }
        let result = &*token.node;
        token.node = RawPtr::null(); //null it out to prevent reuse
        result
    }

    /// Mutable version of take_unwrap_token_exportable
    pub unsafe fn take_unwrap_token_exportable_mut <'a> (&'a mut self, token: &mut NodeRefToken<K,V>) -> &'a mut Node<K,V> {
        if token.node.is_null() { fail!("cannot be dereffed twice!") }
        let result = &mut*(token.node as *mut _);
        token.node = RawPtr::null(); //null it out to prevent reuse
        result
    }

    /// Get a reference tied to the token's life to avoid locking the tree itself, and to prevent leaking the reference
    pub unsafe fn take_unwrap_token <'a> (&self, token: &'a mut NodeRefToken<K,V>) -> &'a Node<K,V> {
        if token.node.is_null() { fail!("cannot be dereffed twice!") }
        let result = &*token.node;
        token.node = RawPtr::null(); //null it out to prevent reuse
        result
    }

    /// Mutable version of take_unwrap_token
    pub unsafe fn take_unwrap_token_mut <'a> (&mut self, token: &'a mut NodeRefToken<K,V>) -> &'a mut Node<K,V> {
        if token.node.is_null() { fail!("cannot be dereffed twice!") }
        let result = &mut*(token.node as *mut _);
        token.node = RawPtr::null(); //null it out to prevent reuse
        result
    }
}

impl <K: Ord, V> Tree<K,V> {
    pub fn new () -> Tree<K,V> {
        Tree{root:None, len: 0}
    }

    pub fn contains_internal (&self, key: &K) -> Option<NodeRefToken<K,V>> {
        let (_, node) = self.find_internal(key);
        node
    }

    pub fn insert_internal (&mut self, key: K, value: V) -> (Option<V>, Option<NodeRefToken<K,V>>){
        let mut old = None;

        let inserted = if self.root.is_none() {
            self.set_root(Some(box Node::new(key, value)));
            Some(NodeRefToken::new(&mut **self.root.as_mut().unwrap()))
        }else{
            let (mut parentOpt, mut nodeOpt) = self.find_internal(&key);
            match &mut nodeOpt {
                &None => {
                    let newNode = box Node::new(key, value);
                    //this is safe as long as the tree is consistent, and we might as well fail otherwise
                    let parent = unsafe{self.take_unwrap_token_mut(parentOpt.as_mut().unwrap())};

                    if newNode.key < parent.key {
                        parent.set_left(Some(newNode));
                        Some(NodeRefToken::new(&mut **parent.left.as_mut().unwrap()))
                    } else {
                        parent.set_right(Some(newNode));
                        Some(NodeRefToken::new(&mut **parent.right.as_mut().unwrap()))
                    }
                }
                &Some(ref mut nodeRefToken) => {
                    let node = unsafe{self.take_unwrap_token_mut(nodeRefToken)};
                    let mut temp = value;
                    mem::swap(&mut temp, &mut node.value);
                    old = Some(temp);

                    Some(NodeRefToken::new(node))
                }
            }
        };

        if old.is_none() {
            self.len += 1;
        }

        (old, inserted)
    }

    /// Returns the parent and node of the element
    pub fn find_internal <'a> (&'a self, key: &K) -> (Option<NodeRefToken<K,V>>, Option<NodeRefToken<K,V>>) {
        match self.root {
            None => return (None, None),
            Some(box ref root) => {
                let mut parent = None;
                let mut curNode = root;
                loop {
                    let nextNodeOpt = match curNode.key.cmp(key) {
                        Equal   => return (parent, Some(NodeRefToken::new(curNode))),
                        Greater => &curNode.left,
                        Less    => &curNode.right,
                    };
                    match nextNodeOpt {
                        &None => return (Some(NodeRefToken::new(curNode)), None),
                        &Some(box ref nextNode) => {
                            parent = Some(NodeRefToken::new(curNode));
                            curNode = nextNode;
                        }
                    }
                }
            }
        }
    }

    pub fn set_root(&mut self, node:Option<Box<Node<K,V>>>){
        self.root = node;
        match &mut self.root {
            &None => {}
            &Some(box ref mut node) => node.parent = RawPtr::null()
        };
    }

    pub fn rotate_left(&mut self, node:&mut Node<K,V>) {
        let b_left = node.is_left();
        if node.right.is_some() {
            let mut childBox = node.right.take_unwrap();
            node.set_right(childBox.left.take());

            match node.get_parent_mut() {
                None => {
                    //we are the root
                    childBox.set_left(self.root.take());
                    self.set_root(Some(childBox));
                }
                Some(ref mut parent) => {
                    //we are not the root
                    if b_left {
                        //we were a left child
                        childBox.set_left(parent.left.take());
                        parent.set_left(Some(childBox));
                    } else {
                        //we were a right child
                        childBox.set_left(parent.right.take());
                        parent.set_right(Some(childBox));
                    }
                }
            }
        }
    }

    pub fn rotate_right(&mut self, node:&mut Node<K,V>) {
        let b_left = node.is_left();
        if node.left.is_some() {
            let mut childBox = node.left.take_unwrap();
            node.set_left(childBox.right.take());

            match node.get_parent_mut() {
                None => {
                    //we are the root
                    childBox.set_right(self.root.take());
                    self.set_root(Some(childBox));
                }
                Some(ref mut parent) => {
                    //we are not the root
                    if b_left {
                        //we were a left child
                        childBox.set_right(parent.left.take());
                        parent.set_left(Some(childBox));
                    } else {
                        //we were a right child
                        childBox.set_right(parent.right.take());
                        parent.set_right(Some(childBox));
                    }
                }
            }
        }
    }

    pub fn rotate_up(&mut self, node:&mut Node<K,V>) {
        let b_left = node.is_left();
        let parent = node.get_parent_mut();

        match parent {
            None => return,
            Some(parent) => {
                if b_left {
                    self.rotate_right(parent);
                } else {
                    self.rotate_left(parent);
                }
            }
        }
    }

    pub fn rotate_down(&mut self, node:&mut Node<K,V>) {
        if node.left.is_some() {
            self.rotate_right(node);
        }else if node.right.is_some() {
            self.rotate_left(node);
        }
    }

    pub fn traverse(&self, f:|&K, &V|, g:|&K, &V|, h:|&K, &V|) {
        enum MoveState {Down, UpFromLeft, UpFromRight};
        match self.root {
            None => return,
            Some(box ref node) => {
                let mut curNode = node;
                let mut curState = Down;
                loop {
                    match curState {
                        Down => {
                            f(&curNode.key, &curNode.value);
                            match curNode.left {
                                None => {
                                    curState = UpFromLeft;
                                }
                                Some(box ref nextNode) => {
                                    curNode = nextNode;
                                    curState = Down;
                                }
                            }
                        }
                        UpFromLeft => {
                            g(&curNode.key, &curNode.value);
                            match curNode.right {
                                None => {
                                    curState = UpFromRight;
                                }
                                Some(box ref nextNode) => {
                                    curNode = nextNode;
                                    curState = Down;
                                }
                            }
                        }
                        UpFromRight => {
                            h(&curNode.key, &curNode.value);
                            match curNode.get_parent() {
                                None => {
                                    return;
                                }
                                Some(nextNode) => {
                                    curState = if curNode.is_left() {
                                        UpFromLeft
                                    } else {
                                        UpFromRight
                                    };
                                    curNode = nextNode;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn preorder_traversal(&self, f:|&K, &V|){
        self.traverse(f, |_,_|(), |_,_|());
    }

    pub fn inorder_traversal(&self, f:|&K, &V|){
        self.traverse(|_,_|(), f, |_,_|());
    }

    pub fn postorder_traversal(&self, f:|&K, &V|){
        self.traverse(|_,_|(), |_,_|(), f);
    }

}

impl <K: Ord, V> Extendable<(K,V)> for Tree<K,V> {
    fn extend <I: Iterator<(K,V)>> (&mut self, mut iter: I) {
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}

impl <K: Ord, V> FromIterator<(K,V)> for Tree<K,V> {
    fn from_iter <I: Iterator<(K,V)>> (iter: I) -> Tree<K,V> {
        let mut tree = Tree::new();
        tree.extend(iter);
        tree
    }
}

impl <K,V> Collection for Tree<K,V> {
    fn len(&self) -> uint{
        self.len
    }
}

impl <K,V> Mutable for Tree<K,V> {
    fn clear(&mut self) {
        self.len = 0;
        self.root = None;
    }
}

impl <K:Ord,V> Default for Tree<K,V> {
    fn default() -> Tree<K,V> {Tree::new()}
}

impl <K:Ord,V> Map<K,V> for Tree<K,V> {
    fn find <'a> (&'a self, key: &K) -> Option<&'a V> {
        let (_, nodeToken) = self.find_internal(key);
        nodeToken.map(|mut token| unsafe{&self.take_unwrap_token_exportable(&mut token).value} )
    }
}

impl <K:Ord, V> MutableMap<K,V> for Tree<K,V> {
    fn find_mut <'a> (&'a mut self, key: &K) -> Option<&'a mut V> {
        let (_, mut nodeToken) = self.find_internal(key);
        match &mut nodeToken {
            &None => None,
            &Some(ref mut token) => Some(unsafe{&mut self.take_unwrap_token_exportable_mut(token).value})
        }
    }

    fn swap (&mut self, key: K, value: V) -> Option<V> {
        let (old, _) = self.insert_internal(key, value);
        old
    }

    fn pop (&mut self, key: &K) -> Option<V> {
        let (_,mut node) = self.find_internal(key);
        match &mut node {
            &None => None,
            &Some(ref mut token) => {
                self.len -= 1;
                if self.len() == 0 {
                    let root = self.root.take_unwrap();

                    Some(root.value)
                } else {
                    let node = unsafe{self.take_unwrap_token_mut(token)};

                    while !node.is_leaf() {
                        self.rotate_down(node);
                    }

                    let is_left = node.is_left();
                    let parent = node.get_parent_mut().unwrap();
                    let nodeBox =
                        if is_left {
                            parent.left.take_unwrap()
                        } else {
                            parent.right.take_unwrap()
                        };

                    Some(nodeBox.value)
                }
            }
        }
    }
}





#[cfg(test)]
mod test{
    use super::{Tree};
    use coltests::collection;
    use coltests::map;

    type ToTest = Tree<uint, uint>;

    use_test!(empty, collection::test_empty::<ToTest>())
    use_test!(clear, collection::test_clear::<ToTest, _>())
    use_test!(from_iter, collection::test_from_iter::<ToTest, _>())
    use_test!(extend, collection::test_extend::<ToTest, _>())
    use_test!(insert, map::test_insert::<ToTest>())
    use_test!(swap, map::test_swap::<ToTest>())
    use_test!(remove, map::test_remove::<ToTest>())
    use_test!(pop, map::test_pop::<ToTest>())
    use_test!(contains, map::test_contains::<ToTest>())
    use_test!(find, map::test_find::<ToTest>())
    use_test!(find_mut, map::test_find_mut::<ToTest>())
    use_test!(integration, map::test_integration::<ToTest>())

    #[test]
    fn test_inorder_traversal(){
        let xs =        [6i,5,7,2,4,10,3,-1,8];
        let expected =  [-1i,2,3,4,5,6,7,8,10];
        let mut index = 0;

        let map:Tree<int,()> = xs.iter().map(|&x| (x, ())).collect();
        map.inorder_traversal(|key,_| {
            assert_eq!(key, &expected[index]);
            index += 1;
        })
    }

    #[test]
    fn test_postorder_traversal(){
        let xs =        [6i,5,7,2,4,10,3,-1,8];
        let expected =  [-1i,3,4,2,5,8,10,7,6];
        let mut index = 0;

        let map:Tree<int,()> = xs.iter().map(|&x| (x, ())).collect();
        map.postorder_traversal(|key,_| {
            assert_eq!(key, &expected[index]);
            index += 1;
        })
    }

    #[test]
    #[should_fail]
    fn test_token_deref_fail(){
        let mut m = Tree::new();
        m.insert(1u,2u);
        let (_,mut tokenOpt) = m.find_internal(&1);
        let token = &mut tokenOpt.take_unwrap();
        unsafe{
            m.take_unwrap_token(token);
            m.take_unwrap_token(token); //should fail
        }
    }
}
