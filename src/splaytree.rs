extern crate bst;

use bst::{Tree, NodeRefToken};
use std::default::Default;
use std::fmt::Show;

// A SplayTree is an adaptive binary search tree optimized for
// effeciently performing access sequences, and not individual accesses themselves
// Any time an element is accessed, it is "splayed". 
// 
// A splay is a special series of rotations to bring a node to the root
// While splaying incurs a substantial overhead over just searching, it provides
// excellent theoretical guarantees for the performance of future searches.
//
// For instance, search for all the keys in the tree, in increasing order, is an
// O(n) operation, rather than the usual O(nlogn) for a self-balancing tree.
//
// Note that as a consequence, splay trees can become *very* unbalanced, and
// individual searches may take O(n).
struct SplayTree<K,V> {
	tree: Tree<K,V>
}

impl <K:Ord, V> SplayTree <K, V> {
	fn new () -> SplayTree<K,V> {
		SplayTree{tree: Tree::new()}
	}

    fn find <'a> (&'a mut self, key: &K) -> Option<&'a V> {
        self.find_mut(key).map(|x| &*x)
    }

	fn contains (&mut self, key: &K) -> bool {
        self.find(key).is_some()
    }

    fn splay (&mut self, token: &mut NodeRefToken<K,V>) {
        enum SplayType {NoSplay, Zig, ZigZig, ZigZag};
        
        let node = unsafe{ self.tree.take_unwrap_token_mut(token) };
        loop {
            let splayType = match node.get_parent() {
                None => NoSplay,
                Some(parent) => match parent.get_parent() {
                    None => Zig,
                    Some(_) => if parent.is_left() == node.is_left() {
                        ZigZig
                    } else {
                        ZigZag
                    }
            	}
            };

            match splayType {
                NoSplay => return,
                Zig => self.tree.rotate_up(node),
                ZigZag => {
                    self.tree.rotate_up(node); 
                    self.tree.rotate_up(node);
                }
                ZigZig => {
                    self.tree.rotate_up(node.get_parent_mut().unwrap()); 
                    self.tree.rotate_up(node)
                }
            }
        }
    }

    fn find_mut <'a> (&'a mut self, key: &K) -> Option<&'a mut V> {
        let (mut parentToken, mut nodeToken) = self.tree.find_internal(key);
        match &mut nodeToken {
            &None => match &mut parentToken {
                &None => None,
                &Some(ref mut token) => {
                    self.splay(token);
                    None
                }
            },
            &Some(ref mut token) => {
                self.splay(token);
                Some(&mut self.tree.root.as_mut().unwrap().value)
            }
        }
    }

    fn insert (&mut self, key: K, value: V) -> bool {
        self.swap(key, value).is_none()
    }

    fn swap (&mut self, key: K, value: V) -> Option<V> {
        //splay SplayTrees insert naively and then splay the inserted node
        let (added, mut token) = self.tree.insert_internal(key, value);
        if token.is_some() {
            self.splay(token.as_mut().unwrap());
        }
        added
    }

    fn remove (&mut self, key: &K) -> bool {
        self.pop(key).is_some()
    }

    fn pop (&mut self, key: &K) -> Option<V>{
        // Splay SplayTrees delete by splaying the node to delete and removing it.
        // Then the largest element in the root's left subtree is splayed,
        // and the right subtree is made the right child of the new root

        if self.contains(key) { //searching for key splays its node
            let mut root = self.tree.root.take_unwrap(); //remove the root from the SplayTree
            let result = root.value;

            if root.left.is_some() {
                self.tree.set_root(root.left.take());
                self.contains(key); //splay up the largest element in the left subtree
                self.tree.root.as_mut().unwrap().set_right(root.right);

            } else {
                // if there's no left subtree (we're deleting the smallest element in the SplayTree), 
                // just make the SplayTree the right subtree
                self.tree.set_root(root.right.take());
            }

            self.tree.len -= 1;
            
            Some(result)
        } else {
            None
        }
    }
}

impl <K: Ord, V> Extendable<(K,V)> for SplayTree<K,V> {
    fn extend <I: Iterator<(K,V)>> (&mut self, mut iter: I) {
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}

impl <K: Ord, V> FromIterator<(K,V)> for SplayTree<K,V> {
    fn from_iter <I: Iterator<(K,V)>> (iter: I) -> SplayTree<K,V> {
        let mut tree = SplayTree::new();
        tree.extend(iter);
        tree
    }
}

impl <K,V> Collection for SplayTree<K,V> {
    fn len(&self) -> uint{
        self.tree.len()
    }
}

impl <K,V> Mutable for SplayTree<K,V> {
    fn clear(&mut self) {
        self.tree.clear()
    }
}

impl <K:Ord,V:Show> Default for SplayTree<K,V> {
    fn default() -> SplayTree<K,V> {SplayTree::new()}
}

#[cfg(test)]
mod test{
    use super::SplayTree;

    #[test]
    fn find_empty() {
        let mut m: SplayTree<int,int> = SplayTree::new();
        assert!(m.find(&5) == None);
    }

    #[test]
    fn find_not_found() {
        let mut m = SplayTree::new();
        assert!(m.insert(1i, 2i));
        assert!(m.insert(5i, 3i));
        assert!(m.insert(9i, 3i));
        assert_eq!(m.find(&2), None);
    }

    #[test]
    fn test_find_mut() {
        let mut m = SplayTree::new();
        assert!(m.insert(1i, 12i));
        assert!(m.insert(2, 8));
        assert!(m.insert(5, 14));
        let new = 100;
        match m.find_mut(&5) {
          None => fail!(), Some(x) => *x = new
        }
        assert_eq!(m.find(&5), Some(&new));
    }

    #[test]
    fn insert_replace() {
        let mut m = SplayTree::new();
        assert!(m.insert(5i, 2i));
        assert!(m.insert(2, 9));
        assert!(!m.insert(2, 11));
        assert_eq!(m.find(&2).unwrap(), &11);
    }

    #[test]
    fn test_remove(){
        let mut m = SplayTree::new();
        m.insert(1u, 2u);
        m.insert(2, 3);
        m.insert(4, 3);
        m.insert(5, 3);
        m.insert(6, 3);

        assert!(m.remove(&2));
        assert!(!m.remove(&3));
        assert!(m.remove(&4));
        assert!(!m.remove(&10));
        assert!(m.remove(&6));
    }
    
    #[test]
    fn test_clear() {
        let mut m = SplayTree::new();
        m.clear();
        assert!(m.insert(5i, 11i));
        assert!(m.insert(12, -3));
        assert!(m.insert(19, 2));
        m.clear();
        assert!(m.find(&5).is_none());
        assert!(m.find(&12).is_none());
        assert!(m.find(&19).is_none());
        assert!(m.is_empty());
    }

    #[test]
    fn u8_map() {
        let mut m = SplayTree::new();

        let k1 = "foo".as_bytes();
        let k2 = "bar".as_bytes();
        let v1 = "baz".as_bytes();
        let v2 = "foobar".as_bytes();

        m.insert(k1.clone(), v1.clone());
        m.insert(k2.clone(), v2.clone());

        assert_eq!(m.find(&k2), Some(&v2));
        assert_eq!(m.find(&k1), Some(&v1));
    }

    #[test]
    fn test_len() {

        let mut m = SplayTree::new();
        assert!(m.insert(3i, 6i));
        assert_eq!(m.len(), 1);
        assert!(m.insert(0, 0));
        assert_eq!(m.len(), 2);
        assert!(m.insert(4, 8));
        assert_eq!(m.len(), 3);
        assert!(m.remove(&3));
        assert_eq!(m.len(), 2);
        assert!(!m.remove(&5));
        assert_eq!(m.len(), 2);
        assert!(m.insert(2, 4));
        assert_eq!(m.len(), 3);
        assert!(m.insert(1, 2));
        assert_eq!(m.len(), 4);
    }

    #[test]
    fn test_from_iter() {
        let xs = [(1i, 1i), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6)];

        let mut map: SplayTree<int, int> = xs.iter().map(|&x| x).collect();

        for &(k, v) in xs.iter() {
            assert_eq!(map.find(&k), Some(&v));
        }
    }
}