use bst::{Tree, NodeRefToken};
use std::default::Default;
use std::fmt::Show;

/// A SplayTree is an adaptive binary search tree optimized for
/// effeciently performing access sequences, and not individual accesses themselves
/// Any time an element is accessed, it is "splayed".
///
/// A splay is a special series of rotations to bring a node to the root
/// While splaying incurs a substantial overhead over just searching, it provides
/// excellent theoretical guarantees for the performance of future searches.
///
/// For instance, search for all the keys in the tree, in increasing order, is an
/// O(n) operation, rather than the usual O(nlogn) for a self-balancing tree.
///
/// Note that as a consequence, splay trees can become *very* unbalanced, and
/// individual searches may take O(n).
pub struct SplayTree<K,V> {
	tree: Tree<K,V>
}

impl <K:Ord, V> SplayTree <K, V> {
	pub fn new () -> SplayTree<K,V> {
		SplayTree{tree: Tree::new()}
	}

    pub fn find <'a> (&'a mut self, key: &K) -> Option<&'a V> {
        self.find_mut(key).map(|x| &*x)
    }

	pub fn contains (&mut self, key: &K) -> bool {
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

    pub fn find_mut <'a> (&'a mut self, key: &K) -> Option<&'a mut V> {
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

    pub fn insert (&mut self, key: K, value: V) -> bool {
        self.swap(key, value).is_none()
    }

    pub fn swap (&mut self, key: K, value: V) -> Option<V> {
        //splay SplayTrees insert naively and then splay the inserted node
        let (added, mut token) = self.tree.insert_internal(key, value);
        if token.is_some() {
            self.splay(token.as_mut().unwrap());
        }
        added
    }

    pub fn remove (&mut self, key: &K) -> bool {
        self.pop(key).is_some()
    }

    pub fn pop (&mut self, key: &K) -> Option<V>{
        // Splay SplayTrees delete by splaying the node to delete and removing it.
        // Then the largest element in the root's left subtree is splayed,
        // and the right subtree is made the right child of the new root

        if self.contains(key) { //searching for key splays its node
            let mut root = self.tree.root.take_unwrap(); //remove the root from the SplayTree

            let left = root.left.take();
            let right = root.right.take();
            let result = root.value;

            if left.is_some() {
                self.tree.set_root(left);
                self.contains(key); //splay up the largest element in the left subtree
                self.tree.root.as_mut().unwrap().set_right(right);
            } else {
                // if there's no left subtree (we're deleting the smallest element in the SplayTree),
                // just make the SplayTree the right subtree
                self.tree.set_root(right);
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
mod test {
    use super::{SplayTree};
    use coltests::collection;
    //use coltests::map;

    type ToTest = SplayTree<uint, uint>;

    use_test!(empty, collection::test_empty::<ToTest>())
    use_test!(clear, collection::test_clear::<ToTest, _>())
    use_test!(from_iter, collection::test_from_iter::<ToTest, _>())
    use_test!(extend, collection::test_extend::<ToTest, _>())
    /*
    use_test!(insert, map::test_insert::<ToTest>())
    use_test!(swap, map::test_swap::<ToTest>())
    use_test!(remove, map::test_remove::<ToTest>())
    use_test!(pop, map::test_pop::<ToTest>())
    use_test!(contains, map::test_contains::<ToTest>())
    use_test!(find, map::test_find::<ToTest>())
    use_test!(find_mut, map::test_find_mut::<ToTest>())
    use_test!(integration, map::test_integration::<ToTest>())
    */
}

#[cfg(test)]
mod bench {
    use coltests::collection;
    use coltests::utils::ordered_sequence;
    use test::Bencher;

    #[bench]
    fn from_iter (bencher: &mut Bencher) {
        collection::bench_from_iter::<Vec<uint>, _, _>(ordered_sequence::<uint>(2), bencher);
    }
}