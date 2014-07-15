use std::collections::{Map, MutableMap};
use std::default::Default;
use std::iter::FromIterator;
use utils::unordered_sequence;

pub fn test_insert <M: MutableMap<uint, uint> + Default> () {
    let mut map:M = Default::default();
    //inserting empty
    assert!(map.insert(1, 1));
    assert_eq!(map.len(), 1);
    //inserting new key
    assert!(map.insert(2, 2));
    assert_eq!(map.len(), 2);
    //inserting pre-existing key
    assert!(!map.insert(1, 2));
    assert_eq!(map.len(), 2);
}

pub fn test_swap <M: MutableMap<uint, uint> + Default> () {
    let mut map:M = Default::default();
    //swaping empty
    assert_eq!(map.swap(1, 2), None);
    assert_eq!(map.len(), 1);
    //swaping new key
    assert_eq!(map.swap(2, 3), None);
    assert_eq!(map.len(), 2);
    //swaping pre-existing key
    assert_eq!(map.swap(1, 4), Some(2));
    assert_eq!(map.len(), 2);
    //confirm swap really occured
    assert_eq!(map.find(&1), Some(&4));
}

pub fn test_remove <M: MutableMap<uint, uint> + Default> () {
    let mut map:M = Default::default();
    map.insert(1,1);
    //deleting non-existent key
    assert!(!map.remove(&2));
    assert_eq!(map.len(), 1);
    //deleting existing key
    assert!(map.remove(&1));
    assert_eq!(map.len(), 0);
}

pub fn test_pop <M: MutableMap<uint, uint> + Default> () {
    let mut map:M = Default::default();
    map.insert(1,2);
    //poping non-existent key
    assert_eq!(map.pop(&2), None);
    assert_eq!(map.len(), 1);
    //poping existing key
    assert_eq!(map.pop(&1), Some(2));
    assert_eq!(map.len(), 0);
}

pub fn test_find_mut <M: MutableMap<uint, uint> + FromIterator<(uint, uint)>> () {
    let values = vec![(1u,2u),(2,3),(3,4),(4,5)];
    let mut map:M = FromIterator::from_iter(values.move_iter());
    //finding existing key
    assert_eq!(map.find_mut(&2), Some(&mut 3));
    //finding non-existent key
    assert_eq!(map.find_mut(&7), None);
}


//Immutables
pub fn test_contains <M: Map<uint, uint> + FromIterator<(uint, uint)>> () {
    let values = vec![(1u,2u),(2,3),(3,4),(4,5)];
    let map:M = FromIterator::from_iter(values.move_iter());
    //finding existing key
    assert!(map.contains_key(&3));
    //finding non-existent key
    assert!(!map.contains_key(&7));
}

pub fn test_find <M: Map<uint, uint> + FromIterator<(uint, uint)>> () {
    let values = vec![(1u,2u),(2,3),(3,4),(4,5)];
    let map:M = FromIterator::from_iter(values.move_iter());
    //finding existing key
    assert_eq!(map.find(&2), Some(&3));
    //finding non-existent key
    assert_eq!(map.find(&7), None);
}

// Many maps only do some interesting things when sufficiently large, 
// or after sufficiently large sequences of operations; let's try!
pub fn test_integration <M: MutableMap<uint, uint> + Default> () {
    let mut values = unordered_sequence::<(uint, uint)>(2);
    let mut map:M = Default::default();

    // fill it up
    let mut count = 0u;
    for (key, value) in values {
        count += 1;
        assert!(map.insert(key, value));
        assert_eq!(map.len(), count);
    }

    // ensure search works
    values = unordered_sequence(2);
    for (key, value) in values {
        assert_eq!(map.find(&key), Some(&value));
        assert_eq!(map.len(), count);
    }

    // ensure remove works
    let mut reduced_values = unordered_sequence::<(uint, uint)>(2).take(80).skip(30);
    for (key, value) in reduced_values {
        count -= 1;
        assert_eq!(map.pop(&key), Some(value));
        assert_eq!(map.len(), count);
    }

    // ensure search still works
    let mut index = 0u;
    values = unordered_sequence(2);
    for (key, value) in values {
        if index >= 30 && index < 80{
            assert_eq!(map.find(&key), None);    
        } else {
            assert_eq!(map.find(&key), Some(&value));
        }
        
        index += 1;
    }

    // ensure swap still works
    index = 0;
    values = unordered_sequence(2);
    for (key, value) in values {
        if index >= 30 && index < 80{
            assert_eq!(map.swap(key, value), None);  
            count += 1;  
        } else {
            assert_eq!(map.swap(key, value + 1), Some(value));
        }
        assert_eq!(map.len(), count);
        index += 1;
    }

    // ensure search *still* works
    index = 0;
    values = unordered_sequence(2);
    for (key, value) in values {
        if index >= 30 && index < 80{
            assert_eq!(map.find(&key), Some(&value));    
        } else {
            assert_eq!(map.find(&key), Some(&(value + 1)));
        }
        index += 1;
    }
}

#[cfg(test)]
mod test {
    use super::{test_insert, test_swap, test_remove, test_pop, test_contains, test_find, test_find_mut, test_integration};
    use std::collections::TreeMap;

    use_test!(test_insert_tree, test_insert::<TreeMap<uint, uint>>())
    use_test!(test_swap_tree, test_swap::<TreeMap<uint, uint>>())
    use_test!(test_remove_tree, test_remove::<TreeMap<uint, uint>>())
    use_test!(test_pop_tree, test_pop::<TreeMap<uint, uint>>())
    use_test!(test_contains_tree, test_contains::<TreeMap<uint, uint>>())
    use_test!(test_find_tree, test_find::<TreeMap<uint, uint>>())
    use_test!(test_find_mut_tree, test_find_mut::<TreeMap<uint, uint>>())
    use_test!(test_integration_tree, test_integration::<TreeMap<uint, uint>>())
}