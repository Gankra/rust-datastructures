use std::default::Default;
use std::iter::{FromIterator, Extendable};

use super::utils::ordered_sequence;

pub fn test_empty <C: Collection + Default> () {
    let col:C = Default::default();
    assert!(col.is_empty());
    assert_eq!(col.len(), 0);
}

pub fn test_from_iter <C: Collection + FromIterator<(uint, uint)>> () {
    let col:C = FromIterator::from_iter(ordered_sequence(1).zip(ordered_sequence(1)));
    assert!(!col.is_empty());
    assert_eq!(col.len(), 10);
}

pub fn test_extend <C: Collection + Default + Extendable<(uint, uint)>> () {
    let mut col:C = Default::default();
    col.extend(ordered_sequence(1).zip(ordered_sequence(1)));
    assert!(!col.is_empty());
    assert_eq!(col.len(), 10);
}

pub fn test_clear <C:Collection + FromIterator<(uint, uint)> + Mutable> () {
    let mut col:C = FromIterator::from_iter(ordered_sequence(1).zip(ordered_sequence(1)));
    col.clear();
    assert!(col.is_empty());
    assert_eq!(col.len(), 0);
}

#[cfg(test)]
mod test {
    use super::{test_empty, test_from_iter, test_extend, test_clear};
    use std::collections::TreeMap;

    use_test!(vec_empty, test_empty::<Vec<(uint, uint)>>())
    use_test!(vec_from_iter, test_from_iter::<Vec<(uint, uint)>>())
    use_test!(vec_extend, test_extend::<Vec<(uint, uint)>>())
    use_test!(vec_clear, test_clear::<Vec<(uint, uint)>>())

    use_test!(map_empty, test_empty::<TreeMap<uint, uint>>())
    use_test!(map_from_iter, test_from_iter::<TreeMap<uint, uint>>())
    use_test!(map_extend, test_extend::<TreeMap<uint, uint>>())
    use_test!(map_clear, test_clear::<TreeMap<uint, uint>>())
}