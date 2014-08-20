use std::cmp::PartialOrd;
use std::iter::range_inclusive;
use std::default::Default;
use coltests::priorityqueue::PriorityQueue;

pub struct BinaryHeap<T>{
    elements: Vec<T>
}

impl<T: PartialOrd> BinaryHeap<T> {
    pub fn new() -> BinaryHeap<T> {
        BinaryHeap{elements: Vec::new()}
    }
}

impl<T: PartialOrd> BinaryHeap<T> {
    fn bubble_up(&mut self, mut index: uint){
        let mut parentIndex = parent(index);
        while index > 0 && self.elements[index] < self.elements[parentIndex] {
            self.swap(index, parentIndex);
            index = parentIndex;
            parentIndex = parent(index);
        }
    }

    fn bubble_down(&mut self, mut index: uint){
        loop{
            let leftIndex = left(index);
            let rightIndex = right(index);
            if self.is_in_bounds(leftIndex) && self.elements[index] > self.elements[leftIndex]
                && (!self.is_in_bounds(rightIndex) || self.elements[rightIndex] > self.elements[leftIndex]) {
                self.swap(index, leftIndex);
                index = leftIndex;
            } else if self.is_in_bounds(rightIndex) && self.elements[index] > self.elements[rightIndex] {
                self.swap(index, rightIndex);
                index = rightIndex;
            } else {
                break;
            }
        }
    }

    fn swap(&mut self, indexA: uint, indexB: uint) {
        self.elements.as_mut_slice().swap(indexA, indexB);
    }

    fn is_in_bounds(&self, index: uint) -> bool {
        index < self.elements.len()
    }
}

impl <T: Ord> PriorityQueue <T> for BinaryHeap <T> {
    fn push(&mut self, element: T) {
        self.elements.push(element);
        let index = self.len() - 1;
        self.bubble_up(index);
    }

    fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            let lastIndex = self.len() - 1;
            self.swap(0, lastIndex);
            let result = self.elements.pop();
            self.bubble_down(0);

            result
        }
    }

    fn peek<'a>(&'a self) -> Option<&'a T> {
        if self.is_empty() {
            None
        } else {
            Some(&self.elements[0])
        }
    }
}

impl <T: PartialOrd> Collection for BinaryHeap <T> {
    fn len(&self) -> uint {
        self.elements.len()
    }
}


impl <T:Ord> Default for BinaryHeap <T> {
    fn default() -> BinaryHeap <T> {
        BinaryHeap::new()
    }
}

impl <T:Ord> Extendable<T> for BinaryHeap<T> {
    fn extend <I: Iterator<T>> (&mut self, mut iter: I) {
        for value in iter {
            self.push(value)
        }
    }
}

impl <T:Ord> FromIterator<T> for BinaryHeap<T> {
    fn from_iter <I: Iterator<T>> (iter: I) -> BinaryHeap<T> {
        let buffer = FromIterator::from_iter(iter);
        make_heap(buffer)
    }
}

impl <T:Ord> Mutable for BinaryHeap<T> {
    fn clear (&mut self) {
        self.elements = Vec::new();
    }
}

fn left(index: uint) -> uint {
    2*index + 1
}

fn right(index: uint) -> uint {
    2*index + 2
}

fn parent(index: uint) -> uint {
    (index - 1)/2
}

fn make_heap<T: Ord>(elements: Vec<T>) -> BinaryHeap<T>{
    let mut heap = BinaryHeap{elements: elements};

    let firstParent = parent(heap.len() - 1);

    for i in range_inclusive(0, firstParent).rev() {
        heap.bubble_down(i);
    }

    heap
}

#[cfg(test)]
mod test {
    use super::BinaryHeap;
    use coltests::collection;
    use coltests::priorityqueue;

    type ToTest = BinaryHeap<uint>;

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
    use super::BinaryHeap;

    use coltests::collection;
    use coltests::priorityqueue;
    use coltests::utils;
    use test::Bencher;

    bench_priorityqueue!(BinaryHeap<uint>)
}