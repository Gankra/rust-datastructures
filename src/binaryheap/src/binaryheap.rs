use std::cmp::PartialOrd;
use std::iter::range_inclusive;

pub struct BinaryHeap<T>{
    elements: Vec<T> 
}

impl<T: PartialOrd> BinaryHeap<T> {
    pub fn new() -> BinaryHeap<T> {
        BinaryHeap{elements: Vec::new()}
    }

    pub fn push(&mut self, element: T) {
        self.elements.push(element);
        let index = self.len() - 1;
        self.bubble_up(index);
    }

    pub fn pop(&mut self) -> Option<T> {
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

    pub fn peek<'a>(&'a self) -> Option<&'a T> {
        if self.is_empty() {
            None
        } else {
            Some(self.elements.get(0))
        }
    }
} 

impl<T: PartialOrd> BinaryHeap<T> {
    fn bubble_up(&mut self, mut index: uint){
        let mut parentIndex = parent(index);
        while index > 0 && self.elements.get(index) > self.elements.get(parentIndex) {
            self.swap(index, parentIndex);
            index = parentIndex;
            parentIndex = parent(index);
        }
    }

    fn bubble_down(&mut self, mut index: uint){
        loop{
            let leftIndex = left(index);
            let rightIndex = right(index);
            if self.is_in_bounds(leftIndex) && self.elements.get(index) < self.elements.get(leftIndex)
                && (!self.is_in_bounds(rightIndex) || self.elements.get(rightIndex) < self.elements.get(leftIndex)) {
                self.swap(index, leftIndex);
                index = leftIndex;
            } else if self.is_in_bounds(rightIndex) && self.elements.get(index) < self.elements.get(rightIndex) {
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

impl <T: PartialOrd> Collection for BinaryHeap<T> {
    fn len(&self) -> uint {
        self.elements.len()
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

pub fn make_heap<T: Ord>(elements: Vec<T>) -> BinaryHeap<T>{
    let mut heap = BinaryHeap{elements: elements};
    
    let firstParent = parent(heap.len() - 1);  

    for i in range_inclusive(0, firstParent).rev() {
        heap.bubble_down(i);
    }

    heap
}

fn main(){
    let mut heap1 = BinaryHeap::new();

    heap1.push(1u);
    heap1.push(2);
    heap1.push(5);
    heap1.push(3);
    heap1.push(10);
    heap1.push(-3);
    heap1.push(100);

    println!("{}", heap1.elements);

    println!("{}", heap1.peek());

    println!("{}", heap1.pop());
    println!("{}", heap1.pop());
    println!("{}", heap1.pop());

    

    let mut heap2 = make_heap(vec![6u,1,0,9,100,3,4,2,8,15,10]);

    println!("{}", heap2.elements);

    println!("{}", heap2.pop());
    println!("{}", heap2.pop());
    println!("{}", heap2.pop());



}