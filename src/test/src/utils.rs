use std::num;
use std::iter::Range;

#[macro_export]
macro_rules! use_test(
    ($name:ident, $test:expr) => (       
        #[test]
        fn $name () {
            $test;
        }
    );
)

pub trait Countable {
    fn biject(number: uint) -> Self;
}

impl Countable for uint {
    fn biject (number: uint) -> uint { number }
}

impl Countable for (uint, uint) {
    fn biject (number: uint) -> (uint, uint) { (number, number) }
}

enum Order { Ordered, Unordered }
pub struct Sequence <C> {
    order: Order,
    range: Range<uint>,
    max: uint,
    base: uint,
}

impl <C: Countable> Iterator <C> for Sequence <C> {
    fn next(&mut self) -> Option<C> {
        let next = match self.order {
            Ordered => self.range.next(),
            Unordered => self.range.next().map(|i| (i * self.base) % self.max) // Hooray for group theory! 
        };
        next.map(|i| Countable::biject(i))
    }
}

pub fn ordered_sequence <C:Countable> (magnitude: uint) -> Sequence<C> {
	Sequence{ order: Ordered, range: range(0, num::pow(10, magnitude)), max :0, base: 0 }
}

pub fn unordered_sequence <C:Countable> (magnitude: uint) -> Sequence<C> {
	let max = num::pow(10, magnitude);
	let base = 7*num::pow(9, magnitude - 1); // Halfish-sized relatively prime number to max
    Sequence{ order: Unordered, range: range(0, max), max: max, base: base }
}