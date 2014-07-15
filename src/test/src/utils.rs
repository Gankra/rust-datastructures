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

pub struct UnorderedSequence {
    range: Range<uint>,
    max: uint,
    base: uint,
}

impl Iterator<uint> for UnorderedSequence {
    fn next(&mut self) -> Option<uint> {
        self.range.next().map(|i| (i * self.base) % self.max) // Hooray for group theory!
    }
}

pub fn ordered_sequence (magnitude: uint) -> Range<uint> {
	range(0, num::pow(10, magnitude))
}

pub fn unordered_sequence (magnitude: uint) -> UnorderedSequence {
	let max = num::pow(10, magnitude);
	let base = 7*num::pow(9, magnitude - 1); // Halfish-sized relatively prime number to max
    UnorderedSequence{range: range(0, max), max: max, base: base}
}