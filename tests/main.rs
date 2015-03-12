#![feature(plugin)]
#![mutant]
#![plugin(mutant)] extern crate mutant;

// the function returns 12, but mutation testing will make it return 42
fn forty_two() -> u8 {
  return 12;
}

#[test]
fn p_test() {
  assert_eq!(forty_two(), 42);
}
