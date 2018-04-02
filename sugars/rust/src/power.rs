// Macros can abstract over patterns.
// (According to the docs, they can abstract over expressions, statements, items, and patterns)
macro_rules! pattern {
    ($x:pat, $y:pat) => (($y, $x))
}

// Macros can define macros
macro_rules! macro_defining_macro {
    ($e:expr) => {
        macro_rules! a_macro_defined_macro {
            () => ($e)
        }
    }
}
macro_defining_macro!("Macros can define macros.");

pub fn power() {
    let pattern!(x, y) = ("first", "second");
    println!("Macros can expand into patterns - x:{}, y:{}", x, y);
    println!("{}", a_macro_defined_macro!());
}
