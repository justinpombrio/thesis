#![feature(log_syntax)]
#![allow(unused_macros)]
#![allow(unused_variables)]

#[macro_use]
mod example;
mod ds_order;
mod safety;
mod power;

use ds_order::ds_order;
use safety::safety;
use power::power;

fn main() {
    println!("# Example #");
    foreach!(x, &[1, 2, 3], {
        println!("{}", x);
    });
    println!();
    println!("# Order of evaluation #");
    ds_order();
    println!();
    println!("# Safety #");
    safety();
    println!();
    println!("# Power #");
    power();
    println!();
}
