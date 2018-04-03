macro_rules! use_list {
    (5) => ({
        println!("five");
    });
    ($other:expr) => ({
        println!("other");
    })
}

// Notice that this macro is never invoked!
macro_rules! list {
    () => (5)
}

macro_rules! outer {
    ($arg:expr) => ({
        log_syntax!("Begin outer call");
        let answer = $arg;
        log_syntax!("Argument to outer is:" $arg);
        log_syntax!("End outer call");
        answer
    })
}

macro_rules! inner {
    ($arg:expr) => ({
        log_syntax!("Begin inner call");
        let answer = $arg;
        log_syntax!("End inner call");
        answer
    })
}

fn outer(_arg: ()) -> () {
    println!("Begin runtime outer call");
    println!("End runtime outer call");
}

fn inner(_arg: usize) -> () {
    println!("Begin runtime inner call");
    println!("End runtime inner call");
}

pub fn ds_order() {
    log_syntax!("# Order of desugaring #");
    outer!(inner!(5)); // OI* macro expansion order.
    outer(inner(5)); // IO runtime evaluation order.
    use_list!(list!(5)); // Notice that 'list!' is never invoked.
    log_syntax!();
}
