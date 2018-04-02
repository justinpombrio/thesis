
// No type error - not type safe!
macro_rules! ill_typed {
    () => {
        1 + "two";
    }
}

// No unbound id error - not scope safe.
macro_rules! ill_scoped {
    () => ({
        x
    })
}

// Rust macros are hygienic
macro_rules! hygienic {
    ($i:ident, $body:expr) => {
        let x = "macro";
        let $i = "user";
        println!("macro->{}", x);
        let x = "macro";
        $body;
    }
}

// This macro has a lurking syntax error, which won't be found until it is used.
macro_rules! syntax_unsafe {
    ($e:expr) => {
        let $e = 0; // Error: e is an expr, not a pattern!
        ()
    }
}

pub fn safety() {
    hygienic!(x, println!("user->{}", x));
    // syntax_unsafe!(1 + 2); // Syntax error when macro is used
    // ill_typed!(); // Type error when macro is used
}
