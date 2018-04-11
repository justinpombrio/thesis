macro_rules! if_let {
    ($p:pat, $e:expr, $b:block) => {
        // Alternatively: if let $p = $e { $b }
        match $e {
            $p => $b,
            _ => ()
        }
    }
}
