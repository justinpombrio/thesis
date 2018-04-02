macro_rules! foreach {
    ($x:ident, $list:expr, $body:block) => {
        for $x in $list {
            $body
        }
    }
}
