#[macro_export]
macro_rules! lisp {
    (let $var:ident $e:expr) => (let $var = $e;);
}