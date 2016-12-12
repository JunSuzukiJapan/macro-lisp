#[macro_use]
extern crate lisp;

#[cfg(test)]
mod tests {
    #[test]
    fn test_let(){
        lisp!(let x 3);
        assert_eq!(3, x);
    }
}