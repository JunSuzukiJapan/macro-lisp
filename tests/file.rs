//#![feature(trace_macros)]
#![allow(unused_must_use)]
#![allow(unused_imports)]

#[macro_use]
extern crate macro_lisp;

#[cfg(test)]
mod file_tests {
    use macro_lisp::*;

    //trace_macros!(true);
    #[test]
    fn test_with_input_from_file(){
        lisp!(progn
            (defconstant filename "target/dummy.data")
            (with-output-to-new-file (file filename)
                (write-all file b"Hello, world!"))
            (defvar s String::new())
            (with-input-from-file (file filename)
                (read-to-string file s))
            (assert-eq "Hello, world!" s)
        );
    }
}