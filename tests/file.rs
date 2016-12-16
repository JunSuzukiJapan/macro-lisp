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
            (with_output_to_new_file (file filename)
                (file.write_all b"Hello, world!"))
            (defvar s String::new())
            (with_input_from_file (file filename)
                (read_to_string file s))
            (assert_eq "Hello, world!" s)
        );
    }
}