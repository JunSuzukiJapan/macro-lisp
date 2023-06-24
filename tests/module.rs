#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(dead_code)]
//#![feature(trace_macros)]

extern crate macro_lisp;

use macro_lisp::lisp;

//trace_macros!(true);

lisp!(module module_test
    (defun do_nothing())

    (defun hello () ()
        (println "Hello")
    )

    (defun add ((x i32) (y i32)) i32
        (+ x y)
    )

    (#[test] defun test_add () ()
        (defconstant num (add 1 2))
        (assert-eq 3 num)
    )
);
