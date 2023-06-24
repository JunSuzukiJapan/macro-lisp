// run command:
//    $ rustup run nightly cargo run --example factorial
//
// if not install nightly, then
//    $ rustup install nightly

extern crate macro_lisp;

use macro_lisp::lisp;

lisp!(defun factorial ((n i32)) i32
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))));

lisp!(defun main () ()
    (defconstant num (factorial 10))
    (println "10! = {}" num));
