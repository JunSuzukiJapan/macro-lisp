// run command:
//    $ rustup run nightly cargo run --example fizzbuzz
//
// if not install nightly, then
//    $ rustup install nightly

extern crate macro_lisp;

use macro_lisp::lisp;

lisp!(defun main() ()
    (dotimes (count 100)
        (defconstant num (1+ count))
        (if (== 0 (% num 3))
            (if (== 0 (% num 5))
                (println "FizzBuzz")
                (println "Fizz"))
            (if (== 0 (% num 5))
                (println "Buzz")
                (println "{}" num))))
);
