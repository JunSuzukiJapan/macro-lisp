// run command:
//    $ rustup run nightly cargo run --example hello
//
// if not install nightly, then
//    $ rustup install nightly

#[macro_use]
extern crate macro_lisp;

lisp!(pub module module_test
    (pub defun hello () ()
        (println "Hello, macro-lisp!")
    )
);

lisp!(defun main () ()
    (module_test::hello)
);
