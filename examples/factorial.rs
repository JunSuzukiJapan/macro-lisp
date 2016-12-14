// run command:
//    $ rustup run nightly cargo run  --example factorial
//
// if not install nightly, then
//    $ rustup install nightly

//#![feature(trace_macros)]

#[macro_use]
extern crate macro_lisp;

//trace_macros!(true);

lisp!(defun factorial ((n i32)) i32
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))));

//trace_macros!(false);

fn main(){
    let num = factorial(10);
    println!("10! = {}", num);  // 3628800
}