#[macro_use]
extern crate macro_lisp;

lisp!(pub module module_test
    (pub defun hello () ()
        (println "Hello")
    )
);

pub mod test {
    pub fn hello2(){
        println!("Hello");
    }
}

/*
lisp!(defun main () ()
    (test::hello2)
);
*/

fn main(){
    test::hello2();
}
