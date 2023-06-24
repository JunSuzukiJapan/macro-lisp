#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(dead_code)]
#![allow(unreachable_code)]
//#![feature(trace_macros)]

extern crate macro_lisp;

#[cfg(test)]
mod tests {
    use macro_lisp::lisp;

    #[test]
    fn test_while_let() {
        lisp!(progn
            (defvar num Some(0))
            (while-let (Some(i) = num)
                (if (> i 9)
                    (setf num None)
                    (setf num Some(i + 1))
                )
            )
            (assert-eq None num)
        );
    }

    #[test]
    fn test_if_let() {
        lisp!(progn
            (defconstant number Some(7))
            (if-let (Some(i) = number)
                // success let
                (assert-eq 7 i)
                // else
                (panic "fail if-let")
            )
        );
    }

    lisp!(defstruct Person
        (pub // public members
            (name String)
            (address String)
        )
        (    // non-public members
         (age i32)
        )
    );

    lisp!(defstruct Person2
        (pub (age i32))
    );

    lisp!(defstruct Point<T>
        ((x T) (y T))
    );

    #[test]
    fn test_struct() {
        lisp!(progn);
    }

    #[test]
    fn test_match() {
        lisp!(progn
            (defconstant s "test")

            (defconstant x (match s
                ("test" => (1))
                (_ =>  (-1))))
            (assert-eq 1 x)

            (match s
                ("hello" => (println "world"))
                (_ => (println "Hum?")))
        );
    }

    #[test]
    fn test_lambda() {
        lisp!(progn
            (defconstant f
                (lambda ((x i32)) (1+ x)))
            (defconstant x (f 5))
            (assert-eq 6 x)

            (defconstant y ((lambda ((x i32)) (* x x)) 4))
            (assert-eq 16 y)
        );
    }

    #[test]
    fn test_loop_break() {
        lisp!(let ((x 0))
            (loop
                (loop
                    (break)
                    (incf x)
                )
                (incf x)
                (break)
            )
            (assert-eq 1 x)
        );
    }

    #[test]
    fn test_doiter() {
        let vec = lisp!(vec 1 2 3 4 5);
        lisp!(let ((x 0))
            (doiter (num vec)
                (setf x (+ x num)))
            (assert-eq 15 x)
        );
        lisp!(let ((x 0))
            (doiter (num (vec 1 2 3 4 5))
                (setf x (+ x num)))
            (assert-eq 15 x)
        );
    }

    #[test]
    fn test_do() {
        lisp!(progn
            (defconstant num
                (do ((x 0 (1+ x))
                     (y 0 (+ y 2)))
                ((> x 5) y)))
            (assert-eq num 12)
        );
    }

    #[test]
    fn test_let() {
        lisp!(progn
            (defconstant x 3)
            (defconstant y 5)
            (let ((x 1)
                  (y 2))
                (incf x)
                (decf y)
                (assert-eq x 2)
                (assert-eq y 1))
            (assert-eq x 3)
            (assert-eq y 5)
        );
    }

    #[test]
    fn test_dotimes() {
        lisp!(progn
            (defvar x 0)
            (dotimes (y 5)
                (setf x (+ x y)))
            (assert-eq x 10)
        );
    }

    #[test]
    fn test_while() {
        lisp!(progn
            (defvar x 0)
            (while (< x 10)
                (incf x))
            (assert-eq x 10)
        );
    }

    #[test]
    fn test_when_unless() {
        lisp!(progn
            (defvar x 0)
            (when true
                (setf x 1))
            (when false
                (setf x 2))
            (assert-eq 1 x)

            (defvar y 0)
            (unless true
                (setf y 1))
            (unless false
                (setf y 2))
            (assert-eq y 2)
        );
    }

    #[test]
    fn test_progn() {
        lisp!(progn
            (defconstant x 3)
            (defconstant y 4)
            (defvar z (* x y))
            (assert-eq 12 z)
        );
    }

    #[test]
    fn test_if() {
        lisp!(if (eq 1 1) (println "equal"));
        lisp!(if (eq 2 2) (println "equal") (println "not equal"));
        let x = lisp!(if true (+ 1 1) (+ 2 2));
        assert_eq!(2, x);

        lisp!(if (<= 1 2) (println "True") (println "False"));
    }

    lisp!(defun hello () ()
        (println "Hello")
    );

    lisp!(defun add1 ((x i32)) i32
        (+ x 1)
    );

    lisp!(defun add ((x i32) (y i32)) i32
        (+ x y)
    );

    lisp!(defun do_nothing());

    #[test]
    fn test_defun() {
        let x = lisp!(add1 5);
        assert_eq!(6, x);

        let x = lisp!(add 3 4);
        assert_eq!(7, x);

        do_nothing();
    }

    #[test]
    fn test_macro_utils() {
        lisp!(print "hello, {}" "world");
    }

    #[test]
    fn test_defconstant() {
        lisp!(defconstant x 3);
        assert_eq!(3, x);

        lisp!(defconstant (y &str) "hello");
    }

    #[test]
    fn test_defvar() {
        lisp!(defvar x 0);
        assert_eq!(0, x);
        lisp!(setf x 1);
        assert_eq!(1, x);

        lisp!(defvar (x i64) 5);
        lisp!(defvar (s String) "test".to_owned());
    }
}
