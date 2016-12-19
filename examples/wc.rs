// run command:
//    $ rustup run nightly cargo run --example wc examples/wc.rs
//
// if not install nightly, then
//    $ rustup install nightly

#[macro_use]
extern crate macro_lisp;

lisp!(use std::env);
lisp!(use std::process::exit);

lisp!(defun is_whitespace ((b u8)) bool
    (match b
        (0x20 | 0x09 | 0x85 | 0x0a | 0x0b | 0x0c | 0x0d => (true))
        (_ => (false) ))
);

lisp!(defun main () ()
    (defconstant (args Vec<String>) env::args().collect())
    (if (< (len args) 2)
        (progn
            (println "usage: wc file")
            (exit 0)))

    (defvar char_count 0)
    (defvar word_count 0)
    (defvar line_count 0)
    (defvar in_word false)

    (defconstant path &args[1])
    (with-input-from-file (file path)
        (doiter (byte file.bytes())
            (incf char_count)

            (defconstant b byte.unwrap())
            (if (== b 0x0a)
                (incf line_count)
            )

            (if in_word
                (if (is_whitespace b)
                    (setf in_word false)
                )
                (if (! (is_whitespace b))
                    (progn
                        (setf in_word true)
                        (incf word_count)
                    )
                )
            )
        )
    )
    (println "{:>10} {:>10} {:>10} {}" line_count word_count char_count path)
);