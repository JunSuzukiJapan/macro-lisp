// run command:
//    $ rustup run nightly cargo run --example wc examples/wc.rs
//
// if not install nightly, then
//    $ rustup install nightly

#[macro_use]
extern crate macro_lisp;

lisp!(use std::env);
lisp!(use std::fs::File);
lisp!(use std::io::Read);

fn is_whitespace(b: u8) -> bool {
    match b {
        0x20 | 0x09 | 0x85 | 0x0a | 0x0b | 0x0c | 0x0d => true,
        _ => false,
    }
}

fn main(){
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("usage: wc file");
        std::process::exit(0);
    }

    let ref path = args[1];
    let file = File::open(path).unwrap();
    
    let mut char_count = 0;
    let mut word_count = 0;
    let mut line_count = 0;
    let mut in_word = false;
    for byte in file.bytes() {
        char_count += 1;

        let b = byte.unwrap();
        if b == 0x0a {
            line_count += 1;
        }

        if in_word {
            if is_whitespace(b) {
                in_word = false;
            }
        }else{
            if ! is_whitespace(b) {
                in_word = true;
                word_count += 1;
            }
        }
    }

    println!("{:>10} {:>10} {:>10} {}", line_count, word_count, char_count, path);
}