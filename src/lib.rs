#[macro_export]
macro_rules! lisp {
    // bool
    (false) => (false);
    (true) => (true);
    (self $(. $e:tt)* ) => (self $(. $e)* );

    // defstruct
    ( $(#[$m:meta])* defstruct $struct_name:ident < $($generic:ident),+ >
        (pub $( ($name:ident $typ:ty) )* )
        ( $( ($name2:ident $typ2:ty) )* )
    ) => (
        $(#[$m]);*
        struct $struct_name < $($generic),+ > {
            $( pub $name: $typ),*
            ,
            $( $name2: $typ2),*
        }
    );
    ( $(#[$m:meta])* defstruct $struct_name:ident < $($generic:ident),+ >
        (pub $( ($name:ident $typ:ty) )* )
    ) => (
        $(#[$m]);*
        struct $struct_name < $($generic),+ > {
            $( pub $name: $typ),*
        }
    );
    ( $(#[$m:meta])* defstruct $struct_name:ident < $($generic:ident),+ >
        ( $( ($name:ident $typ:ty) )* )
    ) => (
        $(#[$m]);*
        struct $struct_name < $($generic),+ > {
            $( $name: $typ ),*
        }
    );
    ( $(#[$m:meta])* defstruct $struct_name:ident
        (pub $( ($name:ident $typ:ty) )* )
        ( $( ($name2:ident $typ2:ty) )* )
    ) => (
        $(#[$m]);*
        struct $struct_name {
            $( pub $name: $typ),*
            ,
            $( $name2: $typ2),*
        }
    );
    ( $(#[$m:meta])* defstruct $struct_name:ident
        (pub $( ($name:ident $typ:ty) )* )
    ) => (
        $(#[$m]);*
        struct $struct_name {
            $( pub $name: $typ),*
        }
    );
    ( $(#[$m:meta])* defstruct $struct_name:ident
        ( $( ($name:ident $typ:ty) )* )
    ) => (
        $(#[$m]);*
        struct $struct_name {
            $( $name: $typ ),*
        }
    );
    ( $(#[$m:meta])* defstruct $struct_name:ident) => (
        $(#[$m]);*
        struct $struct_name;
    );

    // match
    (match $e:tt $( ( $pattern:pat $(| $pat2:pat)* => ( $($e2:tt)* ) ) )* ) => (
        match $crate::lisp_arg!($e) {
            $($pattern $(| $pat2)* => $crate::lisp_match_arg!($($e2)*) ),*
        }
    );

    // with-xxx
    (with-input-from-file ($var:ident $path:tt)
        $( ( $($e2:tt)* ) )*
    ) => ({
        use std;
        use std::io::Read;
        let $var = std::fs::File::open($crate::lisp_arg!($path)).unwrap();
        $( $crate::lisp!( $($e2)* ) );*
    });
    (with-input-from-mut-file ($var:ident $path:tt)
        $( ( $($e2:tt)* ) )*
    ) => ({
        use std;
        use std::io::Read;
        let mut $var = std::fs::File::open($crate::lisp_arg!($path)).unwrap();
        $( $crate::lisp!( $($e2)* ) );*
    });

    (with-output-to-new-file ($var:ident $path:tt)
        $( ( $($e2:tt)* ) )*
    ) => ({
        use std;
        use std::io::Write;
        let mut $var = std::fs::File::create($crate::lisp_arg!($path)).unwrap();
        $( $crate::lisp!( $($e2)* ) );*
    });
    /*
    (with-output-to-string ($var:ident)  $( ( $($e2:tt)* ) )* ) => (
        let mut $var = String::new();
        $( $crate::lisp!( $($e2)* ) );*
        $var
    );

    (with-input-from-string ($var:ident $s:tt)
        $( ( $($e2:tt)* ) )*
    ) => ({
        let mut $var:&str = $crate::lisp_arg!($s); // check type
        $( $crate::lisp!( $($e2)* ) );*
    });
    */

    //
    // for impl Read
    //
    (read $file:tt $s:ident) => ($crate::lisp_arg!($file).read(&mut $crate::lisp_arg!($s)));
    (read-to-string $file:tt $s:ident) => ($crate::lisp_arg!($file).read_to_string(&mut $crate::lisp_arg!($s)));
    (read-to-end $file:tt $s:ident) => ($crate::lisp_arg!($file).read_to_end(&mut $crate::lisp_arg!($s)));
    (read-exact $file:tt $s:ident) => ($crate::lisp_arg!($file).read_exact(&mut $crate::lisp_arg!($s)));
    (bytes $readable:tt) => ($crate::lisp_arg!($readable).bytes());
    (chars $readable:tt) => ($crate::lisp_arg!($readable).chars());
    (chain $readable:tt $next:tt) => ($crate::lisp_arg!($readable).chain($next));
    (take $readable:tt $limit:tt) => ($crate::lisp_arg!($readable).take($limit));

    //
    // for impl Write
    //
    (write $buffer:tt $e:tt) => ($crate::lisp_arg!($buffer).write($crate::lisp_arg!($e)));
    (write-all $buffer:tt $e:tt) => ($crate::lisp_arg!($buffer).write_all($crate::lisp_arg!($e)));
    (write-format $buffer:tt $fmt:tt) => ($crate::lisp_arg!($buffer).write_fmt($crate::lisp_arg!($fmt)));
    (flush $writable:tt) => ($crate::lisp_arg!($writable).flush());

    //
    // for impl Seek
    //
    (seek $buffer:tt $e:tt) => ($crate::lisp_arg!($buffer).seek($crate::lisp_arg!($e)));

    //
    // for impl etc.
    //
    (by-ref $object:tt) => ($crate::lisp_arg!($object).by_ref());

    //
    // let,do,etc
    //

    // let
    (let ( $( ($var:ident $e:tt) )* )
        $( ( $($e2:tt)* ) )*
    ) => ({
        $(let mut $var = $crate::lisp_arg!($e);)*
        $( $crate::lisp!( $($e2)* ) );*
    });

    // progn
    (progn $( ( $($e:tt)* ) )* ) => ({ $( $crate::lisp!( $($e)* ) );* });

    //
    // Loops
    //
    (break) => (break;);
    //(break : $label:item) => (break $label;);
    (continue) => (continue;);
    //(continue : $labl:item) => (continue $label;);

    // loop
    (loop $( ( $($e:tt)* ) )* ) => ( loop { $( $crate::lisp!( $($e)* ) );* });
    //(: $label:ident loop $( ( $($e:tt)* ) )* ) => ($label: loop { $( $crate::lisp!( $($e)* ) );* });

    // while
    (while $cond:tt $( ( $($e:tt)* ) )* ) => ( while $crate::lisp_arg!($cond) { $( $crate::lisp!( $($e)* ) );* });

    // while-let
    (while-let ( $pattern:pat = $($cond:tt)* ) $( ( $($e:tt)* ) )* ) => ( while  let $pattern = $crate::lisp_arg!($($cond)*) { $( $crate::lisp!( $($e)* ) );* });

    // dotimes
    (dotimes ($var:ident $count:tt) $( ( $($e:tt)* ) )* ) => (
        for $var in 0..$crate::lisp_arg!($count) {
            $( $crate::lisp!( $($e)* ) );*
        }
    );

    // do
    (do ( $( ($var:ident $init:tt $step:tt) )* )
        ($cond:tt $result:tt)
        $( ( $($e:tt)* ) )*
    ) => ({
        $(let mut $var = $crate::lisp_arg!($init);)*
        while !($crate::lisp_arg!($cond)) {
            $( $crate::lisp!( $($e)* ) );*
            $($var = $crate::lisp_arg!($step);)*
        }
        $crate::lisp_arg!($result)
    });

    // doiter
    (doiter ($var:ident $( $iter:tt )* ) $( ( $($e:tt)* ) )* ) => (
        for $var in $crate::lisp_arg!( $($iter)* ) {
            $( $crate::lisp!( $($e)* ) );*
        }
    );

    //
    // Branch
    //

    // if
    (if ( $($cond:tt)* ) $e1:tt $e2:tt) => (if $crate::lisp!($($cond)*) { $crate::lisp_arg!($e1) }else{ $crate::lisp_arg!($e2) });
    (if ( $($cond:tt)* ) $e:tt) => (if $crate::lisp!($($cond)*) { $crate::lisp_arg!($e) });
    (if $cond:tt $e1:tt $e2:tt) => (if $cond { $crate::lisp_arg!($e1) }else{ $crate::lisp_arg!($e2) });
    (if $cond:tt $e:tt) => (if $cond { $crate::lisp_arg!($e) });

    // if-let
    (if-let ( $pattern:pat = $($cond:tt)* ) $e1:tt $e2:tt) => (if let $pattern = $crate::lisp_arg!($($cond)*) { $crate::lisp_arg!($e1) }else{ $crate::lisp_arg!($e2) });
    (if-let ( $pattern:pat = $($cond:tt)* ) $e:tt) => (if let $pattern = $crate::lisp_arg!($($cond)*) { $crate::lisp_arg!($e) });

    // when unless
    (when ( $($cond:tt)* ) $e:tt) => (if $crate::lisp!($($cond)*) { $crate::lisp_arg!($e) });
    (when $cond:tt $e:tt) => (if $cond { $crate::lisp_arg!($e) });
    (unless ( $($cond:tt)* ) $e:tt) => (if ! ($crate::lisp!($($cond)*)) { $crate::lisp_arg!($e) });
    (unless $cond:tt $e:tt) => (if !($cond) { $crate::lisp_arg!($e) });

     // extern crate
    ( $(#[$m:meta])* extern-crate $sym:ident) => ($(#[$m]);* extern crate $sym;);

    // use
    (use $sym:tt $(:: $sym2:tt)* ) => (use $sym $(:: $sym2)* ;);

    // mod
    ( $(#[$m:meta])* module $sym:ident
        $( ( $($e:tt)* ))*
     ) => (
         $(#[$m]);*
         mod $sym {
             $( $crate::lisp!( $($e)* ); )*
         }
    );
    ( $(#[$m:meta])* pub module $sym:ident
        $( ( $($e:tt)* ))*
     ) => (
         $(#[$m]);*
         pub mod $sym {
             $( $crate::lisp!( $($e)* ); )*
         }
    );

    // defun & return
    ( $(#[$m:meta])* defun $sym:ident ( $( ( $name:ident $typ:ty ) )* ) $return_type:tt
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        fn $sym ( $($name : $typ),* ) -> $return_type {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // defun & void
    ( $(#[$m:meta])* defun $sym:ident ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        fn $sym( $($name : $typ),* ) {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // pub defun & return
    ( $(#[$m:meta])* pub defun $sym:ident ( $( ( $name:ident $typ:ty ) )* ) $return_type:tt
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        pub fn $sym( $($name : $typ),* ) -> $return_type {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // pub defun & void
    ( $(#[$m:meta])* pub defun $sym:ident ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        pub fn $sym( $($name : $typ),* ) {
            $( $crate::lisp!( $($e)* ) );*
        }
    );

    // defconstant
    (defconstant ($var:ident $typ:ty) ( $($e:tt)+ ) ) => (let $var:$typ = $crate::lisp!( $($e)+););
    (defconstant ($var:ident $typ:ty) $e:expr) => (let $var:$typ = $e;);
    (defconstant $var:ident ( $($e:tt)+ ) ) => (let $var = $crate::lisp!( $($e)+ ););
    (defconstant $var:ident $e:expr) => (let $var = $e;);

    // defvar
    (defvar ($var:ident $typ:ty) ( $($e:tt)+ )) => (let mut $var:$typ = $crate::lisp!( $($e)+););
    (defvar ($var:ident $typ:ty) $e:expr) => (let mut $var:$typ = $e;);

    (defvar $var:ident ( $($e: tt)+ )) => (let mut $var = $crate::lisp!( $($e)+););
    (defvar $var:ident $e:expr) => (let mut $var = $e;);
    //(defvar $var:ident $e:tt) => (let mut $var = $crate::lisp_arg!($e););

    // setf
    (setf $var:ident ( $($e: tt)+ ) ) => ($var = $crate::lisp!( $($e)+););
    (setf $var:ident $e:expr) => ($var = $e);

    // compare
    (eq $x:tt $y:tt) => ($crate::lisp_arg!($x) == $crate::lisp_arg!($y));
    (== $x:tt $y:tt) => ($crate::lisp_arg!($x) == $crate::lisp_arg!($y));
    (!= $x:tt $y:tt) => ($crate::lisp_arg!($x) != $crate::lisp_arg!($y));
    (< $x:tt $y:tt) => ($crate::lisp_arg!($x) < $crate::lisp_arg!($y));
    (> $x:tt $y:tt) => ($crate::lisp_arg!($x) > $crate::lisp_arg!($y));
    (<= $x:tt $y:tt) => ($crate::lisp_arg!($x) <= $crate::lisp_arg!($y));
    (>= $x:tt $y:tt) => ($crate::lisp_arg!($x) >= $crate::lisp_arg!($y));

    // macro util
    (print $( $e:tt )+) => ( print!( $($e),+ ) );
    (println $( $e:tt )+) => ( println!( $($e),+ ) );
    (format $( $e:tt )+) =>( format!( $($e),+ ) );
    (format-args $( $e:tt )+) => ( format_args!( $($e),+ ) );
    (assert $e1:tt $e2:tt) => ( assert!($e1, $e2); );
    (assert-eq $e1:tt $e2:tt) => ( assert_eq!($e1, $e2); );
    (debug-assert $e1:tt $e2:tt) => ( debug_assert!($e1, $e2); );
    (debug-assert-eq $e1:tt $e2:tt) => ( debug_assert_eq!($e1, $e2); );
    (panic $($arg:tt)+ ) => ( panic!( $($arg)+ ); );

    // +,-,*,/,%
    (+ $x:tt $y:tt) => ($crate::lisp_arg!($x) + $crate::lisp_arg!($y));
    (- $x:tt $y:tt) => ($crate::lisp_arg!($x) - $crate::lisp_arg!($y));
    (* $x:tt $y:tt) => ($crate::lisp_arg!($x) * $crate::lisp_arg!($y));
    (/ $x:tt $y:tt) => ($crate::lisp_arg!($x) / $crate::lisp_arg!($y));
    (% $x:tt $y:tt) => ($crate::lisp_arg!($x) % $crate::lisp_arg!($y));

    // incf,decf
    (incf $var:ident) => ($var = $var + 1);
    (decf $var:ident) => ($var = $var - 1);

    // 1+,1-
    (1+ $e:tt) => ($crate::lisp_arg!($e) + 1);
    (1- $e:tt) => ($crate::lisp_arg!($e) - 1);

    // !
    (! $e:tt) => ( ! $crate::lisp_arg!($e));

    // util methods
    (len $e:tt) => ($crate::lisp_arg!($e).len());

    // tuple
    (tuple $($e:tt)* ) => ( ($($crate::lisp_arg!($e)),*) );

    // vec
    (vec $($e:tt)* ) => ( vec![$($crate::lisp_arg!($e)),*] );

    // lambda & move
    (lambda move ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (move | $($name : $typ),* |{ $( $crate::lisp!( $($e)* ) );* });
    // lambda
    (lambda ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (| $($name : $typ),* |{ $( $crate::lisp!( $($e)* ) );* });

    // funcall
    ((lambda ( ( $($name:ident $typ:ty)* ) ) $( ( $($e:tt)* ))* ) $($e2:tt)*) => (
        (| $($name : $typ),* | { $( $crate::lisp!( $($e)* ) );* })( $($crate::lisp_arg!($e2)),* )
    );

    // ident
    ( $sym:ident $(:: $sym2:ident )+ $( $e:tt )* ) => ( $sym $(:: $sym2 )+ ( $($crate::lisp_arg!($e)),* ) );
    ( $sym:ident . $( $sym2:ident ).+ $( $e:tt )* ) => ( $sym.$( $sym2 ).+ ( $($crate::lisp_arg!($e)),* ) );
    ( $sym:ident $( $e:tt )* ) => ( $sym ( $($crate::lisp_arg!($e)),* ) );

    // execute rust
    (rust $( $st:stmt )* ) => ( $($st);* );
    // other
    ($e:expr) => ($e);
}

#[macro_export]
macro_rules! lisp_arg {
    ( ( $($e:tt)* ) ) => ( $crate::lisp!( $($e)* ) );
    ($e:expr) => ($e);
}

#[macro_export]
macro_rules! lisp_match_arg {
    ($e:expr) => ($e);
    ( $($e:tt)* ) => ($crate::lisp!( $($e)* ));
}
