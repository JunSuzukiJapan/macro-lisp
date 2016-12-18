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
    (match $e:tt $( ( $pattern:pat => ( $($e2:tt)* ) ) )* ) => (
        match lisp_arg!($e) {
            $($pattern => lisp_match_arg!($($e2)*) ),*
        }
    );

    // with-xxx
    (with-input-from-file ($var:ident $path:tt)
        $( ( $($e2:tt)* ) )*
    ) => ({
        use std;
        use std::io::Read;
        let mut $var = std::fs::File::open(lisp_arg!($path)).unwrap();
        $( lisp!( $($e2)* ) );*
    });

    (with-output-to-new-file ($var:ident $path:tt)
        $( ( $($e2:tt)* ) )*
    ) => ({
        use std;
        use std::io::Write;
        let mut $var = std::fs::File::create(lisp_arg!($path)).unwrap();
        $( lisp!( $($e2)* ) );*        
    });
    /*
    (with-output-to-string ($var:ident)  $( ( $($e2:tt)* ) )* ) => (
        let mut $var = String::new();
        $( lisp!( $($e2)* ) );*        
        $var
    );

    (with-input-from-string ($var:ident $s:tt)
        $( ( $($e2:tt)* ) )*
    ) => ({
        let mut $var:&str = lisp_arg!($s); // check type
        $( lisp!( $($e2)* ) );*        
    });
    */

    //
    // for impl Read
    //
    (read $file:tt $s:ident) => (lisp_arg!($file).read(&mut lisp_arg!($s)));
    (read-to-string $file:tt $s:ident) => (lisp_arg!($file).read_to_string(&mut lisp_arg!($s)));
    (read-to-end $file:tt $s:ident) => (lisp_arg!($file).read_to_end(&mut lisp_arg!($s)));
    (read-exact $file:tt $s:ident) => (lisp_arg!($file).read_exact(&mut lisp_arg!($s)));
    (bytes $readable:tt) => (lisp_arg!($readable).bytes());
    (chars $readable:tt) => (lisp_arg!($readable).chars());
    (chain $readable:tt $next:tt) => (lisp_arg!($readable).chain($next));
    (take $readable:tt $limit:tt) => (lisp_arg!($readable).take($limit));

    //
    // for impl Write
    //
    (write $buffer:tt $e:tt) => (lisp_arg!($buffer).write(lisp_arg!($e)));
    (write-all $buffer:tt $e:tt) => (lisp_arg!($buffer).write_all(lisp_arg!($e)));
    (write-format $buffer:tt $fmt:tt) => (lisp_arg!($buffer).write_fmt(lisp_arg!($fmt)));
    (flush $writable:tt) => (lisp_arg!($writable).flush());

    //
    // for imple Seek
    //
    (seek $buffer:tt $e:tt) => (lisp_arg!($buffer).seek(lisp_arg!($e)));

    //
    // for impl etc.
    //
    (by-ref $object:tt) => (lisp_arg!($object).by_ref());

    //
    // let,do,etc
    //

    // let
    (let ( $( ($var:ident $e:tt) )* )
        $( ( $($e2:tt)* ) )*
    ) => ({
        $(let mut $var = lisp_arg!($e);)*
        $( lisp!( $($e2)* ) );*
    });

    // progn
    (progn $( ( $($e:tt)* ) )* ) => ({ $( lisp!( $($e)* ) );* });

    //
    // Loops
    //
    (break) => (break;);
    //(break : $label:item) => (break $label;);
    (continue) => (continue;);
    //(continue : $labl:item) => (continue $label;);

    // loop
    (loop $( ( $($e:tt)* ) )* ) => ( loop { $( lisp!( $($e)* ) );* });
    //(: $label:ident loop $( ( $($e:tt)* ) )* ) => ($label: loop { $( lisp!( $($e)* ) );* });

    // while
    (while $cond:tt $( ( $($e:tt)* ) )* ) => ( while lisp_arg!($cond) { $( lisp!( $($e)* ) );* });

    // while-let
    (while-let ( $pattern:pat = $($cond:tt)* ) $( ( $($e:tt)* ) )* ) => ( while  let $pattern = lisp_arg!($($cond)*) { $( lisp!( $($e)* ) );* });
    
    // dotimes
    (dotimes ($var:ident $count:tt) $( ( $($e:tt)* ) )* ) => (
        for $var in 0..lisp_arg!($count) {
            $( lisp!( $($e)* ) );*
        }
    );

    // do
    (do ( $( ($var:ident $init:tt $step:tt) )* )
        ($cond:tt $result:tt)
        $( ( $($e:tt)* ) )*
    ) => ({
        $(let mut $var = lisp_arg!($init);)*
        while !(lisp_arg!($cond)) {
            $( lisp!( $($e)* ) );*
            $($var = lisp_arg!($step);)*
        }
        lisp_arg!($result)
    });

    // doiter
    (doiter ($var:ident $iter:tt) $( ( $($e:tt)* ) )* ) => (
        for $var in lisp_arg!($iter) {
            $( lisp!( $($e)* ) );*
        }
    );

    //
    // Branch
    //

    // if
    (if ( $($cond:tt)* ) $e1:tt $e2:tt) => (if lisp!($($cond)*) { lisp_arg!($e1) }else{ lisp_arg!($e2) });
    (if ( $($cond:tt)* ) $e:tt) => (if lisp!($($cond)*) { lisp_arg!($e) });
    (if $cond:tt $e1:tt $e2:tt) => (if $cond { lisp_arg!($e1) }else{ lisp_arg!($e2) });
    (if $cond:tt $e:tt) => (if $cond { lisp_arg!($e) });

    // if-let
    (if-let ( $pattern:pat = $($cond:tt)* ) $e1:tt $e2:tt) => (if let $pattern = lisp_arg!($($cond)*) { lisp_arg!($e1) }else{ lisp_arg!($e2) });
    (if-let ( $pattern:pat = $($cond:tt)* ) $e:tt) => (if let $pattern = lisp_arg!($($cond)*) { lisp_arg!($e) });

    // when unless
    (when ( $($cond:tt)* ) $e:tt) => (if lisp!($($cond)*) { lisp_arg!($e) });
    (when $cond:tt $e:tt) => (if $cond { lisp_arg!($e) });
    (unless ( $($cond:tt)* ) $e:tt) => (if ! (lisp!($($cond)*)) { lisp_arg!($e) });
    (unless $cond:tt $e:tt) => (if !($cond) { lisp_arg!($e) });

     // extern crate
    ( $(#[$m:meta])* extern-crate $sym:ident) => ($(#[$m]);* extern crate $sym;);

    // use
    (use $sym:tt) => (use $sym;);

    // mod
    ( $(#[$m:meta])* module $sym:ident
        $( ( $($e:tt)* ))*
     ) => (
         $(#[$m]);*
         mod $sym {
             $( lisp!( $($e)* ); )*
         }
    );
    ( $(#[$m:meta])* pub module $sym:ident
        $( ( $($e:tt)* ))*
     ) => (
         $(#[$m]);*
         pub mod $sym {
             $( lisp!( $($e)* ); )*
         }
    );

    // defun
    ( $(#[$m:meta])* defun $sym:ident ( $( ( $name:ident $typ:ty ) )* ) $return_type:tt
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        fn $sym( $($name : $typ),* ) -> $return_type {
            $( lisp!( $($e)* ) );*
        }
    );
    ( $(#[$m:meta])* defun $sym:ident ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        fn $sym( $($name : $typ),* ) {
            $( lisp!( $($e)* ) );*
        }
    );
    ( $(#[$m:meta])* pub defun $sym:ident ( $( ( $name:ident $typ:ty ) )* ) $return_type:tt
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        pub fn $sym( $($name : $typ),* ) -> $return_type {
            $( lisp!( $($e)* ) );*
        }
    );
    ( $(#[$m:meta])* pub defun $sym:ident ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        pub fn $sym( $($name : $typ),* ) {
            $( lisp!( $($e)* ) );*
        }
    );

    // defconstant
    (defconstant ($var:ident $typ:ty) ( $($e:tt)+ ) ) => (let $var:$typ = lisp!( $($e)+););
    (defconstant ($var:ident $typ:ty) $e:expr) => (let $var:$typ = $e;);
    (defconstant $var:ident ( $($e:tt)+ ) ) => (let $var = lisp!( $($e)+ ););
    (defconstant $var:ident $e:expr) => (let $var = $e;);

    // defvar
    (defvar ($var:ident $typ:ty) ( $($e:tt)+ )) => (let mut $var:$typ = lisp!( $($e)+););
    (defvar ($var:ident $typ:ty) $e:expr) => (let mut $var:$typ = $e;);
    //(defvar ($var:ident $typ:ty) $e:expr) => (let mut $var:$typ = $e;);

    (defvar $var:ident ( $($e: tt)+ )) => (let mut $var = lisp!( $($e)+););
    (defvar $var:ident $e:expr) => (let mut $var = $e;);
    //(defvar $var:ident $e:tt) => (let mut $var = lisp_arg!($e););

    // setf
    (setf $var:ident ( $($e: tt)+ ) ) => ($var = lisp!( $($e)+););
    (setf $var:ident $e:expr) => ($var = $e);

    // compare
    (eq $x:tt $y:tt) => (lisp_arg!($x) == lisp_arg!($y)); 
    (== $x:tt $y:tt) => (lisp_arg!($x) == lisp_arg!($y)); 
    (!= $x:tt $y:tt) => (lisp_arg!($x) != lisp_arg!($y)); 
    (< $x:tt $y:tt) => (lisp_arg!($x) < lisp_arg!($y)); 
    (> $x:tt $y:tt) => (lisp_arg!($x) > lisp_arg!($y)); 
    (<= $x:tt $y:tt) => (lisp_arg!($x) <= lisp_arg!($y)); 
    (>= $x:tt $y:tt) => (lisp_arg!($x) >= lisp_arg!($y)); 

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
    (+ $x:tt $y:tt) => (lisp_arg!($x) + lisp_arg!($y)); 
    (- $x:tt $y:tt) => (lisp_arg!($x) - lisp_arg!($y)); 
    (* $x:tt $y:tt) => (lisp_arg!($x) * lisp_arg!($y)); 
    (/ $x:tt $y:tt) => (lisp_arg!($x) / lisp_arg!($y)); 
    (% $x:tt $y:tt) => (lisp_arg!($x) % lisp_arg!($y));

    // incf,decf
    (incf $var:ident) => ($var = $var + 1);
    (decf $var:ident) => ($var = $var - 1);

    // 1+,1-
    (1+ $e:tt) => (lisp_arg!($e) + 1);
    (1- $e:tt) => (lisp_arg!($e) - 1);

    // tuple
    (tuple $($e:tt)* ) => ( ($(lisp_arg!($e)),*) );

    // vec
    (vec $($e:tt)* ) => ( vec![$(lisp_arg!($e)),*] );

    // lambda
    (lambda move ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (move | $($name : $typ),* |{ $( lisp!( $($e)* ) );* });
    (lambda ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (| $($name : $typ),* |{ $( lisp!( $($e)* ) );* });

    // funcall
    ((lambda ( ( $($name:ident $typ:ty)* ) ) $( ( $($e:tt)* ))* ) $($e2:tt)*) => (
        (| $($name : $typ),* | { $( lisp!( $($e)* ) );* })( $(lisp_arg!($e2)),* )
    );

    (  $sym:ident :: $( $sym2:ident )::+ $( $e:tt )* ) => ( $sym::$( $sym2 )::+ ( $(lisp_arg!($e)),* ) );
    (  $sym:ident . $( $sym2:ident ).+ $( $e:tt )* ) => ( $sym.$( $sym2 ).+ ( $(lisp_arg!($e)),* ) );
    ($sym:ident $( $e:tt )* ) => ( $sym ( $(lisp_arg!($e)),* ) );

    // execute rust
    (rust $( $st:stmt )* ) => ( $($st);* );
    // other
    ($e:expr) => ($e);
}

#[macro_export]
macro_rules! lisp_arg {
    ( ( $e:tt ) ) => (lisp!($e));
    ( ( $($e:tt)* ) ) => ( lisp!( $($e)* ) );
    ($e:expr) => ($e);
}

#[macro_export]
macro_rules! lisp_match_arg {
    ($e:expr) => ($e);
    ( $($e:tt)* ) => (lisp!( $($e)* ));
}