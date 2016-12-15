 #[derive(Debug)]
pub struct Cons<T, U> {
    pub car: T,
    pub cdr: U 
}

#[macro_export]
macro_rules! lisp {
    // while
    (while $cond:tt $( ( $($e:tt)* ) )* ) => ( while lisp_arg!($cond) { $( lisp!( $($e)* ) );* });
    // dotimes
    (dotimes ($var:ident $count:tt) $( ( $($e:tt)* ) )* ) => (
        for $var in 0..lisp_arg!($count) {
            $( lisp!( $($e)* ) );*
        }
    );
    // progn
    (progn $( ( $($e:tt)* ) )* ) => ( $( lisp!( $($e)* ) );* );
    // if
    (if ( $($cond:tt)* ) $e1:tt $e2:tt) => (if lisp!($($cond)*) { lisp_arg!($e1) }else{ lisp_arg!($e2) });
    (if ( $($cond:tt)* ) $e:tt) => (if lisp!($($cond)*) { lisp_arg!($e) });
    (if $cond:tt $e1:tt $e2:tt) => (if $cond { lisp_arg!($e1) }else{ lisp_arg!($e2) });
    (if $cond:tt $e:tt) => (if $cond { lisp_arg!($e) });

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
    (defconstant ($var:ident $typ:ty) ( $($e: tt)+ ) ) => (let $var:$typ = lisp!( $($e)+););
    (defconstant ($var:ident $typ:ty) $e:expr) => (let $var:$typ = $e;);
    (defconstant $var:ident ( $($e: tt)+ ) ) => (let $var = lisp!( $($e)+ ););
    (defconstant $var:ident $e:expr) => (let $var = $e;);

    // defvar
    (defvar ($var:ident $typ:ty) ( $($e: tt)+ )) => (let mut $var:$typ = lisp!( $($e)+););
    (defvar ($var:ident $typ:ty) $e:expr) => (let mut $var:$typ = $e;);
    //(defvar ($var:ident $typ:ty) $e:expr) => (let mut $var:$typ = $e;);

    (defvar $var:ident ( $($e: tt)+ )) => (let mut $var = lisp!( $($e)+););
    (defvar $var:ident $e:expr) => (let mut $var = $e;);
    //(defvar $var:ident $e:tt) => (let mut $var = lisp_arg!($e););

    // cons, car, cdr
    (cons $car:tt $cdr:tt) => ($crate::Cons{car: $car, cdr: $cdr});
    (car $cell:tt) => ($cell.car);
    (cdr $cell:tt) => ($cell.cdr);

    // setf
    (setf (car $e1:tt) ( $($e: tt)+ ) ) => ($e1.car = lisp!( $($e)+));
    (setf (cdr $e1:tt) ( $($e: tt)+ ) ) => ($e1.cdr = lisp!( $($e)+));
    (setf (car $e1:tt) $e2:expr) => ($e1.car = $e2);
    (setf (cdr $e1:tt) $e2:expr) => ($e1.cdr = $e2);
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
    (assert $e1:tt $e2:tt) => ( assert!($e1, $e2); );
    (assert_eq $e1:tt $e2:tt) => ( assert_eq!($e1, $e2); );
    (debug_assert $e1:tt $e2:tt) => ( debug_assert!($e1, $e2); );
    (debug_assert_eq $e1:tt $e2:tt) => ( debug_assert_eq!($e1, $e2); );

    // +,-,*,/,%
    (+ $x:tt $y:tt) => (lisp_arg!($x) + lisp_arg!($y)); 
    (- $x:tt $y:tt) => (lisp_arg!($x) - lisp_arg!($y)); 
    (* $x:tt $y:tt) => (lisp_arg!($x) * lisp_arg!($y)); 
    (/ $x:tt $y:tt) => (lisp_arg!($x) / lisp_arg!($y)); 
    (% $x:tt $y:tt) => (lisp_arg!($x) % lisp_arg!($y));

    // incf,decf
    (incf (car $e1:tt)) => ($e1.car = $e1.car + 1);
    (incf (cdr $e1:tt)) => ($e1.cdr = $e1.cdr + 1);
    (incf $var:ident) => ($var = $var + 1);
    (decf (car $e1:tt)) => ($e1.car = $e1.car - 1);
    (decf (cdr $e1:tt)) => ($e1.cdr = $e1.cdr - 1);
    (cecf $var:ident) => ($var = $var - 1);

    // funcall
    ( ( $($e:tt)* ) ) => ( lisp!( $($e)* ) );
    (  $sym:ident :: $( $sym2:ident )::+ $( $e:tt )* ) => ( $sym::$( $sym2 )::+ ( $(lisp_arg!($e)),* ) );
    ($sym:ident $( $e:tt )* ) => ( $sym ( $(lisp_arg!($e)),* ) );

    // execute rust expr
    (rust $( $e:tt )* ) => ( $($e);* );
    // other
    ($e:expr) => ($e);
}

#[macro_export]
macro_rules! lisp_arg {
    ( ( $e:tt ) ) => (lisp!($e));
    ( ( $($e:tt)* ) ) => ( lisp!( $($e)* ) );
    ($e:expr) => ($e);
}
