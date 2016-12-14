 #[derive(Debug)]
pub struct Cons<T, U> {
    pub car: T,
    pub cdr: U 
}

#[macro_export]
macro_rules! lisp {
    // progn
    (progn $( ( $($e:tt)* ) )* ) => ( $( lisp!( $($e)* ) );* );
    // if
    (if ( $($cond:tt)* ) ( $($e1:tt)* ) ( $($e2:tt)* )) => (if lisp!($($cond)*) { lisp!($($e1)*) }else{ lisp!($($e2)*) });
    (if ( $($cond:tt)* ) ( $($e1:tt)* ) $e2:tt ) => (if lisp!($($cond)*) { lisp!($($e1)*) }else{ $e2 });
    (if ( $($cond:tt)* ) $e1:tt ( $($e2:tt)* )) => (if lisp!($($cond)*) { $e1 }else{ lisp!($($e2)*) });
    (if ( $($cond:tt)* ) $e1:tt $e2:tt) => (if lisp!($($cond)*) { $e1 }else{ $e2 });

    (if ( $($cond:tt)* ) ( $($e:tt)* )) => (if lisp!($($cond)*) { lisp!($($e)*) });
    (if ( $($cond:tt)* ) $e:tt) => (if lisp!($($cond)*) { $e });

    (if $cond:tt ( $($e1:tt)* ) ( $($e2:tt)* )) => (if $cond { lisp!($($e1)*) }else{ lisp!($($e2)*) });
    (if $cond:tt ( $($e1:tt)* ) $e2:tt) => (if $cond { lisp!($($e1)*) }else{ $e2 });
    (if $cond:tt $e1:tt ( $($e2:tt)* )) => (if $cond { $e1 }else{ lisp!($($e2)*) });
    (if $cond:tt $e1:tt $e2:tt) => (if $cond { $e1 }else{ $e2 });

    (if $cond:tt ( $($e:tt)* )) => (if $cond { lisp!($($e)*) });
    (if $cond:tt $e:tt) => (if $cond { $e });
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
    (defvar $var:ident ( $($e: tt)+ )) => (let mut $var = lisp!( $($e)+););
    (defvar $var:ident $e:expr) => (let mut $var = $e;);
    // cons, car, cdr
    (cons $car:tt $cdr:tt) => (Cons{car: $car, cdr: $cdr});
    (car $cell:tt) => ($cell.car);
    (cdr $cell:tt) => ($cell.cdr);
    // setf
    (setf (car $e1:tt) ( $($e: tt)+ ) ) => ($e1.car = lisp!( $($e)+));
    (setf (cdr $e1:tt) ( $($e: tt)+ ) ) => ($e1.cdr = lisp!( $($e)+));
    (setf (car $e1:tt) $e2:expr) => ($e1.car = $e2);
    (setf (cdr $e1:tt) $e2:expr) => ($e1.cdr = $e2);
    (setf $var:ident ( $($e: tt)+ ) ) => ($var = lisp!( $($e)+););
    (setf $var:ident $e:expr) => ($var = $e);
    // eq
    (eq ( $($e1: tt)+ ) ( $($e2: tt)+ ) ) => (lisp!( $($e1)+) == lisp!( $($e2)+));
    (eq $e1:tt ( $($e: tt)+ ) ) => ($e1 == lisp!( $($e)+) );
    (eq $e1:tt $e2:tt) => ($e1 == $e2);
    // compare
    (== ( $($e1: tt)+ ) ( $($e2: tt)+ ) ) => (lisp!( $($e1)+) == lisp!( $($e2)+));
    (== $e1:tt ( $($e: tt)+ ) ) => ($e1 == lisp!( $($e)+) );
    (== ( $($e1: tt)+ ) $e2:tt) => (lisp!( $($e1)+ ) == $e2);
    (== $e1:tt $e2:tt) => ($e1 == $e2);

    (!= ( $($e1: tt)+ ) ( $($e2: tt)+ ) ) => (lisp!( $($e1)+) != lisp!( $($e2)+));
    (!= $e1:tt ( $($e: tt)+ ) ) => ($e1 != lisp!( $($e)+) );
    (!= ( $($e1: tt)+ ) $e2:tt) => (lisp!( $($e1)+ ) != $e2);
    (!= $e1:tt $e2:tt) => ($e1 != $e2);

    (< ( $($e1: tt)+ ) ( $($e2: tt)+ ) ) => (lisp!( $($e1)+) < lisp!( $($e2)+));
    (< $e1:tt ( $($e: tt)+ ) ) => ($e1 < lisp!( $($e)+) );
    (< ( $($e1: tt)+ ) $e2:tt) => (lisp!( $($e1)+ ) < $e2);
    (< $e1:tt $e2:tt) => ($e1 < $e2);

    (> ( $($e1: tt)+ ) ( $($e2: tt)+ ) ) => (lisp!( $($e1)+) > lisp!( $($e2)+));
    (> $e1:tt ( $($e: tt)+ ) ) => ($e1 > lisp!( $($e)+) );
    (> ( $($e1: tt)+ ) $e2:tt) => (lisp!( $($e1)+ ) > $e2);
    (> $e1:tt $e2:tt) => ($e1 > $e2);

    (<= ( $($e1: tt)+ ) ( $($e2: tt)+ ) ) => (lisp!( $($e1)+) <= lisp!( $($e2)+));
    (<= $e1:tt ( $($e: tt)+ ) ) => ($e1 <= lisp!( $($e)+) );
    (<= ( $($e1: tt)+ ) $e2:tt) => (lisp!( $($e1)+ ) <= $e2);
    (<= $e1:tt $e2:tt) => ($e1 <= $e2);    

    (>= ( $($e1: tt)+ ) ( $($e2: tt)+ ) ) => (lisp!( $($e1)+) >= lisp!( $($e2)+));
    (>= $e1:tt ( $($e: tt)+ ) ) => ($e1 >= lisp!( $($e)+) );
    (>= ( $($e1: tt)+ ) $e2:tt) => (lisp!( $($e1)+ ) >= $e2);
    (>= $e1:tt $e2:tt) => ($e1 >= $e2);
    // macro util
    (print $( $e:tt )+) => ( print!( $($e),+ ) );
    (println $( $e:tt )+) => ( println!( $($e),+ ) );
    (assert $e1:tt $e2:tt) => ( assert!($e1, $e2); );
    (assert_eq $e1:tt $e2:tt) => ( assert_eq!($e1, $e2); );
    (debug_assert $e1:tt $e2:tt) => ( debug_assert!($e1, $e2); );
    (debug_assert_eq $e1:tt $e2:tt) => ( debug_assert_eq!($e1, $e2); );
    // +,-,*,/,%
    (+ ( $($e1: tt)+ ) ( $($e2: tt)+ ) ) => (lisp!( $($e1)+) + lisp!( $($e2)+));
    (+ $e1:tt ( $($e: tt)+ ) ) => ($e1 + lisp!( $($e)+) );
    (+ ( $($e1: tt)+ ) $e2:tt) => (lisp!( $($e1)+ ) + $e2);
    (+ $x:tt $y:tt) => ($x + $y);

    (- ( $($e1: tt)+ ) ( $($e2: tt)+ ) ) => (lisp!( $($e1)+) - lisp!( $($e2)+));
    (- $e1:tt ( $($e: tt)+ ) ) => ($e1 - lisp!( $($e)+) );
    (- ( $($e1: tt)+ ) $e2:tt) => (lisp!( $($e1)+ ) - $e2);
    (- $x:tt $y:tt) => ($x - $y);

    (* ( $($e1: tt)+ ) ( $($e2: tt)+ ) ) => (lisp!( $($e1)+) * lisp!( $($e2)+));
    (* $e1:tt ( $($e: tt)+ ) ) => ($e1 * lisp!( $($e)+) );
    (* ( $($e1: tt)+ ) $e2:tt) => (lisp!( $($e1)+ ) * $e2);
    (* $x:tt $y:tt) => ($x * $y); 

    (/ ( $($e1: tt)+ ) ( $($e2: tt)+ ) ) => (lisp!( $($e1)+) / lisp!( $($e2)+));
    (/ $e1:tt ( $($e: tt)+ ) ) => ($e1 / lisp!( $($e)+) );
    (/ ( $($e1: tt)+ ) $e2:tt) => (lisp!( $($e1)+ ) / $e2);
    (/ $x:tt $y:tt) => ($x / $y); 

    (% ( $($e1: tt)+ ) ( $($e2: tt)+ ) ) => (lisp!( $($e1)+) % lisp!( $($e2)+));
    (% $e1:tt ( $($e: tt)+ ) ) => ($e1 % lisp!( $($e)+) );
    (% ( $($e1: tt)+ ) $e2:tt) => (lisp!( $($e1)+ ) % $e2);
    (% $x:tt $y:tt) => ($x % $y);

    // funcall
    //( ( $($e:tt)* ) ) => ( lisp!( $($e)* ) );
    ( ( $sym:tt $($e:tt)* ) ) => ( lisp!( $sym $($e)* ) );
    ($sym:ident ( $e1:tt )) => ( $sym(args!($e1)) );
    //($sym:ident $e1:expr) => ( $sym($e1) );
    ($sym:ident $e1:tt $e2:tt) => ( $sym($e1, $e2) );
    ($sym:ident $( $e:tt )* ) => ( $sym ( $(args!($e)),* ) );

    // execute rust expr
    (rust $( $e:tt )* ) => ( $($e);* );
    // other
    ($e:expr) => ($e);
}

#[macro_export]
macro_rules! args {
    ( ( $e:tt ) ) => (lisp!($e));
    ( ( $($e:tt)* ) ) => ( lisp!( $($e)* ) );
    ($e:expr) => ($e);
}