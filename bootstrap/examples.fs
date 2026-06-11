\ examples.fs — checked Forth programs. Each `: NAME ( typed-effect ) … ;` is
\ type-checked at definition time, then runs as ordinary Gforth.
\   Run:  gforth examples.fs
require habu.fs

\ --- arithmetic ---
: SQUARE ( i64 -- i64 )        DUP * ;
: HYP2   ( i64 i64 -- i64 )    SQUARE SWAP SQUARE + ;        \ a²+b²
: ABSV   ( i64 -- i64 )        DUP 0 < IF NEGATE THEN ;

\ --- control flow + recursion ---
: FACT   ( i64 -- i64 )        DUP 0= IF DROP 1 ELSE DUP 1- RECURSE * THEN ;
: COUNTDOWN ( i64 -- i64 )     BEGIN 1- DUP 0= UNTIL ;

\ --- locals ---
: AVG2   ( i64 i64 -- i64 )    {: a b :} a b + 2 / ;

\ --- quotations + combinators (typed AND runnable) ---
: TWICE  ( i64 -- i64 )        [: DUP + ;] EXECUTE ;          \ 2n
: UNDER1+ ( i64 i64 -- i64 i64 ) [: 1+ ;] DIP ;              \ 1+ the lower
: SUMSQ  ( i64 -- i64 i64 )    [: DUP * ;] KEEP ;             \ n²  n
: PM1    ( i64 -- i64 i64 )    [: 1+ ;] [: 1- ;] BI ;         \ n+1  n-1
: NTH+   ( i64 i64 -- i64 )    [: 1+ ;] TIMES ;               \ add 1, n times

\ --- polymorphic (works at any type) ---
: DUP2   ( a -- a a )          DUP ;

cr ." habu examples:" cr
." 7 SQUARE      = " 7 SQUARE . cr
." 3 4 HYP2      = " 3 4 HYP2 . cr
." -9 ABSV       = " -9 ABSV . cr
." 5 FACT        = " 5 FACT . cr
." 10 20 AVG2    = " 10 20 AVG2 . cr
." 21 TWICE      = " 21 TWICE . cr
." 5 9 UNDER1+   = " 5 9 UNDER1+ . . cr
." 6 SUMSQ       = " 6 SUMSQ . . cr
." 7 PM1         = " 7 PM1 . . cr
." 4 3 NTH+       = " 4 3 NTH+ . cr
cr ." (a rejected definition prints a diagnostic and is not defined:)" cr
: BADDEF ( i64 -- i64 ) DUP ;          \ inferred ( i64 -- i64 i64 ): rejected
." BADDEF defined? " s" BADDEF" find-name 0<> . cr
bye
