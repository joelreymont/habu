\ t-cg-prims.fs — broadened AOT prim set: stack/arith/compare ops, nested
\ ?DO/LOOP (return-stack based), and >R/R@/R>. Compiles each to a native exe and
\ checks the result. Slow; run explicitly:  gforth test/t-cg-prims.fs -e bye
require ../caf-cg.fs
CHECKING-ON? off  require test/tester.fs  CHECKING-ON? on
CODEGEN-ON? on
decimal

: T-ROT  ( i64 -- i64 ) 1 2 ROT + + ;          \ n+3
: T-ABS  ( i64 -- i64 ) ABS ;
: T-MIN  ( i64 -- i64 ) 7 MIN ;
: T-MAX  ( i64 -- i64 ) 7 MAX ;
: T-TUCK ( i64 -- i64 ) 5 TUCK + + ;           \ n+10
: T-INV  ( i64 -- i64 ) INVERT ;
: T-2*   ( i64 -- i64 ) 2* ;
: T-2/   ( i64 -- i64 ) 2/ ;
: T-LSH  ( i64 -- i64 ) 3 LSHIFT ;
: T-MOD  ( i64 -- i64 ) 5 /MOD DROP ;
: NEST   ( i64 -- i64 ) 0 SWAP 0 ?DO  3 0 ?DO 1+ LOOP  LOOP ;   \ 3n (nested)
: USE-R  ( i64 -- i64 ) >R R@ R> + ;                            \ 2n

T{ 99 RUN-NATIVE T-ROT  -> 102 }T
T{ -8 RUN-NATIVE T-ABS  ->   8 }T
T{  3 RUN-NATIVE T-MIN  ->   3 }T
T{  9 RUN-NATIVE T-MAX  ->   9 }T
T{  5 RUN-NATIVE T-TUCK ->  15 }T
T{  0 RUN-NATIVE T-INV  -> 255 }T
T{  5 RUN-NATIVE T-2*   ->  10 }T
T{ 20 RUN-NATIVE T-2/   ->  10 }T
T{  2 RUN-NATIVE T-LSH  ->  16 }T
T{ 17 RUN-NATIVE T-MOD  ->   2 }T
T{  4 RUN-NATIVE NEST   ->  12 }T
T{ 10 RUN-NATIVE NEST   ->  30 }T
T{ 21 RUN-NATIVE USE-R  ->  42 }T
