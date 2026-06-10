\ t-cg-hook.fs — the checker drives the generator: define checked words, then
\ compile them (and their callees) to native Mac executables and run them.
\ Proves source → check → ARM64 → multi-word Mach-O → native. Slow (exec per
\ case); run explicitly:  gforth test/t-cg-hook.fs -e bye
require ../caf-cg.fs
CHECKING-ON? off  require test/tester.fs  CHECKING-ON? on   \ tester is infra
CODEGEN-ON? on
decimal

\ leaf words
: SQUARE  ( i64 -- i64 )  DUP * ;
: CUBE    ( i64 -- i64 )  DUP DUP * * ;
: ABSV    ( i64 -- i64 )  DUP 0< IF NEGATE THEN ;
: FACT    ( i64 -- i64 )  1 SWAP 1+ 1 ?DO I * LOOP ;
: SUMTO   ( i64 -- i64 )  0 SWAP 1+ 1 ?DO I + LOOP ;

T{  7 RUN-NATIVE SQUARE -> 49 }T
T{  5 RUN-NATIVE CUBE  -> 125 }T
T{ -9 RUN-NATIVE ABSV  ->   9 }T
T{  9 RUN-NATIVE ABSV  ->   9 }T
T{  5 RUN-NATIVE FACT  -> 120 }T
T{ 10 RUN-NATIVE SUMTO ->  55 }T

\ word calling words (subroutine ABI, BL between words)
: QUAD    ( i64 -- i64 )  SQUARE SQUARE ;        \ x^4
T{  3 RUN-NATIVE QUAD ->  81 }T                   \ 3^4

\ recursion (RECURSE → BL self)
: RFACT   ( i64 -- i64 )  DUP 2 < IF DROP 1 ELSE DUP 1- RECURSE * THEN ;
T{  5 RUN-NATIVE RFACT -> 120 }T
T{  4 RUN-NATIVE RFACT ->  24 }T
T{  0 RUN-NATIVE RFACT ->   1 }T

\ mutual: even/odd-ish via a helper
: DEC2     ( i64 -- i64 )  1- 1- ;
: QUADDEC  ( i64 -- i64 )  QUAD DEC2 ;             \ x^4 - 2
T{  3 RUN-NATIVE QUADDEC -> 79 }T
