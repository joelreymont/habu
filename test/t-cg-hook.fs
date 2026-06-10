\ t-cg-hook.fs — the checker drives the generator: define checked words, then
\ compile them to native Mac executables and run them. Proves source → check →
\ ARM64 → Mach-O → native. Slow (exec per case); run explicitly:
\   gforth test/t-cg-hook.fs -e bye
require ../caf-cg.fs
CHECKING-ON? off  require test/tester.fs  CHECKING-ON? on   \ tester is infra
CODEGEN-ON? on
decimal

: SQUARE  ( i64 -- i64 )  DUP * ;
: CUBE    ( i64 -- i64 )  DUP DUP * * ;
: ABSV    ( i64 -- i64 )  DUP 0< IF NEGATE THEN ;
: FACT    ( i64 -- i64 )  1 SWAP 1+ 1 ?DO I * LOOP ;
: SUMTO   ( i64 -- i64 )  0 SWAP 1+ 1 ?DO I + LOOP ;

\ each runs as a freshly-compiled native executable, exiting with the result
T{  7 RUN-NATIVE SQUARE -> 49 }T
T{  5 RUN-NATIVE CUBE  -> 125 }T
T{ -9 RUN-NATIVE ABSV  ->   9 }T
T{  9 RUN-NATIVE ABSV  ->   9 }T
T{  5 RUN-NATIVE FACT  -> 120 }T
T{ 10 RUN-NATIVE SUMTO ->  55 }T
