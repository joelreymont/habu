\ t-cg-cli.fs — caf emits STANDALONE CLI executables (no gforth) that read an
\ integer argv[1], compute, and print. Emits each, runs it via the shell with an
\ argument, and checks stdout. Slow; run explicitly:  gforth test/t-cg-cli.fs -e bye
require ../caf-cg.fs
CHECKING-ON? off  require test/tester.fs  CHECKING-ON? on
CODEGEN-ON? on
decimal

: SQUARE ( i64 -- i64 ) DUP * ;
: CUBE   ( i64 -- i64 ) DUP DUP * * ;
: RFACT  ( i64 -- i64 ) DUP 2 < IF DROP 1 ELSE DUP 1- RECURSE * THEN ;
: SUMTO  ( i64 -- i64 ) 0 SWAP 1+ 1 ?DO I + LOOP ;
: GCD    ( i64 i64 -- i64 ) DUP 0= IF DROP ELSE SWAP OVER MOD RECURSE THEN ;

s" /tmp/t-sq"    CAF-EXE SQUARE
s" /tmp/t-cube"  CAF-EXE CUBE
s" /tmp/t-rfact" CAF-EXE RFACT
s" /tmp/t-sumto" CAF-EXE SUMTO
s" /tmp/t-gcd"   CAF-EXE GCD

2variable CLIO
: ,arg ( n -- )  s>d <# #s #> cs+ ;             \ append " n" to the command
: RUN-CLI ( n exe-a exe-u -- )                  \ run `exe n`, capture stdout
   cmd(  cs+  s"  " cs+  ,arg  s"  > /tmp/t-cliout" cs+  )run drop
   s" /tmp/t-cliout" slurp-file CLIO 2! ;
: RUN-CLI2 ( a b exe-a exe-u -- )               \ run `exe a b`, capture stdout
   cmd(  cs+  s"  " cs+  swap ,arg  s"  " cs+  ,arg  s"  > /tmp/t-cliout" cs+  )run drop
   s" /tmp/t-cliout" slurp-file CLIO 2! ;
: CLI= ( a u -- f )  CLIO 2@ compare 0= ;

 12 s" /tmp/t-sq"    RUN-CLI  T{ s\" 144\n"  CLI= -> true }T
  6 s" /tmp/t-cube"  RUN-CLI  T{ s\" 216\n"  CLI= -> true }T
  7 s" /tmp/t-rfact" RUN-CLI  T{ s\" 5040\n" CLI= -> true }T
100 s" /tmp/t-sumto" RUN-CLI  T{ s\" 5050\n" CLI= -> true }T
12 18 s" /tmp/t-gcd" RUN-CLI2 T{ s\" 6\n"    CLI= -> true }T
48 36 s" /tmp/t-gcd" RUN-CLI2 T{ s\" 12\n"   CLI= -> true }T
