\ t-cg-io.fs — compiled caf programs print to stdout via the native `.` runtime
\ (itoa + write syscall). Captures the program's stdout and checks it. Slow
\ (exec per case); run explicitly:  gforth test/t-cg-io.fs -e bye
require ../caf-cg.fs
CHECKING-ON? off  require test/tester.fs  CHECKING-ON? on
CODEGEN-ON? on
decimal

: SHOW   ( i64 -- i64 )  DUP . ;
: CUBE   ( i64 -- i64 )  DUP DUP * * ;
: SHOWC  ( i64 -- i64 )  CUBE DUP . ;

2variable PO
: GETOUT ( input "name" -- )                 \ run word, capture stdout into PO
   parse-name WORD-PFA dup 0= abort" no such word" swap BUILD-PROGRAM
   s" /tmp/caf-io" EMIT-EXE
   s" /tmp/caf-io > /tmp/caf-ioout" system
   s" /tmp/caf-ioout" slurp-file PO 2! ;
: OUT-IS ( a u -- f )  PO 2@ compare 0= ;

42 GETOUT SHOW    T{ s\" 42\n"  OUT-IS -> true }T
-5 GETOUT SHOW    T{ s\" -5\n"  OUT-IS -> true }T
 4 GETOUT SHOWC   T{ s\" 64\n"  OUT-IS -> true }T
 0 GETOUT SHOW    T{ s\" 0\n"   OUT-IS -> true }T
