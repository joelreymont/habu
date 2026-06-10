\ t-cg-diff.fs — differential corpus: compile each straight-line body to a native
\ exe that prints its full result via `.`, run it, and compare the decimal output
\ to the gforth-threaded oracle running the same body+input. FULL 64-bit compare
\ (not the low-byte exit code), over an adversarial input set. Slow (exec per
\ case); run explicitly:  gforth test/t-cg-diff.fs -e bye
require ../src/cg/walk.fs
require test/tester.fs

create DBUF 300 chars allot   variable DLEN
: D+ ( a u -- )  DBUF DLEN @ + swap dup DLEN +! move ;
: BODY+. ( a u -- a2 u2 )  0 DLEN !  D+  s"  ." D+  DBUF DLEN @ ;   \ append " ."

2variable NOUT   2variable GOUT
: NATIVE-OUT {: in -- :}                 ( ba bu )    \ compile "body .", run, capture stdout
   BODY+. in COMPILE-WORD
   s" /tmp/caf-diff" EMIT-EXE
   s" /tmp/caf-diff > /tmp/caf-diffo 2>/dev/null" system
   s" /tmp/caf-diffo" slurp-file NOUT 2! ;

2variable OSRC   variable OIN
: ORACLE-RUN ( -- )  OIN @  OSRC 2@ evaluate ;        \ push input, interpret "body ."
: ORACLE-OUT {: in -- :}                 ( ba bu )    \ gforth runs the same; capture stdout
   BODY+. OSRC 2!  in OIN !
   s" /tmp/caf-gora" w/o create-file throw {: fh :}
   ['] ORACLE-RUN fh outfile-execute  fh close-file throw
   s" /tmp/caf-gora" slurp-file GOUT 2! ;

: DIFF? {: ba bu in -- f :}              \ native output ≡ oracle output ?
   ba bu in NATIVE-OUT  ba bu in ORACLE-OUT  NOUT 2@ GOUT 2@ compare 0= ;

\ corpus × adversarial inputs (each must match the threaded oracle bit-for-bit)
2variable BODY
: ALL ( body-a body-u -- )  BODY 2!
   T{ BODY 2@        0 DIFF? -> true }T
   T{ BODY 2@        1 DIFF? -> true }T
   T{ BODY 2@       -1 DIFF? -> true }T
   T{ BODY 2@      255 DIFF? -> true }T
   T{ BODY 2@      256 DIFF? -> true }T
   T{ BODY 2@    65535 DIFF? -> true }T
   T{ BODY 2@  9999999 DIFF? -> true }T
   T{ BODY 2@ 1000000007 DIFF? -> true }T ;

s" DUP *"                        ALL    \ overflow/wrap on big inputs
s" DUP +"                        ALL
s" 1+ 1+ 2*"                     ALL
s" DUP 13 LSHIFT XOR"            ALL    \ shifted-EOR fusion, full 64-bit
s" DUP 1 RSHIFT XOR"             ALL
s" 7 * 3 +"                      ALL    \ const-operand immediates
s" DUP DUP * *"                  ALL    \ cube — sign + overflow
s" DUP DUP + +"                  ALL    \ 3x — ROT/dup paths
