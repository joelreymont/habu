\ t-cg-forth.fs — a STANDALONE native Forth (no gforth, no C). caf emits a Mach-O
\ containing a dictionary + outer interpreter; it parses an embedded source line,
\ number-pushes, FINDs primitives, EXECUTEs them. Slow; run explicitly:
\   gforth test/t-cg-forth.fs -e bye
require ../src/cg/forth.fs
require test/tester.fs

2variable FO
: NF ( src-a src-u -- )                          \ build native Forth on src, capture stdout
   s" /tmp/t-nf" FORTH-EXE
   s" /tmp/t-nf > /tmp/t-nfout 2>/dev/null" system
   s" /tmp/t-nfout" slurp-file FO 2! ;
: NF= ( a u -- f )  FO 2@ compare 0= ;

s" 2 3 + ."       NF  T{ s\" 5\n"   NF= -> true }T
s" 10 20 + 5 * ." NF  T{ s\" 150\n" NF= -> true }T
s" 6 dup + ."     NF  T{ s\" 12\n"  NF= -> true }T
s" 100 7 - ."     NF  T{ s\" 93\n"  NF= -> true }T
s" 8 3 swap - ."  NF  T{ s\" -5\n"  NF= -> true }T
s" 1 2 3 + + ."   NF  T{ s\" 6\n"   NF= -> true }T
s" 7 6 * ."       NF  T{ s\" 42\n"  NF= -> true }T
