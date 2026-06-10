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

\ --- Stage 2: runtime `:`/`;` compiler (stencil inlining into an mmap'd region) ---
\ case-insensitive: UPPER-CASE source matches lower-case built-ins
s" : SQ DUP * ; 5 SQ ."                NF  T{ s\" 25\n"    NF= -> true }T
s" : DOUBLE DUP + ; 21 DOUBLE ."       NF  T{ s\" 42\n"    NF= -> true }T
s" : FIVE 5 ; FIVE FIVE + ."           NF  T{ s\" 10\n"    NF= -> true }T
s" 5 DUP . ."                          NF  T{ s\" 5\n5\n"  NF= -> true }T
\ two definitions in one program (W^X re-toggle: RW before second slot write)
s" : A1 DUP * ; : A2 7 ; 3 A2 ."       NF  T{ s\" 7\n"     NF= -> true }T
\ user word inlined into another (transitive stencil copy), 4 levels deep
s" : A DUP * ; : B A A ; : C B B ; 2 C ."  NF  T{ s\" 65536\n" NF= -> true }T
s" : OCT QUAD DUP + ; : QUAD DUP * ; 3 QUAD ."  NF  T{ s\" 9\n" NF= -> true }T
