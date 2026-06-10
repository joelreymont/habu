\ t-cg-forth.fs — a STANDALONE native Forth (no gforth, no C). caf emits a Mach-O
\ containing a dictionary + outer interpreter; it parses an embedded source line,
\ number-pushes, FINDs primitives, EXECUTEs them. Slow; run explicitly:
\   gforth test/t-cg-forth.fs -e bye
require nf.fs                                    \ NF-RUN / NF= build+run+capture harness
require test/tester.fs

: NF ( src-a src-u -- )  NF-RUN ;                \ alias used by the cases below

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

\ --- Stage 3: read program from STDIN (batch REPL), incl. multi-line input ---
s\" : DOUBLE DUP + ;\n: QUAD DOUBLE DOUBLE ;\n7 QUAD .\n2 3 + .\n"
   NF-REPL  T{ s\" 28\n5\n" NF= -> true }T
s" : SQ DUP * ; 9 SQ ."  NF-REPL  T{ s\" 81\n" NF= -> true }T
