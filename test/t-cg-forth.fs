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

\ --- expanded core word set (self-host milestone 1): comparisons, logic,
\ shifts, /, mod, and the deeper stack shuffles, all as native stencils ---
s" 3 3 = ."          NF  T{ s\" -1\n" NF= -> true }T
s" 3 4 = ."          NF  T{ s\" 0\n"  NF= -> true }T
s" 3 4 <> ."         NF  T{ s\" -1\n" NF= -> true }T
s" 2 5 < ."          NF  T{ s\" -1\n" NF= -> true }T
s" 5 2 > ."          NF  T{ s\" -1\n" NF= -> true }T
s" 2 2 <= ."         NF  T{ s\" -1\n" NF= -> true }T
s" 3 2 >= ."         NF  T{ s\" -1\n" NF= -> true }T
s" 0 0= ."           NF  T{ s\" -1\n" NF= -> true }T
s" 5 0= ."           NF  T{ s\" 0\n"  NF= -> true }T
s" 12 10 and ."      NF  T{ s\" 8\n"  NF= -> true }T
s" 12 3 or ."        NF  T{ s\" 15\n" NF= -> true }T
s" 12 10 xor ."      NF  T{ s\" 6\n"  NF= -> true }T
s" 0 invert ."       NF  T{ s\" -1\n" NF= -> true }T
s" 5 negate ."       NF  T{ s\" -5\n" NF= -> true }T
s" 1 4 lshift ."     NF  T{ s\" 16\n" NF= -> true }T
s" 64 2 rshift ."    NF  T{ s\" 16\n" NF= -> true }T
s" 20 3 / ."         NF  T{ s\" 6\n"  NF= -> true }T
s" 20 3 mod ."       NF  T{ s\" 2\n"  NF= -> true }T
s" 1 2 nip ."        NF  T{ s\" 2\n"  NF= -> true }T
s" 7 9 over - ."     NF  T{ s\" 2\n"  NF= -> true }T   \ 7 9 over=7; 9-7=2 (top), then . prints top
s" 1 2 tuck - + ."   NF  T{ s\" 1\n"  NF= -> true }T   \ tuck: 1 2 -> 2 1 2; 1-2=-1; 2+-1=1
s" 1 2 3 rot . . ." NF  T{ s\" 1\n3\n2\n" NF= -> true }T   \ rot: 1 2 3 -> 2 3 1; prints 1,3,2
s" 1 2 3 -rot . . ." NF T{ s\" 2\n1\n3\n" NF= -> true }T   \ -rot: 1 2 3 -> 3 1 2; prints 2,1,3
s" 5 6 2dup + . + ." NF  T{ s\" 11\n11\n" NF= -> true }T   \ 2dup: 5 6 -> 5 6 5 6; 5+6=11 .; 5+6=11 .
\ a checked-style word using the new set: a SQUARED that also compares
s" : SQ DUP * ; 4 SQ 16 = ."  NF  T{ s\" -1\n" NF= -> true }T

\ --- control flow (self-host 1b): IF/ELSE/THEN + BEGIN/UNTIL/WHILE/REPEAT, all
\ compiled inside : definitions via JIT forward/backward branch patching ---
s" : ABS DUP 0 < IF NEGATE THEN ; -5 ABS ."  NF  T{ s\" 5\n"  NF= -> true }T
s" : ABS DUP 0 < IF NEGATE THEN ; 7 ABS ."   NF  T{ s\" 7\n"  NF= -> true }T
s" : SGN DUP 0 < IF DROP -1 ELSE 0 > IF 1 ELSE 0 THEN THEN ; -3 SGN ."
   NF  T{ s\" -1\n" NF= -> true }T
s" : SGN DUP 0 < IF DROP -1 ELSE 0 > IF 1 ELSE 0 THEN THEN ; 8 SGN ."
   NF  T{ s\" 1\n"  NF= -> true }T
s" : SGN DUP 0 < IF DROP -1 ELSE 0 > IF 1 ELSE 0 THEN THEN ; 0 SGN ."
   NF  T{ s\" 0\n"  NF= -> true }T
s" : CNT 0 BEGIN 1 + DUP 5 >= UNTIL ; 0 CNT ."  NF  T{ s\" 5\n"  NF= -> true }T
s" : SUMN 0 SWAP BEGIN DUP 0 > WHILE DUP -ROT + SWAP 1 - REPEAT DROP ; 5 SUMN ."
   NF  T{ s\" 15\n" NF= -> true }T
s" : F 0 BEGIN 1 + DUP 3 < WHILE REPEAT ; 0 F ."  NF  T{ s\" 3\n"  NF= -> true }T

\ --- memory + data space (self-host 2): @ ! c@ c! cells here allot , c,
\ + CREATE/VARIABLE defining words (separate always-RW data mmap) ---
s" here 42 over ! @ ."                NF  T{ s\" 42\n" NF= -> true }T
s" 5 cells ."                         NF  T{ s\" 40\n" NF= -> true }T
s" here 100 , 200 , drop here 8 - @ ."  NF  T{ s\" 200\n" NF= -> true }T
s" here 65 over c! c@ ."              NF  T{ s\" 65\n"  NF= -> true }T
s" variable v  42 v !  v @ ."         NF  T{ s\" 42\n" NF= -> true }T
s" variable a variable b  1 a !  2 b !  a @ b @ + ."  NF  T{ s\" 3\n" NF= -> true }T
s" create arr 3 cells allot  10 arr !  20 arr 8 + !  arr @ arr 8 + @ + ."
   NF  T{ s\" 30\n" NF= -> true }T
s" variable v  5 v !  v @ 1 + v !  v @ ."  NF  T{ s\" 6\n" NF= -> true }T

\ --- string literals + TYPE (self-host 3): S" embeds bytes in the code image,
\ jumps over them, pushes (abs-addr, len); TYPE is the write(2) syscall ---
s\" : HI S\" hello\" TYPE ; HI"             NF  T{ s\" hello" NF= -> true }T
s\" : G S\" abc\" TYPE S\" de\" TYPE ; G"   NF  T{ s\" abcde" NF= -> true }T
s\" : LEN S\" hello\" NIP . ; LEN"          NF  T{ s\" 5\n"   NF= -> true }T
