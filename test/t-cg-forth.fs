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

\ --- execution tokens + catch/throw (self-host 4 part): ' ['] EXECUTE, and
\ catch/throw (machine-stack handler frames; resume via PC-relative ADR) ---
s" : SQ DUP * ; : D ['] SQ EXECUTE ; 5 D ."           NF  T{ s\" 25\n" NF= -> true }T
s" : SQ DUP * ; 6 ' SQ EXECUTE ."                     NF  T{ s\" 36\n" NF= -> true }T
s" : BAD 99 throw ; : T ['] BAD catch . ; T"          NF  T{ s\" 99\n" NF= -> true }T
s" : GOOD ; : T ['] GOOD catch . ; T"                 NF  T{ s\" 0\n"  NF= -> true }T
s" : BAD 7 throw ; : T ['] BAD catch 0= IF 111 ELSE 222 THEN . ; T"
   NF  T{ s\" 222\n" NF= -> true }T
\ data stack is restored to the catch point on throw (the 1 2 are discarded)
s" : F 1 2 3 throw ; : T ['] F catch . ; T"           NF  T{ s\" 3\n"  NF= -> true }T
\ nested: inner catches, outer sees normal completion (exc 0)
s" : BAD 42 throw ; : INNER ['] BAD catch ; : T ['] INNER catch . ; T"
   NF  T{ s\" 0\n"  NF= -> true }T

\ --- locals {: a b :} (self-host 4): machine-stack frame, slot 0 = first name;
\ references resolve to a load; frame torn down at ';'; coexists with catch ---
s" : SQ {: x :} x x * ; 7 SQ ."                       NF  T{ s\" 49\n"  NF= -> true }T
s" : SUB3 {: a b :} a b - ; 10 3 SUB3 ."             NF  T{ s\" 7\n"   NF= -> true }T
s" : H {: a b :} a a * b b * + ; 3 4 H ."            NF  T{ s\" 25\n"  NF= -> true }T
s" : F {: x :} x 0 < IF x NEGATE ELSE x THEN ; -5 F ."  NF  T{ s\" 5\n"   NF= -> true }T
s" : G {: n :} 0 BEGIN 1 + DUP n >= UNTIL ; 5 G ."   NF  T{ s\" 5\n"   NF= -> true }T
s" : BAD 99 throw ; : T {: x :} ['] BAD catch x + ; 5 T ."  NF  T{ s\" 104\n" NF= -> true }T

\ --- wordlists + search order (self-host 5): per-record wid; WORDLIST hands out
\ fresh ids; get/set-current pick the wordlist for new defs; search-wl filters ---
s" wordlist set-current get-current ."                  NF  T{ s\" 1\n"  NF= -> true }T
s" wordlist set-current : BAR 42 ; 0 set-current BAR ." NF  T{ s\" 42\n" NF= -> true }T
s\" : SQ DUP * ; : T S\" SQ\" 0 search-wl 0 <> . ; T"   NF  T{ s\" -1\n" NF= -> true }T
s\" : T S\" NOPE\" 0 search-wl . ; T"                   NF  T{ s\" 0\n"  NF= -> true }T
s\" : T S\" dup\" 0 search-wl 0 <> . ; T"               NF  T{ s\" -1\n" NF= -> true }T
\ a word defined into wid 1 is found there, not in wid 0
s\" wordlist set-current : W2 9 ; 0 set-current : T S\" W2\" 1 search-wl 0 <> . S\" W2\" 0 search-wl . ; T"
   NF  T{ s\" -1\n0\n" NF= -> true }T

\ --- self-host 6/7 core: the checker's type-term encoding + unification +
\ binding-chain resolution, compiled and run NATIVELY by the standalone (uses
\ constants, a CREATE'd binding array, BEGIN/WHILE/REPEAT, IF/ELSE/THEN). The
\ algorithmic heart of the checker, proven runnable on the standalone itself. ---
s" 0 constant T-CON   1 constant T-VAR   -1 constant UNBOUND   create TVT 512 allot   : TVINIT 0 BEGIN dup cells TVT + UNBOUND swap ! 1 + dup 63 > UNTIL drop ;   : TV@ cells TVT + @ ;   : TV! cells TVT + ! ;   : C-MKCON 3 lshift ;   : C-MKVAR 3 lshift T-VAR or ;   : C-TAG 7 and ;   : C-PAY 3 rshift ;   : C-ISVAR 7 and T-VAR = ;   : C-UNICON C-PAY swap C-PAY = ;   : C-RESOLVE BEGIN dup C-ISVAR IF dup C-PAY TV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ;   : C-UNIFY C-RESOLVE swap C-RESOLVE swap OVER C-ISVAR IF swap C-PAY TV! -1 ELSE dup C-ISVAR IF C-PAY TV! -1 ELSE C-UNICON THEN THEN ;   TVINIT   5 C-MKCON C-PAY .   5 C-MKCON C-TAG .   7 C-MKVAR C-ISVAR .   TVINIT 1 C-MKVAR 0 TV! 5 C-MKCON 1 TV! 0 C-MKVAR C-RESOLVE C-PAY .   TVINIT 0 C-MKVAR 9 C-MKCON C-UNIFY .   0 C-MKVAR C-RESOLVE C-PAY .   3 C-MKCON 4 C-MKCON C-UNIFY .   3 C-MKCON 3 C-MKCON C-UNIFY ."
   NF  T{ s\" 5\n0\n-1\n5\n-1\n9\n0\n-1\n" NF= -> true }T

\ --- self-host 7 core-risk: the occurs-check through recursive PTR types
\ (PLAN's "core risk: occurs/resolve mutual recursion"), iterative via a
\ worklist, compiled and run NATIVELY by the standalone. ---
s" 0 constant T-CON  1 constant T-VAR  2 constant T-PTR  -1 constant UNBOUND  create TVT 512 allot  : TVINIT 0 BEGIN dup cells TVT + UNBOUND swap ! 1 + dup 63 > UNTIL drop ;  : TV@ cells TVT + @ ;  : C-MKCON 3 lshift ;  : C-MKVAR 3 lshift T-VAR or ;  : C-PAY 3 rshift ;  : C-ISVAR 7 and T-VAR = ;  : C-ISPTR 7 and T-PTR = ;  : C-RESOLVE BEGIN dup C-ISVAR IF dup C-PAY TV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ;  create ARENA 512 allot  variable ASP  : MK-PTR ASP @ cells ARENA + ! ASP @ 3 lshift T-PTR or ASP @ 1 + ASP ! ;  : PTR>INNER C-PAY cells ARENA + @ ;  create WL 256 allot  variable WLSP  variable FOUND  : WL-PUSH WLSP @ cells WL + ! WLSP @ 1 + WLSP ! ;  : WL-POP WLSP @ 1 - WLSP ! WLSP @ cells WL + @ ;  : OCCURS 0 WLSP ! WL-PUSH {: id :} 0 FOUND ! BEGIN WLSP @ WHILE WL-POP C-RESOLVE dup C-ISVAR IF C-PAY id = IF -1 FOUND ! THEN ELSE dup C-ISPTR IF PTR>INNER WL-PUSH ELSE drop THEN THEN REPEAT FOUND @ ;  TVINIT 0 ASP !  0 0 C-MKVAR OCCURS .  0 1 C-MKVAR OCCURS .  0 0 C-MKVAR MK-PTR OCCURS .  0 0 C-MKVAR MK-PTR MK-PTR OCCURS .  0 1 C-MKVAR MK-PTR OCCURS .  0 5 C-MKCON MK-PTR OCCURS ."
   NF  T{ s\" -1\n0\n-1\n-1\n0\n0\n" NF= -> true }T

\ --- self-host 7: the FULL unification engine — type + row unification with
\ structural recursion (push/rest), type-var and row-var binding, worklist
\ algorithm — compiled and run NATIVELY by the standalone. The central
\ operation of the typed checker, proven runnable on the self-hosted Forth. ---
s" 0 constant T-CON 1 constant T-VAR 2 constant T-PTR 3 constant S-ROW 4 constant S-PUSH -1 constant UNBOUND create TVT 512 allot create RVT 512 allot : TVINIT 0 BEGIN dup cells TVT + UNBOUND swap ! dup cells RVT + UNBOUND swap ! 1 + dup 63 > UNTIL drop ; : TAG 7 and ; : PAY 3 rshift ; : MK-CON 3 lshift ; : MK-VAR 3 lshift T-VAR or ; : MK-ROW 3 lshift S-ROW or ; : TV@ cells TVT + @ ; : TV! cells TVT + ! ; : RV@ cells RVT + @ ; : RV! cells RVT + ! ; create SPA 1024 allot variable SPN : MK-PUSH SPN @ 2 * cells SPA + {: a :} a 8 + ! a ! SPN @ 3 lshift S-PUSH or SPN @ 1 + SPN ! ; : P>TYPE PAY 2 * cells SPA + @ ; : P>REST PAY 2 * cells SPA + 8 + @ ; : ISVAR TAG T-VAR = ; : ISROW TAG S-ROW = ; : T-RES BEGIN dup ISVAR IF dup PAY TV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ; : R-RES BEGIN dup ISROW IF dup PAY RV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ; create UWL 512 allot variable USP variable UOK : U-PUSH USP @ cells UWL + ! USP @ 1 + USP ! ; : U-POP USP @ 1 - USP ! USP @ cells UWL + @ ; : PAIR swap U-PUSH U-PUSH ; : UNPAIR U-POP U-POP swap ; : U-ROW R-RES swap R-RES swap 2dup = IF 2drop ELSE over ISROW IF swap PAY RV! ELSE dup ISROW IF PAY RV! ELSE 2dup P>TYPE swap P>TYPE swap PAIR P>REST swap P>REST swap PAIR THEN THEN THEN ; : U-TYPE T-RES swap T-RES swap 2dup = IF 2drop ELSE over ISVAR IF swap PAY TV! ELSE dup ISVAR IF PAY TV! ELSE over PAY over PAY = IF 2drop ELSE 2drop 0 UOK ! THEN THEN THEN THEN ; : UNIFY 0 USP ! -1 UOK ! PAIR BEGIN USP @ UOK @ and WHILE UNPAIR over TAG dup S-ROW = swap S-PUSH = or IF U-ROW ELSE U-TYPE THEN REPEAT UOK @ ; TVINIT 0 SPN ! 0 USP ! 5 MK-CON 0 MK-ROW MK-PUSH 1 MK-VAR 2 MK-ROW MK-PUSH UNIFY . 1 MK-VAR T-RES PAY . 0 MK-ROW R-RES 2 MK-ROW R-RES = . 3 MK-CON 4 MK-CON UNIFY ."
   NF  T{ s\" -1
5
-1
0
" NF= -> true }T

\ --- self-host 7 capstone: TYPE-CHECK a word body natively. Compose the DUP
\ and * effects (APPLY = unify din + thread dout) over a polymorphic stack;
\ the checker infers the input must be i64 (code 1) and the result is i64 —
\ the full checker pipeline (build effects -> unify+compose -> infer) native. ---
s" 0 constant T-CON 1 constant T-VAR 2 constant T-PTR 3 constant S-ROW 4 constant S-PUSH -1 constant UNBOUND create TVT 512 allot create RVT 512 allot : TVINIT 0 BEGIN dup cells TVT + UNBOUND swap ! dup cells RVT + UNBOUND swap ! 1 + dup 63 > UNTIL drop ; : TAG 7 and ; : PAY 3 rshift ; : MK-CON 3 lshift ; : MK-VAR 3 lshift T-VAR or ; : MK-ROW 3 lshift S-ROW or ; : TV@ cells TVT + @ ; : TV! cells TVT + ! ; : RV@ cells RVT + @ ; : RV! cells RVT + ! ; create SPA 1024 allot variable SPN : MK-PUSH SPN @ 2 * cells SPA + {: a :} a 8 + ! a ! SPN @ 3 lshift S-PUSH or SPN @ 1 + SPN ! ; : P>TYPE PAY 2 * cells SPA + @ ; : P>REST PAY 2 * cells SPA + 8 + @ ; : ISVAR TAG T-VAR = ; : ISROW TAG S-ROW = ; : T-RES BEGIN dup ISVAR IF dup PAY TV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ; : R-RES BEGIN dup ISROW IF dup PAY RV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ; create UWL 512 allot variable USP variable UOK : U-PUSH USP @ cells UWL + ! USP @ 1 + USP ! ; : U-POP USP @ 1 - USP ! USP @ cells UWL + @ ; : PAIR swap U-PUSH U-PUSH ; : UNPAIR U-POP U-POP swap ; : U-ROW R-RES swap R-RES swap 2dup = IF 2drop ELSE over ISROW IF swap PAY RV! ELSE dup ISROW IF PAY RV! ELSE 2dup P>TYPE swap P>TYPE swap PAIR P>REST swap P>REST swap PAIR THEN THEN THEN ; : U-TYPE T-RES swap T-RES swap 2dup = IF 2drop ELSE over ISVAR IF swap PAY TV! ELSE dup ISVAR IF PAY TV! ELSE over PAY over PAY = IF 2drop ELSE 2drop 0 UOK ! THEN THEN THEN THEN ; : UNIFY 0 USP ! -1 UOK ! PAIR BEGIN USP @ UOK @ and WHILE UNPAIR over TAG dup S-ROW = swap S-PUSH = or IF U-ROW ELSE U-TYPE THEN REPEAT UOK @ ; : APPLY {: din dout :} din UNIFY drop dout ; TVINIT 0 SPN ! 0 USP ! 0 MK-ROW 10 MK-VAR 11 MK-ROW MK-PUSH 10 MK-VAR 10 MK-VAR 11 MK-ROW MK-PUSH MK-PUSH APPLY 1 MK-CON 1 MK-CON 20 MK-ROW MK-PUSH MK-PUSH 1 MK-CON 20 MK-ROW MK-PUSH APPLY P>TYPE T-RES PAY . 10 MK-VAR T-RES PAY ."
   NF  T{ s\" 1
1
" NF= -> true }T

\ --- self-host 6/7: a native TYPE-CHECKER over source bodies. The standalone
\ tokenizes a body string, looks up each word's effect (fresh-instantiated),
\ composes via row-polymorphic unification, and DETECTS type errors. Verified:
\ "dup +" ok (i64), "dup 0= +" REJECTED (+ cannot take a bool), "dup dup + +" ok.
s" 0 constant T-CON 1 constant T-VAR 2 constant T-PTR 3 constant S-ROW 4 constant S-PUSH -1 constant UNBOUND create TVT 512 allot create RVT 512 allot : TVINIT 0 BEGIN dup cells TVT + UNBOUND swap ! dup cells RVT + UNBOUND swap ! 1 + dup 63 > UNTIL drop ; : TAG 7 and ; : PAY 3 rshift ; : MK-CON 3 lshift ; : MK-VAR 3 lshift T-VAR or ; : MK-ROW 3 lshift S-ROW or ; : TV@ cells TVT + @ ; : TV! cells TVT + ! ; : RV@ cells RVT + @ ; : RV! cells RVT + ! ; create SPA 1024 allot variable SPN : MK-PUSH SPN @ 2 * cells SPA + {: a :} a 8 + ! a ! SPN @ 3 lshift S-PUSH or SPN @ 1 + SPN ! ; : P>TYPE PAY 2 * cells SPA + @ ; : P>REST PAY 2 * cells SPA + 8 + @ ; : ISVAR TAG T-VAR = ; : ISROW TAG S-ROW = ; : T-RES BEGIN dup ISVAR IF dup PAY TV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ; : R-RES BEGIN dup ISROW IF dup PAY RV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ; create UWL 512 allot variable USP variable UOK : U-PUSH USP @ cells UWL + ! USP @ 1 + USP ! ; : U-POP USP @ 1 - USP ! USP @ cells UWL + @ ; : PAIR swap U-PUSH U-PUSH ; : UNPAIR U-POP U-POP swap ; : U-ROW R-RES swap R-RES swap 2dup = IF 2drop ELSE over ISROW IF swap PAY RV! ELSE dup ISROW IF PAY RV! ELSE 2dup P>TYPE swap P>TYPE swap PAIR P>REST swap P>REST swap PAIR THEN THEN THEN ; : U-TYPE T-RES swap T-RES swap 2dup = IF 2drop ELSE over ISVAR IF swap PAY TV! ELSE dup ISVAR IF PAY TV! ELSE over PAY over PAY = IF 2drop ELSE 2drop 0 UOK ! THEN THEN THEN THEN ; : UNIFY 0 USP ! -1 UOK ! PAIR BEGIN USP @ UOK @ and WHILE UNPAIR over TAG dup S-ROW = swap S-PUSH = or IF U-ROW ELSE U-TYPE THEN REPEAT UOK @ ; variable FV : FRESH FV @ dup 1 + FV ! ; variable OK variable DCUR : NEW -1 OK ! 0 SPN ! 0 USP ! TVINIT 0 FV ! FRESH MK-ROW DCUR ! ; : STEP {: din dout :} DCUR @ din UNIFY OK @ and OK ! dout DCUR ! ; : DUP-E FRESH FRESH {: a s :} a MK-VAR s MK-ROW MK-PUSH a MK-VAR a MK-VAR s MK-ROW MK-PUSH MK-PUSH STEP ; : ADD-E FRESH {: s :} 1 MK-CON 1 MK-CON s MK-ROW MK-PUSH MK-PUSH 1 MK-CON s MK-ROW MK-PUSH STEP ; : ZEQ-E FRESH {: s :} 1 MK-CON s MK-ROW MK-PUSH 2 MK-CON s MK-ROW MK-PUSH STEP ; variable SEQ : STR= {: a u b v :} u v = IF -1 SEQ ! 0 BEGIN dup u < WHILE dup a + c@ over b + c@ <> IF 0 SEQ ! THEN 1 + REPEAT drop ELSE 0 SEQ ! THEN SEQ @ ; : DO-TOK {: a u :} a u s" dup" STR= IF DUP-E ELSE a u s" +" STR= IF ADD-E ELSE a u s" *" STR= IF ADD-E ELSE a u s" 0=" STR= IF ZEQ-E THEN THEN THEN THEN ; variable TBASE variable TBLEN variable TI variable TSTART : CHECK {: a u :} a TBASE ! u TBLEN ! NEW 0 TI ! BEGIN TI @ TBLEN @ < WHILE BEGIN TI @ TBLEN @ < TBASE @ TI @ + c@ 32 = and WHILE TI @ 1 + TI ! REPEAT TI @ TBLEN @ < IF TBASE @ TI @ + TSTART ! BEGIN TI @ TBLEN @ < TBASE @ TI @ + c@ 32 <> and WHILE TI @ 1 + TI ! REPEAT TSTART @ TBASE @ TI @ + TSTART @ - DO-TOK THEN REPEAT OK @ ; : TOPTY DCUR @ P>TYPE T-RES PAY ; : T1 s" dup +" CHECK . ; : T2 s" dup +" CHECK drop TOPTY . ; : T3 s" dup 0= +" CHECK . ; : T4 s" dup dup + +" CHECK . ; T1 T2 T3 T4"
   NF  T{ s\" -1
1
0
-1
" NF= -> true }T

\ --- self-host 6/7/9: CHECKED COMPILATION wired into the standalone. The native
\ checker is installed as the compile hook (set-check); each : definition is
\ type-checked at ; and PUBLISHED only if well-typed. The hook prints its
\ verdict: SQ (dup *) -> -1 published; BAD (dup 0= +) -> 0 REJECTED; 7 SQ -> 49.
\ The standalone now checks AND compiles natively — a self-hosting checked Forth.
s" 0 constant T-CON 1 constant T-VAR 2 constant T-PTR 3 constant S-ROW 4 constant S-PUSH -1 constant UNBOUND create TVT 512 allot create RVT 512 allot : TVINIT 0 BEGIN dup cells TVT + UNBOUND swap ! dup cells RVT + UNBOUND swap ! 1 + dup 63 > UNTIL drop ; : TAG 7 and ; : PAY 3 rshift ; : MK-CON 3 lshift ; : MK-VAR 3 lshift T-VAR or ; : MK-ROW 3 lshift S-ROW or ; : TV@ cells TVT + @ ; : TV! cells TVT + ! ; : RV@ cells RVT + @ ; : RV! cells RVT + ! ; create SPA 1024 allot variable SPN : MK-PUSH SPN @ 2 * cells SPA + {: a :} a 8 + ! a ! SPN @ 3 lshift S-PUSH or SPN @ 1 + SPN ! ; : P>TYPE PAY 2 * cells SPA + @ ; : P>REST PAY 2 * cells SPA + 8 + @ ; : ISVAR TAG T-VAR = ; : ISROW TAG S-ROW = ; : T-RES BEGIN dup ISVAR IF dup PAY TV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ; : R-RES BEGIN dup ISROW IF dup PAY RV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ; create UWL 512 allot variable USP variable UOK : U-PUSH USP @ cells UWL + ! USP @ 1 + USP ! ; : U-POP USP @ 1 - USP ! USP @ cells UWL + @ ; : PAIR swap U-PUSH U-PUSH ; : UNPAIR U-POP U-POP swap ; : U-ROW R-RES swap R-RES swap 2dup = IF 2drop ELSE over ISROW IF swap PAY RV! ELSE dup ISROW IF PAY RV! ELSE 2dup P>TYPE swap P>TYPE swap PAIR P>REST swap P>REST swap PAIR THEN THEN THEN ; : U-TYPE T-RES swap T-RES swap 2dup = IF 2drop ELSE over ISVAR IF swap PAY TV! ELSE dup ISVAR IF PAY TV! ELSE over PAY over PAY = IF 2drop ELSE 2drop 0 UOK ! THEN THEN THEN THEN ; : UNIFY 0 USP ! -1 UOK ! PAIR BEGIN USP @ UOK @ and WHILE UNPAIR over TAG dup S-ROW = swap S-PUSH = or IF U-ROW ELSE U-TYPE THEN REPEAT UOK @ ; variable FV : FRESH FV @ dup 1 + FV ! ; variable OK variable DCUR : NEW -1 OK ! 0 SPN ! 0 USP ! TVINIT 0 FV ! FRESH MK-ROW DCUR ! ; : STEP {: din dout :} DCUR @ din UNIFY OK @ and OK ! dout DCUR ! ; : DUP-E FRESH FRESH {: a s :} a MK-VAR s MK-ROW MK-PUSH a MK-VAR a MK-VAR s MK-ROW MK-PUSH MK-PUSH STEP ; : ADD-E FRESH {: s :} 1 MK-CON 1 MK-CON s MK-ROW MK-PUSH MK-PUSH 1 MK-CON s MK-ROW MK-PUSH STEP ; : ZEQ-E FRESH {: s :} 1 MK-CON s MK-ROW MK-PUSH 2 MK-CON s MK-ROW MK-PUSH STEP ; variable SEQ : STR= {: a u b v :} u v = IF -1 SEQ ! 0 BEGIN dup u < WHILE dup a + c@ over b + c@ <> IF 0 SEQ ! THEN 1 + REPEAT drop ELSE 0 SEQ ! THEN SEQ @ ; : DO-TOK {: a u :} a u s" dup" STR= IF DUP-E ELSE a u s" +" STR= IF ADD-E ELSE a u s" *" STR= IF ADD-E ELSE a u s" 0=" STR= IF ZEQ-E THEN THEN THEN THEN ; variable TBASE variable TBLEN variable TI variable TSTART : CHECK {: a u :} a TBASE ! u TBLEN ! NEW 0 TI ! BEGIN TI @ TBLEN @ < WHILE BEGIN TI @ TBLEN @ < TBASE @ TI @ + c@ 32 = and WHILE TI @ 1 + TI ! REPEAT TI @ TBLEN @ < IF TBASE @ TI @ + TSTART ! BEGIN TI @ TBLEN @ < TBASE @ TI @ + c@ 32 <> and WHILE TI @ 1 + TI ! REPEAT TSTART @ TBASE @ TI @ + TSTART @ - DO-TOK THEN REPEAT OK @ ; : TOPTY DCUR @ P>TYPE T-RES PAY ; : HOOK CHECK dup . ; ' HOOK set-check : SQ dup * ; : BAD dup 0= + ; 7 SQ ."
   NF  T{ s\" -1
0
49
" NF= -> true }T

\ --- self-host 8: the ARM64 instruction ENCODERS run natively on the standalone
\ and produce byte-identical machine words to caf's own asm.fs (cross-checked
\ against the gforth oracle): add/sub/mul/orr/movz. Pure bit-math codegen native. ---
s" : ENC-ADD {: rd rn rm :} 2332033024 rm 16 lshift or rn 5 lshift or rd or ; : ENC-SUB {: rd rn rm :} 3405774848 rm 16 lshift or rn 5 lshift or rd or ; : ENC-MUL {: rd rn rm :} 2600500224 rm 16 lshift or rn 5 lshift or rd or ; : ENC-ORR {: rd rn rm :} 2852126720 rm 16 lshift or rn 5 lshift or rd or ; : ENC-MOVZ {: rd imm :} 3531603968 imm 5 lshift or rd or ; 0 1 2 ENC-ADD . 3 4 5 ENC-SUB . 0 1 2 ENC-MUL . 7 8 9 ENC-ORR . 0 42 ENC-MOVZ . 16 1 ENC-MOVZ ."
   NF  T{ s\" 2332164128
3406102659
2600631328
2852716807
3531605312
3531604016
" NF= -> true }T

\ --- self-host 8: the full CODEGEN PIPELINE runs natively — an ICode record
\ buffer, a peephole optimizer (kills self-move MOV x5,x5), and ARM64 encoding.
\ Input [MOV x5,x5; ADD x1,x2,x3; MOV x7,x8] -> optimize -> encode yields the ADD
\ and the live MOV (self-move dropped), byte-identical to caf's asm.fs. ---
s" : ENC-ADD {: rd rn rm :} 2332033024 rm 16 lshift or rn 5 lshift or rd or ; : ENC-MOV {: rd rm :} 2852127712 rm 16 lshift or rd or ; create IC 256 cells allot variable ICN : ICREC 4 cells * IC + ; : IC4 {: op a b c :} ICN @ ICREC op over ! a over 8 + ! b over 16 + ! c swap 24 + ! ICN @ 1 + ICN ! ; : ICOP {: i :} i ICREC @ ; : ICA {: i :} i ICREC 8 + @ ; : ICB {: i :} i ICREC 16 + @ ; : ICC {: i :} i ICREC 24 + @ ; : KILL {: i :} 0 i ICREC ! ; : OPT 0 BEGIN dup ICN @ < WHILE dup ICOP 1 = IF dup dup ICA swap ICB = IF dup KILL THEN THEN 1 + REPEAT drop ; : GEN 0 BEGIN dup ICN @ < WHILE dup ICOP 1 = IF dup ICA over ICB ENC-MOV . THEN dup ICOP 2 = IF dup ICA over ICB over ICC ENC-ADD . THEN 1 + REPEAT drop ; 0 ICN ! 1 5 5 0 IC4 2 1 2 3 IC4 1 7 8 0 IC4 OPT GEN"
   NF  T{ s\" 2332229697
2852652007
" NF= -> true }T


\ --- self-host 10 (foundation): caf's Mach-O emission is byte-DETERMINISTIC —
\ the same standalone source builds to identical bytes (the fixpoint's
\ reproducibility prerequisite; only the external codesign signature differs). ---
: BLD ( -- a u )  s" : SQ DUP * ; 5 SQ ." EMIT-FORTH BUILD-MACHO  MBUF MLEN @ ;
: SAME-BUILD? ( -- f )  BLD s" /tmp/det-x" WRITE-EXE  BLD s" /tmp/det-x" slurp-file 2swap compare 0= ;
T{ SAME-BUILD? -> true }T

\ --- self-host 10: the standalone EMITS A RUNNABLE NATIVE EXECUTABLE itself —
\ builds the Mach-O (header + load commands), encodes exit(42), writes the file
\ via syscalls. After the same external ad-hoc codesign caf uses, the OS runs it
\ and it exits 42. gforth is dropped from emission — the standalone produces
\ native binaries on its own (checker + codegen + ICode + encoders + Mach-O + IO).
: STANDALONE-EMITS-EXE ( -- rc )
   s" 4294967296 constant VMBASE 65536 constant MPAGE 4096 constant CODEOFF variable MSTART : MOFF here MSTART @ - ; : M8 c, ; : M32 {: w :} w 255 and M8 w 8 rshift 255 and M8 w 16 rshift 255 and M8 w 24 rshift 255 and M8 ; : M64 {: x :} x M32 x 32 rshift M32 ; : SPAD {: a u total :} 0 BEGIN dup total < WHILE dup u < IF dup a + c@ M8 ELSE 0 M8 THEN 1 + REPEAT drop ; : MNAME 16 SPAD ; : MPAD {: target :} BEGIN MOFF target < WHILE 0 M8 REPEAT ; : P32 {: w a :} w 255 and a c! w 8 rshift 255 and a 1 + c! w 16 rshift 255 and a 2 + c! w 24 rshift 255 and a 3 + c! ; create PB 64 allot variable PL : PSET 0 PL ! BEGIN dup PL @ > WHILE over PL @ + c@ PB PL @ + c! PL @ 1 + PL ! REPEAT 2drop 0 PB PL @ + c! ; : BUILD here MSTART ! 4277009103 M32 16777228 M32 0 M32 2 M32 0 M32 0 M32 2097285 M32 0 M32 25 M32 72 M32 s\" __PAGEZERO\" MNAME 0 M64 VMBASE M64 0 M64 0 M64 0 M32 0 M32 0 M32 0 M32 25 M32 152 M32 s\" __TEXT\" MNAME VMBASE M64 MPAGE M64 0 M64 MPAGE M64 5 M32 5 M32 1 M32 0 M32 s\" __text\" MNAME s\" __TEXT\" MNAME VMBASE CODEOFF + M64 12 M64 CODEOFF M32 2 M32 0 M32 0 M32 2147484160 M32 0 M32 0 M32 0 M32 25 M32 72 M32 s\" __LINKEDIT\" MNAME VMBASE MPAGE + M64 MPAGE M64 MPAGE M64 0 M64 1 M32 1 M32 0 M32 0 M32 14 M32 32 M32 12 M32 s\" /usr/lib/dyld\" 20 SPAD 2147483688 M32 24 M32 CODEOFF M64 0 M64 12 M32 56 M32 24 M32 2 M32 88866816 M32 65536 M32 s\" /usr/lib/libSystem.B.dylib\" 32 SPAD 6 MSTART @ 16 + P32 MOFF 32 - MSTART @ 20 + P32 CODEOFF MPAD 3531605312 M32 3531604016 M32 3556773889 M32 MPAGE MPAD ; : SAVE s\" /tmp/se-out\" PSET BUILD PB 1537 493 open {: fd :} fd MSTART @ MOFF write drop fd close ; SAVE" NF-REPL
   s" codesign -f -s - /tmp/se-out 2>/dev/null && chmod +x /tmp/se-out && /tmp/se-out; echo $? > /tmp/se-rc" system
   s" /tmp/se-rc" slurp-file s>number? 2drop ;
T{ STANDALONE-EMITS-EXE -> 42 }T

\ --- self-host 8+10: the standalone GENERATES a program's code via its own native
\ encoders (movz/mul/svc to compute 6*7) and emits it as a runnable Mach-O. After
\ the external codesign caf uses, the OS runs it and it exits 42 — the standalone's
\ native codegen -> native executable, end to end, no gforth.
: CODEGEN-EXE ( -- rc )
   s" 4294967296 constant VMBASE 65536 constant MPAGE 4096 constant CODEOFF variable MSTART : MOFF here MSTART @ - ; : M8 c, ; : M32 {: w :} w 255 and M8 w 8 rshift 255 and M8 w 16 rshift 255 and M8 w 24 rshift 255 and M8 ; : M64 {: x :} x M32 x 32 rshift M32 ; : SPAD {: a u total :} 0 BEGIN dup total < WHILE dup u < IF dup a + c@ M8 ELSE 0 M8 THEN 1 + REPEAT drop ; : MNAME 16 SPAD ; : MPAD {: target :} BEGIN MOFF target < WHILE 0 M8 REPEAT ; : P32 {: w a :} w 255 and a c! w 8 rshift 255 and a 1 + c! w 16 rshift 255 and a 2 + c! w 24 rshift 255 and a 3 + c! ; : ENC-MOVZ {: rd imm :} 3531603968 imm 5 lshift or rd or ; : ENC-MUL {: rd rn rm :} 2600500224 rm 16 lshift or rn 5 lshift or rd or ; : ENC-SVC {: imm :} 3556769793 imm 5 lshift or ; create PB 64 allot variable PL : PSET 0 PL ! BEGIN dup PL @ > WHILE over PL @ + c@ PB PL @ + c! PL @ 1 + PL ! REPEAT 2drop 0 PB PL @ + c! ; : BUILD here MSTART ! 4277009103 M32 16777228 M32 0 M32 2 M32 0 M32 0 M32 2097285 M32 0 M32 25 M32 72 M32 s\" __PAGEZERO\" MNAME 0 M64 VMBASE M64 0 M64 0 M64 0 M32 0 M32 0 M32 0 M32 25 M32 152 M32 s\" __TEXT\" MNAME VMBASE M64 MPAGE M64 0 M64 MPAGE M64 5 M32 5 M32 1 M32 0 M32 s\" __text\" MNAME s\" __TEXT\" MNAME VMBASE CODEOFF + M64 20 M64 CODEOFF M32 2 M32 0 M32 0 M32 2147484160 M32 0 M32 0 M32 0 M32 25 M32 72 M32 s\" __LINKEDIT\" MNAME VMBASE MPAGE + M64 MPAGE M64 MPAGE M64 0 M64 1 M32 1 M32 0 M32 0 M32 14 M32 32 M32 12 M32 s\" /usr/lib/dyld\" 20 SPAD 2147483688 M32 24 M32 CODEOFF M64 0 M64 12 M32 56 M32 24 M32 2 M32 88866816 M32 65536 M32 s\" /usr/lib/libSystem.B.dylib\" 32 SPAD 6 MSTART @ 16 + P32 MOFF 32 - MSTART @ 20 + P32 CODEOFF MPAD 0 6 ENC-MOVZ M32 1 7 ENC-MOVZ M32 0 0 1 ENC-MUL M32 16 1 ENC-MOVZ M32 128 ENC-SVC M32 MPAGE MPAD ; : SAVE s\" /tmp/se2-out\" PSET BUILD PB 1537 493 open {: fd :} fd MSTART @ MOFF write drop fd close ; SAVE" NF-REPL
   s" codesign -f -s - /tmp/se2-out 2>/dev/null && chmod +x /tmp/se2-out && /tmp/se2-out; echo $? > /tmp/se2-rc" system
   s" /tmp/se2-rc" slurp-file s>number? 2drop ;
T{ CODEGEN-EXE -> 42 }T

\ --- multiple {: :} blocks per word (fixed-frame locals) ---
s" : T4 {: a b :} {: c d :} a b + c + d + ; 1 2 3 4 T4 ." NF  T{ s\" 10\n" NF= -> true }T
s" variable Z 5 Z !  : MK {: inner :} Z @ {: idx :} idx inner + ; 100 MK ." NF  T{ s\" 105\n" NF= -> true }T
