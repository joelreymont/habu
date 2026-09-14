\ Execute the recovery guard ABI and its real interpreter/JIT consumers.
require nf.fs

: BES= ( got want -- )
   2dup <> if ." got " over . ." expected " dup . cr NFOUT 2@ type cr
      true abort" bootstrap engine stack mismatch"
   then 2drop ;

\ This genuine engine probe leaves the VM stacks alone. It verifies every
\ scratch GPR, LR and NZCV around each guard, using an aligned machine frame.
: BES-PRESERVE ( -- )
   LBL LBL {: bad done :}
   SP SP 32 SUBI, 21 SP 0 STR, 22 SP 8 STR, 30 SP 16 STR,
   21 $F0000000 LIT64, $D51B4215 STACK-GUARD:WORD,
   18 0 ?do i i 256 + MOVZ, loop
   0 0 STACK-GUARD:CHECK-DATA
   0 0 STACK-GUARD:CHECK-RETURN
   0 0 STACK-GUARD:CHECK-LOOP
   $D53B4215 STACK-GUARD:WORD,
   22 $F0000000 LIT64, 21 22 CMP, C-NE bad BCOND,
   18 0 ?do 21 i 256 + MOVZ, i 21 CMP, C-NE bad BCOND, loop
   21 SP 16 LDR, 30 21 CMP, C-NE bad BCOND,
   21 SP 0 LDR, 22 SP 8 LDR, SP SP 32 ADDI, done B,
   bad LBL, 0 103 MOVZ, NR-EXIT-GROUP SYS,
   done LBL, ;

: BES-IMAGE ( src-a src-u -- )
   SRCN ! SRCA ! EMIT-RESET-BUILDER EMIT-LABELS
   EMIT-MAIN EMIT-PRIMITIVE-SECTIONS EMIT-DICTIONARY-SECTIONS
   EMIT-RUNTIME-SECTIONS
   s" bes-preserve" ['] BES-PRESERVE FPRIM
   EMIT-DICT EMIT-SOURCE-BYTES NF-BIN$ EMIT-EXE ;

\ Combine stdout and stderr so refusals prove both the code and named message.
: BES-RUN ( src-a src-u -- rc )
   BES-IMAGE
   0 NF-CMD-U ! NF-BIN$ NF-ARG,
   s"  > " NF-CMD, NF-OUT$ NF-ARG, s"  2>&1" NF-CMD,
   NF-CMD NF-CMD-U @ system $? WSTAT>RC
   NF-OUT$ slurp-file NFOUT 2! ;

: BES-OK ( src-a src-u -- ) BES-RUN 0 BES= ;
: BES-REFUSED ( src-a src-u -- )
   BES-RUN ENGINE-ERROR:STACK-BOUNDS BES=
   s" hb: stack bounds exceeded" NF= 0= abort" bootstrap stack refusal lost its name" ;

: BES-NUMBER ( n -- ) 0 <# #s #> NF-CMD, s\" \n" NF-CMD, ;

: BES-ABI ( -- )
   s" STACK-ABI:BASE-CELL . STACK-ABI:CAP-CELL . STACK-ABI:REPL-BASE-CELL . STACK-ABI:REPL-CAP-CELL . STACK-ABI:BOOT-BYTES . STACK-ABI:RETURN-OFF . STACK-ABI:RETURN-END . STACK-ABI:LOOP-OFF . STACK-ABI:LOOP-END . STACK-ABI:LOOP-FRAME-BYTES . STACK-ABI:CATCH-BASE . STACK-ABI:CATCH-CAP . STACK-ABI:CATCH-BYTES . STACK-ABI:CATCH-MAGIC . STACK-ABI:EVAL-BASE . STACK-ABI:EVAL-CAP . STACK-ABI:EVAL-BYTES ." BES-OK
   0 NF-CMD-U !
   STACK-ABI:BASE-CELL BES-NUMBER STACK-ABI:CAP-CELL BES-NUMBER
   STACK-ABI:REPL-BASE-CELL BES-NUMBER STACK-ABI:REPL-CAP-CELL BES-NUMBER
   STACK-ABI:BOOT-BYTES BES-NUMBER
   STACK-ABI:RETURN-OFF BES-NUMBER STACK-ABI:RETURN-END BES-NUMBER
   STACK-ABI:LOOP-OFF BES-NUMBER STACK-ABI:LOOP-END BES-NUMBER
   STACK-ABI:LOOP-FRAME-BYTES BES-NUMBER
   STACK-ABI:CATCH-BASE BES-NUMBER STACK-ABI:CATCH-CAP BES-NUMBER
   STACK-ABI:CATCH-BYTES BES-NUMBER STACK-ABI:CATCH-MAGIC BES-NUMBER
   STACK-ABI:EVAL-BASE BES-NUMBER STACK-ABI:EVAL-CAP BES-NUMBER
   STACK-ABI:EVAL-BYTES BES-NUMBER
   NF-CMD NF-CMD-U @ NF= 0= abort" recovery stack ABI differs from its source leaf"
   s" PRIM: bes-preserve PRIM; bes-preserve 42 ." BES-OK
   s\" 42\n" NF= 0= abort" recovery guard changed caller state" ;

: BES-DATA ( -- )
   s" drop" BES-REFUSED
   s" create BUF 32 allot : EMPTY ( -- ) ; : GO ( -- ) ['] EMPTY BUF 0 run-in-stack ; GO" BES-OK
   s" create BUF 32 allot : ONE ( -- ) 42 . ; : GO ( -- ) ['] ONE BUF 8 run-in-stack ; GO" BES-OK
   s\" 42\n" NF= 0= abort" recovery exact cell changed"
   s" create BUF 32 allot : TWO ( -- ) 11 22 2drop ; : GO ( -- ) ['] TWO BUF 8 run-in-stack ; GO" BES-REFUSED
   s" create BUF 32 allot : EMPTY ( -- ) ; : GO ( -- ) ['] EMPTY BUF -1 run-in-stack ; GO" BES-REFUSED
   s" create BUF 32 allot : EMPTY ( -- ) ; : GO ( -- ) ['] EMPTY BUF 1 + 8 run-in-stack ; GO" BES-REFUSED
   s" : EMPTY ( -- ) ; : GO ( -- ) ['] EMPTY NULL-PTR 0 run-in-stack ; GO" BES-REFUSED
   s\" create BUF 32 allot : TEXT ( -- ) s\" x\" 2drop ; : GO ( -- ) ['] TEXT BUF 8 run-in-stack ; GO" BES-REFUSED ;

: BES-RETURN ( -- )
   s" STACK-ABI:RETURN-CELLS 2 - data-base RSP-CELL + ! 11 22 2>r 2r> . ." BES-OK
   s\" 22\n11\n" NF= 0= abort" recovery return pair order changed"
   s" STACK-ABI:RETURN-CELLS 1 - data-base RSP-CELL + ! 11 22 2>r" BES-REFUSED
   s" 1 data-base RSP-CELL + ! 2r>" BES-REFUSED
   s" : MOVE ( -- ) 11 >r r> drop ; STACK-ABI:RETURN-CELLS data-base RSP-CELL + ! MOVE" BES-REFUSED
   s" : MOVE ( -- ) 11 >r 0 data-base RSP-CELL + ! r@ drop r> drop ; MOVE" BES-REFUSED ;

: BES-LOOP ( -- )
   s" : ONE ( -- ) 1 0 do i . loop ; STACK-ABI:LOOP-FRAMES 1 - data-base LOOPSP-CELL + ! ONE" BES-OK
   s\" 0\n" NF= 0= abort" recovery last loop frame changed"
   s" : ONE ( -- ) 1 0 do i drop loop ; STACK-ABI:LOOP-FRAMES data-base LOOPSP-CELL + ! ONE" BES-REFUSED
   s" : ONE ( -- ) 1 0 do 0 data-base LOOPSP-CELL + ! unloop exit loop ; ONE" BES-REFUSED ;

: BES-LIFECYCLE ( -- )
   s" create BUF 32 allot : RAISE ( -- ) 7 throw ; : CROSS ( -- ) ['] RAISE BUF 8 run-in-stack ; ' CROSS catch . 1 2 3 4 . . . ." BES-OK
   s\" 7\n4\n3\n2\n1\n" NF= 0= abort" recovery catch lost the caller allocation"
   s\" create BUF 2048 allot : INNER ( -- ) s\" 17 .\" INCLUDE-EVALUATE data-base STACK-ABI:CAP-CELL + @ . ; : GO ( -- ) ['] INNER BUF 2048 run-in-stack ; GO data-base STACK-ABI:CAP-CELL + @ ." BES-OK
   s\" 17\n2048\n16384\n" NF= 0= abort" recovery evaluate lost its active allocation"
   s\" create BUF 32 allot : RAISE ( -- ) 7 throw ; : CROSS ( -- ) ['] RAISE BUF 8 run-in-stack ; : INNER ( -- ) s\" CROSS\" INCLUDE-EVALUATE ; ' INNER catch . 1 2 3 4 . . . ." BES-OK
   s\" 7\n4\n3\n2\n1\n" NF= 0= abort" recovery evaluate unwind lost the caller allocation"
   \ Evaluation consumes a two-cell token span. The reverse nesting must
   \ restore the boot allocation before the four-cell post-throw control.
   s\" create BUF 16 allot : RAISE ( -- ) 7 throw ; : INNER ( -- ) s\" RAISE\" INCLUDE-EVALUATE ; : CROSS ( -- ) ['] INNER BUF 8 run-in-stack ; ' CROSS catch ." BES-REFUSED
   s\" create BUF 16 allot : RAISE ( -- ) 7 throw ; : INNER ( -- ) s\" RAISE\" INCLUDE-EVALUATE ; : CROSS ( -- ) ['] INNER BUF 16 run-in-stack ; ' CROSS catch . 1 2 3 4 . . . ." BES-OK
   s\" 7\n4\n3\n2\n1\n" NF= 0= abort" recovery nested evaluate unwind lost the caller allocation" ;

: BES-CAT ( a u b v -- c w ) {: a u b v :}
   u v + allocate throw {: c :}
   a c u move b c u + v move c u v + ;

: BES-DEBUG-RUN ( tail size -- ) {: tail size :}
   \ The static seed has no realpath loader; bake the unchanged debug module.
   \ Its two ordinary REPL interfaces are enough for these batch breakpoints.
   s" src/habu/debug.f" slurp-file {: module bytes :}
   s\" package BES-BP : DATAB ( -- ptr a ) data-base ; : EMITS ( ptr u8 n -- ) type ;\n"
   module bytes BES-CAT {: head headsize :}
   head headsize tail size BES-CAT {: source sourcesize :}
   module free throw head free throw
   source sourcesize BES-OK source free throw ;

: BES-DEBUGGER ( -- )
   s" create BUF 32 allot : EMPTY ( -- ) ; ' EMPTY BP+ : GO ( -- ) ['] EMPTY BUF 0 run-in-stack ; GO ;package" BES-DEBUG-RUN
   NFOUT 2@ nip 26 BES=
   s" create BUF 32 allot : KEEP ( n -- n ) ; ' KEEP BP+ : ONE ( -- ) 17 KEEP drop ; : GO ( -- ) ['] ONE BUF 8 run-in-stack ; GO ;package" BES-DEBUG-RUN
   NFOUT 2@ nip 43 BES=
   NFOUT 2@ 26 /string s\" 0000000000000011\n" compare 0<>
      abort" recovery breakpoint lost its actual top cell" ;

BES-ABI BES-DATA BES-RETURN BES-LOOP BES-LIFECYCLE BES-DEBUGGER
.( bootstrap-engine-stack: ok ) cr
bye
