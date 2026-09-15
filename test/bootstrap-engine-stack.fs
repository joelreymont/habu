\ Execute the recovery guard ABI and its real interpreter/JIT consumers.
\
\ Every VM stack is now a guarded mapping (dot habu-replace-per-transfer-
\ 8523fb98): a push/pop past the capacity or below the base faults into an
\ inaccessible page and the crash handler names it, instead of a per-transfer
\ compare failing a named check. STACK-GUARD:CHECK-DATA/CHECK-RETURN/
\ CHECK-LOOP -- the per-transfer guard readers this file used to probe
\ directly -- are gone; BES-PRESERVE, which called them to prove a guard
\ check preserves caller state, went with them.
require nf.fs

: BES= ( got want -- )
   2dup <> if ." got " over . ." expected " dup . cr NFOUT 2@ type cr
      true abort" bootstrap engine stack mismatch"
   then 2drop ;

\ A fresh, independent guarded VM-stack mapping for the built test program to
\ hand to run-in-stack: run-in-stack now refuses anything that is not one
\ (GUARDED-EXTENT?), and this stage0 seed carries no lib/memory.f
\ MEM-ALLOC-GUARDED, so the fixture makes its own with the same emitter the
\ engine itself uses. Leaves the mapped base on the data stack.
: BES-MKSTACK ( -- )
   STACK-ABI:PAGE-BYTES 10 STACK-GUARD:EMIT-MAP
   10 G-PUSH ;

\ bes-mkstack is registered as a raw dictionary primitive (FPRIM), not
\ certified to the checker, so a checked `:` body that calls it would die
\ "non-certified definition". Disabling the check hook for the built test
\ program is simpler than declaring a PRIM: axiom for a test-only word: these
\ fixtures exercise run-in-stack/catch/evaluate/the guard-page fault, not
\ checked-compile certification, and every baked source is a single, fresh,
\ standalone program (not the checker's own suite).
create BES-SRC-BUF 4096 allot

: BES-IMAGE ( src-a src-u -- ) {: sa su :}
   s" 0 set-check " {: pa pu :}
   pa BES-SRC-BUF pu move
   sa BES-SRC-BUF pu + su move
   BES-SRC-BUF pu su +
   SRCN ! SRCA ! EMIT-RESET-BUILDER EMIT-LABELS
   EMIT-MAIN EMIT-PRIMITIVE-SECTIONS EMIT-DICTIONARY-SECTIONS
   EMIT-RUNTIME-SECTIONS
   s" bes-mkstack" ['] BES-MKSTACK FPRIM
   EMIT-DICT EMIT-SOURCE-BYTES NF-BIN$ EMIT-EXE ;

\ Combine stdout and stderr so refusals prove both the code and named message.
: BES-RUN ( src-a src-u -- rc )
   BES-IMAGE
   0 NF-CMD-U ! NF-BIN$ NF-ARG,
   s"  > " NF-CMD, NF-OUT$ NF-ARG, s"  2>&1" NF-CMD,
   NF-CMD NF-CMD-U @ system $? WSTAT>RC
   NF-OUT$ slurp-file NFOUT 2! ;

: BES-OK ( src-a src-u -- ) BES-RUN 0 BES= ;

\ A guard-page fault (data/return/loop stack bounds exceeded) is a fail-closed
\ process exit, named on fd 2 by src/habu/crash.f / crash.fs
\ C-CRASH-STACK-GUARDS: the classifier now runs before the register-header
\ write (so it never prints), and each message is one CRS-DATA$/CRS-RET$/
\ CRS-LOOP$ string with its own newline emitted by a single BYTES,, so the
\ length C-CRASH-GUARD-REPORT writes is exactly that string's length -- no
\ separate length constant to drift from it, no header, no trailing bytes.
\ Callers check NFOUT for the exact one-line message with NF=.
: BES-REFUSED ( src-a src-u -- ) BES-RUN ENGINE-ERROR:STACK-BOUNDS BES= ;

: BES-NUMBER ( n -- )
   dup 0< if s\" -" NF-CMD, negate then
   0 <# #s #> NF-CMD, s\" \n" NF-CMD, ;

: BES-ABI ( -- )
   s" STACK-ABI:BASE-CELL . STACK-ABI:CAP-CELL . STACK-ABI:REPL-BASE-CELL . STACK-ABI:REPL-CAP-CELL . STACK-ABI:PAGE-BYTES . STACK-ABI:BOOT-BYTES . STACK-ABI:RETURN-BASE-CELL . STACK-ABI:LOOP-BASE-CELL . STACK-ABI:RETURN-BYTES . STACK-ABI:RETURN-CELLS . STACK-ABI:LOOP-BYTES . STACK-ABI:LOOP-FRAME-BYTES . STACK-ABI:LOOP-FRAMES . STACK-ABI:E-UNGUARDED . STACK-ABI:CATCH-BASE . STACK-ABI:CATCH-CAP . STACK-ABI:CATCH-BYTES . STACK-ABI:CATCH-MAGIC . STACK-ABI:EVAL-BASE . STACK-ABI:EVAL-CAP . STACK-ABI:EVAL-BYTES ." BES-OK
   0 NF-CMD-U !
   STACK-ABI:BASE-CELL BES-NUMBER STACK-ABI:CAP-CELL BES-NUMBER
   STACK-ABI:REPL-BASE-CELL BES-NUMBER STACK-ABI:REPL-CAP-CELL BES-NUMBER
   STACK-ABI:PAGE-BYTES BES-NUMBER STACK-ABI:BOOT-BYTES BES-NUMBER
   STACK-ABI:RETURN-BASE-CELL BES-NUMBER STACK-ABI:LOOP-BASE-CELL BES-NUMBER
   STACK-ABI:RETURN-BYTES BES-NUMBER STACK-ABI:RETURN-CELLS BES-NUMBER
   STACK-ABI:LOOP-BYTES BES-NUMBER STACK-ABI:LOOP-FRAME-BYTES BES-NUMBER
   STACK-ABI:LOOP-FRAMES BES-NUMBER STACK-ABI:E-UNGUARDED BES-NUMBER
   STACK-ABI:CATCH-BASE BES-NUMBER STACK-ABI:CATCH-CAP BES-NUMBER
   STACK-ABI:CATCH-BYTES BES-NUMBER STACK-ABI:CATCH-MAGIC BES-NUMBER
   STACK-ABI:EVAL-BASE BES-NUMBER STACK-ABI:EVAL-CAP BES-NUMBER
   STACK-ABI:EVAL-BYTES BES-NUMBER
   NF-CMD NF-CMD-U @ NF= 0= abort" recovery stack ABI differs from its source leaf" ;

: BES-DATA ( -- )
   \ Data-stack underflow now faults into the guard page below the boot
   \ mapping; the crash handler names it and exits STACK-BOUNDS (102). `drop`
   \ alone will not show it -- BDROP is a bare pointer decrement (habu1.f/
   \ forth.fs: "XDS XDS 8 SUBI,"), so it never dereferences memory and cannot
   \ fault; `dup` reads the slot it duplicates (G-POP), so it does.
   s" dup" BES-REFUSED
   s\" hb: stack bounds exceeded (data)\n" NF= 0=
      abort" bootstrap data-stack underflow lost its guard-page name"

   \ run-in-stack on a guarded mapping works.
   s" : ONE ( -- ) 42 . ; : GO ( -- ) ['] ONE bes-mkstack STACK-ABI:PAGE-BYTES run-in-stack ; GO" BES-OK
   s\" 42\n" NF= 0=
      abort" recovery run-in-stack on a guarded mapping lost its result"

   \ run-in-stack now refuses any extent that is not itself a STACK-GUARD
   \ mapping (GUARDED-EXTENT?), catchable as STACK-ABI:E-UNGUARDED (-3802).
   \ A plain create/allot buffer sits inside the DATA region, so it is
   \ refused regardless of size or body.
   s" create BUF 32 allot : EMPTY ( -- ) ; : GO ( -- ) ['] EMPTY BUF 8 run-in-stack ; ' GO catch . " BES-OK
   s\" -3802\n" NF= 0=
      abort" recovery run-in-stack accepted an unguarded buffer"

   \ Capacity 0 is refused even on an otherwise plausible base.
   s" create BUF 32 allot : EMPTY ( -- ) ; : GO ( -- ) ['] EMPTY BUF 0 run-in-stack ; ' GO catch . " BES-OK
   s\" -3802\n" NF= 0=
      abort" recovery run-in-stack accepted a zero-capacity extent"

   \ A NULL base is refused before anything else runs.
   s" : EMPTY ( -- ) ; : GO ( -- ) ['] EMPTY 0 0 run-in-stack ; ' GO catch . " BES-OK
   s\" -3802\n" NF= 0=
      abort" recovery run-in-stack accepted a NULL base"

   \ A base that is not PAGE-BYTES aligned is refused: a plain buffer can
   \ never satisfy the alignment proof.
   s" create BUF 32 allot : EMPTY ( -- ) ; : GO ( -- ) ['] EMPTY BUF 1 + 8 run-in-stack ; ' GO catch . " BES-OK
   s\" -3802\n" NF= 0=
      abort" recovery run-in-stack accepted a misaligned base" ;

: BES-RETURN ( -- )
   s" STACK-ABI:RETURN-CELLS 2 - data-base RSP-CELL + ! 11 22 2>r 2r> . ." BES-OK
   s\" 22\n11\n" NF= 0= abort" recovery return pair order changed"
   s" STACK-ABI:RETURN-CELLS 1 - data-base RSP-CELL + ! 11 22 2>r" BES-REFUSED
   s\" hb: stack bounds exceeded (return)\n" NF= 0=
      abort" bootstrap return-stack overflow lost its guard-page name"
   s" 1 data-base RSP-CELL + ! 2r>" BES-REFUSED
   s\" hb: stack bounds exceeded (return)\n" NF= 0=
      abort" bootstrap return-stack underflow lost its guard-page name"
   s" : MOVE ( -- ) 11 >r r> drop ; STACK-ABI:RETURN-CELLS data-base RSP-CELL + ! MOVE" BES-REFUSED
   s\" hb: stack bounds exceeded (return)\n" NF= 0=
      abort" bootstrap >r at full return-stack capacity lost its guard-page name"
   s" : MOVE ( -- ) 11 >r 0 data-base RSP-CELL + ! r@ drop r> drop ; MOVE" BES-REFUSED
   s\" hb: stack bounds exceeded (return)\n" NF= 0=
      abort" bootstrap r@ underflow lost its guard-page name" ;

: BES-LOOP ( -- )
   s" : ONE ( -- ) 1 0 do i . loop ; STACK-ABI:LOOP-FRAMES 1 - data-base LOOPSP-CELL + ! ONE" BES-OK
   s\" 0\n" NF= 0= abort" recovery last loop frame changed"
   s" : ONE ( -- ) 1 0 do i drop loop ; STACK-ABI:LOOP-FRAMES data-base LOOPSP-CELL + ! ONE" BES-REFUSED
   s\" hb: stack bounds exceeded (loop)\n" NF= 0=
      abort" bootstrap loop-frame overflow lost its guard-page name"
   \ `unloop` alone will not show an underflow either, for the same reason
   \ `drop` does not: J-UNLOOP only decrements LOOPSP-CELL (a plain DATA-region
   \ cell), it never dereferences the frame-stack mapping. `i` does -- it reads
   \ the corrupted frame's index -- so the probe reads before it unwinds.
   s" : ONE ( -- ) 1 0 do 0 data-base LOOPSP-CELL + ! i drop unloop exit loop ; ONE" BES-REFUSED
   s\" hb: stack bounds exceeded (loop)\n" NF= 0=
      abort" bootstrap loop-frame underflow lost its guard-page name" ;

: BES-LIFECYCLE ( -- )
   s" : RAISE ( -- ) 7 throw ; : CROSS ( -- ) ['] RAISE bes-mkstack STACK-ABI:PAGE-BYTES run-in-stack ; ' CROSS catch . 1 2 3 4 . . . ." BES-OK
   s\" 7\n4\n3\n2\n1\n" NF= 0= abort" recovery catch lost the caller allocation"
   s\" : INNER ( -- ) s\" 17 .\" INCLUDE-EVALUATE data-base STACK-ABI:CAP-CELL + @ . ; : GO ( -- ) ['] INNER bes-mkstack STACK-ABI:PAGE-BYTES run-in-stack ; GO data-base STACK-ABI:CAP-CELL + @ ." BES-OK
   s\" 17\n65536\n65536\n" NF= 0= abort" recovery evaluate lost its active allocation"
   s\" : RAISE ( -- ) 7 throw ; : CROSS ( -- ) ['] RAISE bes-mkstack STACK-ABI:PAGE-BYTES run-in-stack ; : INNER ( -- ) s\" CROSS\" INCLUDE-EVALUATE ; ' INNER catch . 1 2 3 4 . . . ." BES-OK
   s\" 7\n4\n3\n2\n1\n" NF= 0= abort" recovery evaluate unwind lost the caller allocation"
   \ Opposite nesting order: run-in-stack wraps the evaluate rather than the
   \ other way around. (The matching too-small-frame refusal case is gone: a
   \ guarded mapping's minimum size is a whole STACK-ABI:PAGE-BYTES now, so
   \ there is no extent too small for this evaluate frame that is not simply
   \ an unguarded buffer -- already covered by BES-DATA.)
   s\" : RAISE ( -- ) 7 throw ; : INNER ( -- ) s\" RAISE\" INCLUDE-EVALUATE ; : CROSS ( -- ) ['] INNER bes-mkstack STACK-ABI:PAGE-BYTES run-in-stack ; ' CROSS catch . 1 2 3 4 . . . ." BES-OK
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
   s" : EMPTY ( -- ) ; ' EMPTY BP+ : GO ( -- ) ['] EMPTY bes-mkstack STACK-ABI:PAGE-BYTES run-in-stack ; GO ;package" BES-DEBUG-RUN
   NFOUT 2@ nip 26 BES=
   s" : KEEP ( n -- n ) ; ' KEEP BP+ : ONE ( -- ) 17 KEEP drop ; : GO ( -- ) ['] ONE bes-mkstack STACK-ABI:PAGE-BYTES run-in-stack ; GO ;package" BES-DEBUG-RUN
   NFOUT 2@ nip 43 BES=
   NFOUT 2@ 26 /string s\" 0000000000000011\n" compare 0<>
      abort" recovery breakpoint lost its actual top cell" ;

BES-ABI BES-DATA BES-RETURN BES-LOOP BES-LIFECYCLE BES-DEBUGGER
.( bootstrap-engine-stack: ok ) cr
bye
