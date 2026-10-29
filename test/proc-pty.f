\ proc-pty.f — focused native process/PTY harness. Run with:
\   bin/hb --load src/lib/errors.f src/lib/process.f test/proc-pty.f
\ src/lib/pty-harness.f owns the terminal; this file types at the child and
\ judges.

require src/lib/pty-harness.f
require src/lib/test/outcome.f
require src/lib/engine-candidate.f
require src/lib/test.f

package PROC-PTY

using PTY-HARNESS

1 constant PTY-QUIET-POLLS         \ polls in a row that bring nothing and so end a drain

variable IN-R
variable IN-W
variable OUT-R
variable OUT-W
variable ERR-R
variable ERR-W
variable PID

\ The engine bakes neither POLL-IN nor the process primitives, so both library
\ files are part of the run line; test/runtime-regression-test.f GE-PROCESS-PTY
\ drives this file, with the engine to test as `-- <path>` or in
\ HABU_UNDER_TEST.
: HB-ARG? ( -- bool )
   SCRIPT-ARGC 0 > ;

: HB-EXE$ ( -- ptr u8 n )
   HB-ARG? if 0 SCRIPT-ARGV$ exit then
   ENGINE-CANDIDATE:PATH$ ;

: BUF-N ( -- n )
   BUF$ {: a:ptr u:n :} u ;

: BUF-DUMP ( -- )
   s" buf bytes: " type BUF-N . cr
   BUF-N 0= if exit then
   s" buf:" type cr
   BUF$ type cr ;

: BUF-ON-FAIL ( n -- )
   T-FAILURES <> if BUF-DUMP then ;

: PTY-TRUE ( bool -- )
   T-FAILURES {: before:n :}
   TTRUE before BUF-ON-FAIL ;

: PTY= ( n n -- )
   T-FAILURES {: before:n :}
   T= before BUF-ON-FAIL ;

: TCONTAINS ( ptr u8 n -- )
   IN-BUF? PTY-TRUE ;

: T-EXIT= ( ptr u8 n ptr u8 n ptr u8 n outcome n -- )
   T-FAILURES {: before:n :}
   T-OUTCOME-EXITED= before BUF-ON-FAIL ;

\ A drain clears the buffer first: the bytes it swallows must not answer a later
\ wait.
: MFD-DRAIN ( -- )
   BUF-CLEAR
   MASTER-FD PTY-QUIET-POLLS DRAIN ;

: SEND-ESC ( n -- )
   27 SEND-BYTE
   91 SEND-BYTE
   SEND-BYTE ;

: STEP-LN ( ptr u8 n -- )
   SEND-LINE
   MFD-DRAIN ;

: STEP-S ( ptr u8 n -- )
   SEND
   MFD-DRAIN ;

: EXPECT ( ptr u8 n -- )
   WAIT-FOR PTY-TRUE ;

\ A redraw while echoing the input already holds a prompt; completion needs a
\ prompt after the answer (src/lib/pty-harness.f WAIT-AFTER).
: PROMPT-READY? ( ptr u8 n -- bool )
   s" habu> " AFTER? ;

: PROMPT-AFTER ( ptr u8 n -- )
   s" habu> " WAIT-AFTER PTY-TRUE ;

\ Wait for a marker and close an absence claim's window at its end. The marker
\ must be one the child prints past the point where the rejected text could
\ appear; a failed wait leaves no window, so the claim behind it is refused too.
: BARRIER ( ptr u8 n -- )
   WAIT-BARRIER PTY-TRUE ;

\ The barrier for a claim about the line before it: the child prints the hex
\ literal in decimal, so only the evaluation can satisfy the wait, never the
\ editor's echo. The REPL reads the next line only after finishing the one
\ before it, so this answer proves that line is over, diagnostics and trailer.
: PROBE-BARRIER ( -- )
   s" $BEEF ." SEND-LINE
   s" 48879" BARRIER ;

: REJECT ( ptr u8 n -- )
   WINDOW-ABSENT? PTY-TRUE ;

: CAPTURE-PIPES ( -- )
   PIPE-PAIR IN-W ! IN-R !
   PIPE-PAIR OUT-W ! OUT-R !
   PIPE-PAIR ERR-W ! ERR-R !
   IN-W @ >FD FD-CLOEXEC!
   OUT-R @ >FD FD-CLOEXEC!
   ERR-R @ >FD FD-CLOEXEC! ;

: CAPTURE-START-HB ( -- )
   HB-EXE$ >LEN IN-R @ >FD OUT-W @ >FD ERR-W @ >FD PROC-SPAWN-IO PID !
   PID @ 0 > PTY-TRUE ;

: CAPTURE-CLOSE-CHILD-ENDS ( -- )
   IN-R @ close
   OUT-W @ close
   ERR-W @ close ;

: CAPTURE-SOURCE$ ( -- ptr u8 n )
   s" 2 3 + ." ;

: CAPTURE-SEND-SOURCE ( -- )
   IN-W @ >FD CAPTURE-SOURCE$ WRITE-LINE
   IN-W @ close ;

\ The pipes are read after the reap, so a deadline here has drained nothing.
: CAPTURE-EXPECT-RC ( -- )
   PID @ >PID WAIT-BUDGET-MS WAIT-EXIT {: oc :}
   CAPTURE-SOURCE$ s" " s" " oc 0 T-EXIT= ;

: CAPTURE-EXPECT-OUT ( -- )
   BUF-CLEAR
   OUT-R @ >FD READ-TO-EOF
   s" 5" TCONTAINS
   OUT-R @ close ;

: CAPTURE-EXPECT-ERR ( -- )
   BUF-CLEAR
   ERR-R @ >FD READ-TO-EOF
   BUF-N 0 PTY=
   ERR-R @ close ;

: CAPTURE-VERIFY ( -- )
   CAPTURE-EXPECT-RC
   CAPTURE-EXPECT-OUT
   CAPTURE-EXPECT-ERR ;

: CAPTURE-HB ( -- )
   CAPTURE-PIPES
   CAPTURE-START-HB
   CAPTURE-CLOSE-CHILD-ENDS
   CAPTURE-SEND-SOURCE
   CAPTURE-VERIFY ;

: PTY-START-HB ( -- )
   HB-EXE$ SPAWN-ON-PTY
   MFD-DRAIN ;

: PTY-PROMPT ( -- )
   s"  ok" PROMPT-AFTER ;

\ Both boot modes must enumerate one dictionary: the engine seeds its baked REPL
\ words at the end of the engine prefix on every boot. test/aot-seed-surface.f
\ folds every dictionary record name into one number; SEED-BATCH runs it as a
\ piped program and PTY-SEED-SURFACE types it at the terminal and requires the
\ same number. Neither side hardcodes it (it moves with every tree edit), and a
\ mode that seeded a different set, order or point cannot produce it. The batch
\ half lives here, not in test/aot-seed-batch-suite.f, because the comparison
\ needs both modes in one process.
create SEEDNUM 64 allot   variable SEEDNUM-U   variable SEEDI

: SEEDNUM$ ( -- ptr u8 n )    SEEDNUM SEEDNUM-U @ ;
: SEED-LINE$ ( -- ptr u8 n )  S\" s\" test/aot-seed-surface.f\" required" ;

: SEED-BYTE ( -- n )
   BUF$ {: a:ptr u:n :}
   SEEDI @ u < if a SEEDI @ + c@ exit then
   0 ;

: SEED-TOKEN! ( -- )                 \ first whitespace-delimited token of the buffer
   0 SEEDNUM-U !  0 SEEDI !
   begin SEED-BYTE dup 0 <> swap 33 < and while  SEEDI @ 1 + SEEDI !  repeat
   begin SEED-BYTE 32 > SEEDNUM-U @ 64 < and while
      SEED-BYTE SEEDNUM SEEDNUM-U @ + c!
      SEEDNUM-U @ 1 + SEEDNUM-U !
      SEEDI @ 1 + SEEDI !
   repeat ;

: SEED-BATCH ( -- )
   CAPTURE-PIPES
   CAPTURE-START-HB
   CAPTURE-CLOSE-CHILD-ENDS
   IN-W @ >FD SEED-LINE$ WRITE-LINE
   IN-W @ close
   BUF-CLEAR
   OUT-R @ >FD READ-TO-EOF
   SEED-TOKEN!
   OUT-R @ close
   ERR-R @ close
   PID @ >PID WAIT-BUDGET-MS WAIT-EXIT {: oc :}
   SEED-LINE$ BUF$ s" " oc 0 T-EXIT=                 \ stderr is never read
   SEEDNUM-U @ 0 > PTY-TRUE ;

: PTY-SEED-SURFACE ( -- )
   SEED-LINE$ STEP-LN
   SEEDNUM$ EXPECT
   s"  ok" PROMPT-AFTER ;

: PTY-ARITH ( -- )
   s" 1 2 + ." STEP-LN
   s" 3" EXPECT
   s"  ok" PROMPT-AFTER ;

: PTY-UNKNOWN ( -- )
   s" frobnicate" STEP-LN
   s" E-UNDEFINED: frobnicate" EXPECT
   s" ?" PROMPT-AFTER
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-SQUARE ( -- )
   s" : SQ dup * ;" STEP-LN
   s"  ok" PROMPT-AFTER
   s" 7 SQ ." STEP-LN
   s" 49" EXPECT
   s"  ok" PROMPT-AFTER ;

\ Certified word on an empty interpret stack: named underdepth reject, then the
\ REPL recovers and the next line evaluates (LDIAGRET recovery leg).
: PTY-UNDERDEPTH ( -- )
   s" SQ" STEP-LN
   s" hb: interpret stack underdepth: SQ" PROMPT-AFTER
   PROBE-BARRIER
   s"  ok" REJECT
   s" 6 SQ ." STEP-LN
   s" 36" EXPECT
   s"  ok" PROMPT-AFTER ;

: PTY-BACKSPACE ( -- )
   s" 1 2 + .." SEND
   127 SEND-BYTE
   10 SEND-BYTE
   MFD-DRAIN
   s" 3" EXPECT
   s"  ok" PROMPT-AFTER ;

: PTY-CANCEL ( -- )
   s" garbage" SEND
   3 SEND-BYTE
   MFD-DRAIN
   s" garbage" PROMPT-AFTER
   PROBE-BARRIER
   s" garbage?" REJECT ;

: PTY-EDIT-SEED ( -- )
   s" 5 ." STEP-LN
   s" 5" EXPECT
   s"  ok" PROMPT-AFTER ;

: PTY-EDIT-LEFT3 ( -- )
   s" 13 ." SEND
   68 SEND-ESC
   68 SEND-ESC
   68 SEND-ESC ;

: PTY-EDIT-INSERT-RUN ( -- )
   48 SEND-BYTE
   10 SEND-BYTE
   MFD-DRAIN
   s" 103" EXPECT
   s"  ok" PROMPT-AFTER ;

: PTY-EDIT-HOME ( -- )
   PTY-EDIT-SEED
   PTY-EDIT-LEFT3
   PTY-EDIT-INSERT-RUN ;

: PTY-HISTORY-UP ( -- )
   65 SEND-ESC
   10 SEND-BYTE
   MFD-DRAIN
   s" 103" EXPECT
   s"  ok" PROMPT-AFTER ;

\ A line longer than the editor's 255-byte buffer is refused by name, never
\ evaluated or saved to history, and the REPL reads on. A truncating editor
\ drops the `;`: the REPL answers " ok", the word is never defined and the
\ session stays compiling. The tail is padding, not a second statement, so a
\ truncating engine still parses it: the red is the missing refusal.
$123 constant PTY-LONG-BYTES   \ 291: the length the fixture types, and the length the refusal must name
$140 constant PTY-LONG-CAP

create PTY-LONG PTY-LONG-CAP allot
variable PTY-LONG-U

: PTY-LONG-C ( n -- ) {: c :}
   PTY-LONG-U @ PTY-LONG-CAP < if
      c PTY-LONG PTY-LONG-U @ + c!
      PTY-LONG-U @ 1+ PTY-LONG-U !
   then ;

: PTY-LONG-S ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while  dup a + c@ PTY-LONG-C  1+ repeat drop ;

: PTY-LONG$ ( -- ptr u8 n )
   PTY-LONG PTY-LONG-U @ ;

: PTY-LONG-BUILD ( -- )                        \ `: PTYLONG ( -- n ) 7 <pad> ;` in PTY-LONG-BYTES
   0 PTY-LONG-U !
   s" : PTYLONG ( -- n ) 7 " PTY-LONG-S
   begin PTY-LONG-U @ PTY-LONG-BYTES 1- < while 32 PTY-LONG-C repeat
   s" ;" PTY-LONG-S ;

: PTY-LINE-CAP-REFUSED ( -- )
   PTY-LONG-BUILD
   PTY-LONG$ STEP-LN
   s" hb: repl line over 255 bytes: 291 typed" PROMPT-AFTER
   PROBE-BARRIER
   s"  ok" REJECT ;                            \ refused, so the line never ran

: PTY-LINE-CAP-UNDEFINED ( -- )
   s" PTYLONG" STEP-LN
   s" E-UNDEFINED: PTYLONG" EXPECT ;

: PTY-LINE-CAP-CONTINUES ( -- )
   s" 21 3 * ." STEP-LN
   s" 63" EXPECT
   s"  ok" PROMPT-AFTER ;

: PTY-LINE-CAP ( -- )
   PTY-LINE-CAP-REFUSED
   PTY-LINE-CAP-UNDEFINED
   PTY-LINE-CAP-CONTINUES ;

\ The product bakes package DEBUG after the REPL (src/habu/native-runtime.f),
\ so the `require` below is a no-op there and states the session's dependency.
\ `using DEBUG` holds for the rest of the session, so the BP, BPW and step lines
\ below type the commands bare.
: PTY-DEBUGGER-LOAD ( -- )
   s" require src/habu/debug.f" STEP-LN
   s"  ok" PROMPT-AFTER
   s" using DEBUG" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-BP-SOURCE ( -- )
   s" : SQB dup * ;" STEP-LN
   s"  ok" PROMPT-AFTER
   s" : IN1 1 + ;" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-BP-ARM-ONESHOT ( -- )
   s" ' SQB BP+" STEP-LN
   s"  ok" PROMPT-AFTER
   s" ' IN1 BP+" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-BP-RUN-SQ ( -- )
   s" 7 SQB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 49" EXPECT ;

: PTY-BP-RUN-IN1 ( -- )
   s" 9 IN1 ." STEP-LN
   s" habu-bp:" EXPECT
   s" 10" EXPECT ;

: PTY-BP-RUN-SQ-CLEARED ( -- )
   s" 6 SQB ." STEP-LN
   s" 36" BARRIER                     \ the answer, past where a hit would have printed
   s" habu-bp:" REJECT ;

: PTY-BP-ONESHOT ( -- )
   PTY-BP-ARM-ONESHOT
   PTY-BP-RUN-SQ
   PTY-BP-RUN-IN1
   PTY-BP-RUN-SQ-CLEARED ;

: PTY-PB-SOURCE ( -- )
   s" : PB dup + ;" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-PB-ARM ( -- )
   s" ' PB BP*" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-PB-FIRST ( -- )
   s" 5 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 10" EXPECT ;

: PTY-PB-SECOND ( -- )
   s" 6 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 12" EXPECT ;

: PTY-PB-CLEAR ( -- )
   s" ' PB BP-" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-BP-PERSISTENT ( -- )
   PTY-PB-SOURCE
   PTY-PB-ARM
   PTY-PB-FIRST
   PTY-PB-SECOND
   PTY-PB-CLEAR ;

: PTY-WATCH-VAR ( -- )
   s" variable WV" STEP-LN
   s"  ok" PROMPT-AFTER
   s" 17 WV !" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-WATCH-ADD ( -- )
   s" WV BPW+" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-WATCH-WORD ( -- )
   s" : WID dup WV @ + ;" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-WATCH-SOURCE ( -- )
   PTY-WATCH-VAR
   PTY-WATCH-ADD
   PTY-WATCH-WORD ;

: PTY-WATCH-ARM ( -- )
   s" ' WID BP+" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-WATCH-RUN ( -- )
   s" 2 WID ." STEP-LN
   s" habu-bp-stack:" EXPECT
   s" habu-bp-watch:" EXPECT
   s" 0000000000000011" EXPECT
   s" 19" EXPECT ;

: PTY-WATCH-CLEAR ( -- )
   s" WV BPW-" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-WATCHPOINT ( -- )
   PTY-WATCH-SOURCE
   PTY-WATCH-ARM
   PTY-WATCH-RUN
   PTY-WATCH-CLEAR ;

: PTY-BPN-ARM ( -- )
   s" 2 ' PB BPN" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-BPN-SKIP ( -- )
   s" 3 PB ." STEP-LN
   s" 6" BARRIER                      \ the answer, past where a hit would have printed
   s" habu-bp:" REJECT ;

: PTY-BPN-FIRE ( -- )
   s" 3 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 6" EXPECT ;

: PTY-BPN-CLEAR ( -- )
   s" ' PB BP-" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-BP-NTH ( -- )
   PTY-BPN-ARM
   PTY-BPN-SKIP
   PTY-BPN-SKIP
   PTY-BPN-FIRE
   PTY-BPN-CLEAR ;

: PTY-DEFINE-F0-F2 ( -- )
   s" : F0 0 ;" STEP-LN
   s"  ok" PROMPT-AFTER
   s" : F1 1 ;" STEP-LN
   s"  ok" PROMPT-AFTER
   s" : F2 2 ;" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-DEFINE-F3-F4 ( -- )
   s" : F3 3 ;" STEP-LN
   s"  ok" PROMPT-AFTER
   s" : F4 4 ;" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-DEFINE-F0-F4 ( -- )
   PTY-DEFINE-F0-F2
   PTY-DEFINE-F3-F4 ;

: PTY-DEFINE-F5-F6 ( -- )
   s" : F5 5 ;" STEP-LN
   s"  ok" PROMPT-AFTER
   s" : F6 6 ;" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-DEFINE-F7-F8 ( -- )
   s" : F7 7 ;" STEP-LN
   s"  ok" PROMPT-AFTER
   s" : F8 8 ;" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-DEFINE-F5-F8 ( -- )
   PTY-DEFINE-F5-F6
   PTY-DEFINE-F7-F8 ;

: PTY-BP-F0-F2 ( -- )
   s" ' F0 BP+" STEP-LN
   s"  ok" PROMPT-AFTER
   s" ' F1 BP+" STEP-LN
   s"  ok" PROMPT-AFTER
   s" ' F2 BP+" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-BP-F3-F4 ( -- )
   s" ' F3 BP+" STEP-LN
   s"  ok" PROMPT-AFTER
   s" ' F4 BP+" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-BP-F0-F4 ( -- )
   PTY-BP-F0-F2
   PTY-BP-F3-F4 ;

: PTY-BP-F5-F6 ( -- )
   s" ' F5 BP+" STEP-LN
   s"  ok" PROMPT-AFTER
   s" ' F6 BP+" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-BP-F7-F8 ( -- )
   s" ' F7 BP+" STEP-LN
   s"  ok" PROMPT-AFTER
   s" ' F8 BP+" STEP-LN
   s" table full" EXPECT ;

: PTY-BP-F5-F8 ( -- )
   PTY-BP-F5-F6
   PTY-BP-F7-F8 ;

: PTY-BP-TABLE-FULL ( -- )
   PTY-DEFINE-F0-F4
   PTY-DEFINE-F5-F8
   PTY-BP-F0-F4
   PTY-BP-F5-F8 ;

: PTY-STEP-BASELINE ( -- )
   s" 5 ." STEP-LN
   s" 5" EXPECT
   s"  ok" PROMPT-AFTER ;

: PTY-STEP-TOKENS ( -- )
   s" step 2 3 + ." STEP-LN
   s" step> 2" EXPECT
   s" step> 3" EXPECT
   s" step> +" EXPECT
   s" 5" EXPECT ;

: PTY-STEP-DEFINE ( -- )
   s" step : SD dup * ;" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-STEP-RUN ( -- )
   s" 4 SD ." STEP-LN
   s" 16" EXPECT ;

: PTY-STEP-RECOVER ( -- )
   s" 8 ." STEP-LN
   s" 8" EXPECT
   s"  ok" PROMPT-AFTER ;

: PTY-STEPPER ( -- )
   PTY-STEP-BASELINE
   PTY-STEP-TOKENS
   PTY-STEP-DEFINE
   PTY-STEP-RUN
   PTY-STEP-RECOVER ;

: PTY-THROW-LINE ( -- )
   s" 99 throw" STEP-LN
   s" ?" PROMPT-AFTER
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-THROW-AFTER ( -- )
   s" 6 ." STEP-LN
   s" 6" EXPECT
   s"  ok" PROMPT-AFTER ;

\ tty-REPL parity for the LCOMPILEDIE compile-error family: a recoverable
\ compile die at the interactive REPL recovers the session like an undefined
\ word (PTY-UNKNOWN, LDIAGRET) instead of exiting the tty. dup-def is the
\ rollback-past-a-published-record case: the first PRDUP commits on its own
\ line; the second line's failure rolls back to that line's start, so PRDUP
\ survives.
: PTY-COMPILE-DEF ( -- )
   s" : PRDUP ( -- ) ;" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-COMPILE-RECOVER ( -- )
   s" : PRDUP ( -- ) ;" STEP-LN
   s" duplicate definition: PRDUP" PROMPT-AFTER
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-COMPILE-AFTER ( -- )
   s" 7 ." STEP-LN
   s" 7" EXPECT
   s"  ok" PROMPT-AFTER ;

: PTY-COMPILE-RECOVERY ( -- )
   PTY-COMPILE-DEF
   PTY-COMPILE-RECOVER
   PTY-COMPILE-AFTER ;

\ Package-scope rollback at the tty REPL: a line that opens a package and aborts
\ mid-definition rolls the open-package scope back to the line-start scope
\ (global here). A dangling PRP would make the next `package PRQ` nest-reject.
: PTY-PKGSCOPE-FAIL ( -- )
   s" package PRP public : PRFOO ( -- ) NOPEWORD ;" STEP-LN
   s" E-UNDEFINED: NOPEWORD" PROMPT-AFTER
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-PKGSCOPE-FRESH ( -- )                        \ a dangling PRP would make this `package` nest-reject (no " ok")
   s" package PRQ ;package" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-PKGSCOPE-GLOBAL ( -- )                       \ the next define lands global and runs; a checked reference confirms checker scope is in step
   s" : PRDONE ( -- n ) 4242 ;  : PRUSE ( -- n ) PRDONE ;  PRUSE ." STEP-LN
   s" 4242" EXPECT
   s"  ok" PROMPT-AFTER ;

: PTY-PKGSCOPE-RECOVERY ( -- )
   PTY-PKGSCOPE-FAIL
   PTY-PKGSCOPE-FRESH
   PTY-PKGSCOPE-GLOBAL ;

\ A line whose create is rolled back retires that record, and the next
\ definition reuses its slot. The does> after it has no created record to
\ patch, so it is refused by name and the session reads on; the regression
\ patches the reused slot's new owner.
: PTY-LASTC-RECOVERY ( -- )
   s" create PLC-GONE 5 , PLC-NO-SUCH-WORD" STEP-LN
   s" PLC-NO-SUCH-WORD" PROMPT-AFTER
   s" : PLC-BEHAVE ( -- ) does> ( -- n ) @ ;" STEP-LN
   s"  ok" PROMPT-AFTER
   s" PLC-BEHAVE" STEP-LN
   s" hb: does> has no created word" PROMPT-AFTER
   PROBE-BARRIER
   s"  ok" REJECT ;

\ The package's using floor comes back with the scope. The failing line closes
\ PRF2, opens a using and PRF3, and aborts: recovery reopens PRF2 with its own
\ floor, so a using PRF2 opens closes again. With PRF3's floor, one above PRF2's
\ depth, that `;using` is refused as closing an outer using.
: PTY-PKGFLOOR-OPEN ( -- )
   s" package PRF1 ;package package PRF2" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-PKGFLOOR-FAIL ( -- )
   s" ;package using PRF1 package PRF3 NOPEWORD" STEP-LN
   s" E-UNDEFINED: NOPEWORD" PROMPT-AFTER
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-PKGFLOOR-CLOSE ( -- )
   s" using PRF1 ;using ;package $C0DE ." STEP-LN
   s" 49374" EXPECT
   s"  ok" PROMPT-AFTER ;

: PTY-PKGFLOOR-RECOVERY ( -- )
   PTY-PKGFLOOR-OPEN
   PTY-PKGFLOOR-FAIL
   PTY-PKGFLOOR-CLOSE ;

\ The line's used publics come back with the scope. The failing line closes
\ PRS2, opens PRS3 in the slot PRS1 held, and aborts: recovery puts PRS1 back
\ and the checker names it again, so PRSY certifies against PRS1:PRSW and runs
\ it. A slot left holding PRS3, whose PRSW takes an input, refuses PRSY.
: PTY-PKGSLOT-OPEN ( -- )
   s" package PRS1 public : PRSW ( -- n ) $FACE ; ;package package PRS3 public : PRSW ( n -- n ) ; ;package package PRS2 using PRS1" STEP-LN
   s"  ok" PROMPT-AFTER ;

: PTY-PKGSLOT-FAIL ( -- )
   s" ;package using PRS3 NOPEWORD" STEP-LN
   s" E-UNDEFINED: NOPEWORD" PROMPT-AFTER
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-PKGSLOT-CHECK ( -- )
   s" : PRSY ( -- n ) PRSW ; PRSY . ;package" STEP-LN
   s" 64206" EXPECT
   s"  ok" PROMPT-AFTER ;

: PTY-PKGSLOT-RECOVERY ( -- )
   PTY-PKGSLOT-OPEN
   PTY-PKGSLOT-FAIL
   PTY-PKGSLOT-CHECK ;

\ The barrier itself, against the live child. An absence claim over a buffer the
\ child has not answered into is granted by the harness's own silence: a drain
\ that returned on its first quiet poll leaves exactly that buffer. One leg has
\ no barrier behind it and must be refused; the other has one and must still
\ catch the text when the child really printed it.
: PTY-REJECT-UNBARRIERED ( -- )
   s" frobnicate" STEP-LN
   BUF-CLEAR
   s"  ok" WINDOW-ABSENT? 0= PTY-TRUE
   PROBE-BARRIER
   s"  ok" WINDOW-ABSENT? PTY-TRUE ;

: PTY-REJECT-CATCHES ( -- )
   s" 5 ." STEP-LN                    \ a line that does print the trailer
   PROBE-BARRIER
   s"  ok" WINDOW-ABSENT? 0= PTY-TRUE ;

: PTY-REJECT-BARRIER ( -- )
   PTY-REJECT-UNBARRIERED
   PTY-REJECT-CATCHES ;

\ Without Enter the child can echo and redraw, but cannot have evaluated the
\ line. Its decimal answer is absent from the hex input. This pins both sides
\ of the wait: the echo prompt is insufficient; the prompt after evaluation is.
: PTY-PROMPT-ORDER ( -- )
   MFD-DRAIN BUF-CLEAR
   s" $BEEF ." SEND
   s" habu> $BEEF ." EXPECT
   0 s" habu> $BEEF ." FIND-FROM {: echo:n :}
   echo 0 >= PTY-TRUE
   s" 48879" PROMPT-READY? 0= PTY-TRUE
   10 SEND-BYTE
   s" 48879" PROMPT-AFTER
   0 s" 48879" FIND-FROM {: answer:n :}
   echo answer < PTY-TRUE
   s" 48879" s" habu> " FIND-AFTER answer > PTY-TRUE ;

: PTY-THROW-RECOVERY ( -- )
   PTY-THROW-LINE
   PTY-THROW-AFTER ;

\ ^D becomes EOF only while the terminal is raw, which it is only while the
\ child sits in its line editor. A ^D that arrives while the line before it
\ still executes is taken by the restored canonical discipline as an empty-line
\ EOF, never becomes a key, and hangs the child and the harness reaping it.
\ Only REDRAW prints the prompt and only after RAW-ON, so a prompt after a
\ completed probe proves the editor holds the terminal; the probe's echoed
\ input cannot satisfy the ordered wait.
: PTY-EDITOR-READY ( -- )
   BUF-CLEAR
   PROBE-BARRIER
   s" 48879" PROMPT-AFTER ;

: PTY-STOP-HB ( -- )
   PTY-EDITOR-READY
   4 SEND-BYTE
   REAP {: oc :}
   HB-EXE$ BUF$ s" " oc 0 T-EXIT=   \ the terminal carries both streams
   CLOSE-MASTER ;

\ A terminal that hangs up mid-line ends the session. KEY1 is one 1-byte read of
\ descriptor 0; a read of 0 (the pty master closed) or -1 must leave the editor,
\ not replay the byte still in the key buffer for ever. That regression is a
\ spin, so this case bounds its own reap: PROC-WAIT-RC has no timeout. The watch
\ descriptor opens while the child is alive: macOS cannot register a process
\ that has already exited. This case runs last and owns the second child:
\ PTY-STOP-HB reaped the first and closed the master, and the harness holds one
\ pair at a time.

$1388 constant PTY-EXIT-MS         \ what a hung-up child gets to leave its editor

: PTY-WATCH-CHILD ( -- fd )
   CHILD-PID PID>N proc-watch-open {: wfd:n :}
   wfd 0 < if E-PROC-OUTPUT throw then
   wfd >FD ;

\ True once the watch says the child is gone; a broken descriptor throws instead
\ of passing for an exit.
: PTY-WATCH-EXITED? ( fd n -- bool ) {: wfd:fd ms:n :}
   wfd ms >MS POLL-IN COUNT>N {: rc:n :}
   rc 0 < if E-PROC-OUTPUT throw then
   rc 0= if false exit then
   0 >IDX PROC-PFD-REVENTS {: ev:n :}
   ev POLLERR POLLNVAL or and 0 <> if E-PROC-OUTPUT throw then
   ev POLLIN and 0 <> ;

: PTY-KILL-CHILD ( -- )
   CHILD-PID SIGKILL PROC-KILL-RAW drop
   CHILD-PID PROC-WAIT-RC MATCH result ok OF drop ENDOF err OF drop ENDOF ;MATCH ;

\ A child that left the editor is reaped for its status; one still in it at the
\ deadline is killed and reaped, so a failing run reports instead of hanging and
\ leaves no process behind.
: PTY-HANGUP-REAP ( fd -- ) {: wfd:fd :}
   wfd PTY-EXIT-MS PTY-WATCH-EXITED? {: gone:bool :}
   gone PTY-TRUE
   gone if
      CHILD-PID PROC-WAIT-RC MATCH result ok OF 0 PTY= ENDOF err OF drop 1 0 PTY= ENDOF ;MATCH
   else
      PTY-KILL-CHILD
   then
   wfd FD>N close ;

\ The half-typed line is what the defect replayed: its last key is the byte in
\ the key buffer when the master closes. Only REDRAW echoes it and only after
\ RAW-ON, so the echo is also the proof that the child holds the terminal raw.
: PTY-HANGUP-MIDLINE ( -- )
   s" 1 2 +" SEND
   s" 1 2 +" EXPECT ;

: PTY-HANGUP ( -- )
   PTY-START-HB
   PTY-PROMPT
   PTY-HANGUP-MIDLINE
   PTY-WATCH-CHILD {: wfd:fd :}
   CLOSE-MASTER
   wfd PTY-HANGUP-REAP ;

: PTY-BASIC ( -- )
   SEED-BATCH                        \ the pipe-mode fold, before this file owns a pty child
   PTY-START-HB
   PTY-PROMPT
   PTY-SEED-SURFACE                  \ first at the prompt: before any case types a definition
   PTY-PROMPT-ORDER
   PTY-ARITH
   PTY-UNKNOWN
   PTY-SQUARE
   PTY-UNDERDEPTH ;

: PTY-EDITOR ( -- )
   PTY-BACKSPACE
   PTY-CANCEL
   PTY-EDIT-HOME
   PTY-HISTORY-UP
   PTY-LINE-CAP ;

: PTY-BREAKPOINTS ( -- )
   PTY-DEBUGGER-LOAD
   PTY-BP-SOURCE
   PTY-BP-ONESHOT
   PTY-BP-PERSISTENT
   PTY-WATCHPOINT
   PTY-BP-NTH
   PTY-BP-TABLE-FULL ;

: PTY-TOOLS ( -- )
   PTY-STEPPER
   PTY-THROW-RECOVERY
   PTY-COMPILE-RECOVERY
   PTY-PKGSCOPE-RECOVERY
   PTY-LASTC-RECOVERY
   PTY-PKGFLOOR-RECOVERY
   PTY-PKGSLOT-RECOVERY
   PTY-REJECT-BARRIER ;

: PTY-HB ( -- )
   PTY-BASIC
   PTY-EDITOR
   PTY-BREAKPOINTS
   PTY-TOOLS
   PTY-STOP-HB
   PTY-HANGUP ;

\ A deadline is no failed case: a reap that ran out of clock, or a wait whose
\ clock ended with the child still at the terminal (src/lib/pty-harness.f
\ WAIT-AFTER-WITHIN), throws E-PROC-TIMEOUT. This run then exits
\ PROC-TIMEOUT-RC (src/lib/process.f), and the gate entry that runs it reports a
\ timeout instead of counting a failure. Every other throw is the run's own.
: RUN ( -- )
   [: CAPTURE-HB PTY-HB ;] catch {: code:n :}
   code E-PROC-TIMEOUT = if
      s" proc-pty: deadline in case " type T-CASES 1 + . cr
      s" proc-pty: a wait or a reap ran out of time" PROC-TIMEOUT-RC die
   then
   code 0<> if code throw then
   T-REPORT ;

RUN

;using

;package
