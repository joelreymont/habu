\ proc-pty.f — focused native process/PTY harness. Run with:
\   bin/hb --load lib/errors.f lib/process.f test/proc-pty.f
\ The engine bakes neither POLL-IN nor the process primitives below them, so the
\ two library files are part of the invocation; test/runtime-regression-test.f
\ (GE-PROCESS-PTY) drives exactly this line, and the engine to test can follow
\ as `-- <path>` or in HABU_UNDER_TEST.
\
\ The terminal itself — the pair, the spawn, the reads, the waits and both
\ barrier shapes — is lib/pty-harness.f; this file types at the child and says
\ what the answers must be.

require lib/pty-harness.f

package PROC-PTY

using PTY-HARNESS

1 constant PTY-QUIET-POLLS         \ polls in a row that bring nothing and so end a drain

variable #FAIL
variable #CASE
variable IN-R
variable IN-W
variable OUT-R
variable OUT-W
variable ERR-R
variable ERR-W
variable PID

: HB-ARG? ( -- bool )
   SCRIPT-ARGC 0 > ;

: HB-EXE$ ( -- ptr u8 n )
   HB-ARG? if 0 SCRIPT-ARGV$ exit then
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: BUF-N ( -- n )
   BUF$ {: a:ptr u:n :} u ;

: BUF-DUMP ( -- )
   s" buf bytes: " type BUF-N . cr
   BUF-N 0= if exit then
   s" buf:" type cr
   BUF$ type cr ;

: T-FAIL ( -- )
   [char] F emit #CASE @ . cr
   BUF-DUMP
   #FAIL @ 1 + #FAIL ! ;

: T= {: got want :} ( n n -- )
   #CASE @ 1 + #CASE !
   got want <> if T-FAIL then ;

: TTRUE ( bool -- )
   #CASE @ 1 + #CASE !
   0= if T-FAIL then ;

: TCONTAINS ( ptr u8 n -- )
   IN-BUF? TTRUE ;

\ Completion, in this file's own counters: the bounded reaps answer an outcome,
\ and only a clean exit with this code passes. A reap that ran out of clock
\ answers timeout, which reds here instead of hanging the run.
: T-EXIT= ( outcome n -- ) {: want:n :}
   MATCH outcome
     exited OF want T= ENDOF
     signaled OF drop 1 0 T= ENDOF
     timeout OF 1 0 T= ENDOF
   ;MATCH ;

\ Every drain starts from an empty buffer: the bytes it swallows are the ones a
\ later wait must not be answered by.
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
   WAIT-FOR TTRUE ;

\ Wait for a marker and close an absence claim's window at its end. The marker
\ has to be one the child prints PAST the point where the rejected text could
\ have appeared; a failed wait leaves no window, so the claim behind it is
\ refused as well.
: BARRIER ( ptr u8 n -- )
   WAIT-BARRIER TTRUE ;

\ The barrier for a claim about the line before it: the child prints the hex
\ literal in decimal, so the answer's digits are in nothing the line editor
\ echoed back and only the evaluation can satisfy the wait. The REPL reads the
\ next line after finishing the one before it, so this answer proves that line
\ is over -- diagnostics, trailer and all.
: PROBE-BARRIER ( -- )
   s" $BEEF ." SEND-LINE
   s" 48879" BARRIER ;

: REJECT ( ptr u8 n -- )
   WINDOW-ABSENT? TTRUE ;

: EXPECT-OK ( -- )
   s"  ok" EXPECT ;

: EXPECT-PROMPT ( -- )
   s" habu> " EXPECT ;

: CAPTURE-PIPES ( -- )
   PIPE-PAIR IN-W ! IN-R !
   PIPE-PAIR OUT-W ! OUT-R !
   PIPE-PAIR ERR-W ! ERR-R !
   IN-W @ >FD FD-CLOEXEC!
   OUT-R @ >FD FD-CLOEXEC!
   ERR-R @ >FD FD-CLOEXEC! ;

: CAPTURE-START-HB ( -- )
   HB-EXE$ >LEN IN-R @ >FD OUT-W @ >FD ERR-W @ >FD PROC-SPAWN-IO PID !
   PID @ 0 > TTRUE ;

: CAPTURE-CLOSE-CHILD-ENDS ( -- )
   IN-R @ close
   OUT-W @ close
   ERR-W @ close ;

: CAPTURE-SEND-SOURCE ( -- )
   IN-W @ >FD s" 2 3 + ." WRITE-LINE
   IN-W @ close ;

: CAPTURE-EXPECT-RC ( -- )
   PID @ >PID WAIT-BUDGET-MS WAIT-EXIT 0 T-EXIT= ;

: CAPTURE-EXPECT-OUT ( -- )
   BUF-CLEAR
   OUT-R @ >FD READ-TO-EOF
   s" 5" TCONTAINS
   OUT-R @ close ;

: CAPTURE-EXPECT-ERR ( -- )
   BUF-CLEAR
   ERR-R @ >FD READ-TO-EOF
   BUF-N 0 T=
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
   s"  ok" EXPECT
   s" habu> " EXPECT ;

\ --- the two boot modes must enumerate ONE dictionary --------------------------
\ Dot habu-decide-arm-the-5234727b (USER RULING 2026-08-11: one dictionary surface
\ for every boot mode). The engine's baked REPL words are seeded into the
\ dictionary at boot; that seed used to run at the interactive REPL entry
\ alone and now runs at the end of the engine prefix on every boot. Every case in
\ this file already depends on the interactive half - without the seed there is no
\ prompt to type at - so what is left to prove is that the BATCH dictionary is the
\ SAME SET, not merely a set that also answers.
\
\ test/aot-seed-surface.f folds every dictionary record name into one number.
\ SEED-BATCH runs it through this engine as a PIPED program and keeps the number
\ it printed; PTY-SEED-SURFACE types the same line at the terminal and requires
\ the same number back. Neither side hardcodes it - it moves with every tree edit
\ - and a mode that seeded a different set, in a different order, or at a
\ different point cannot produce it.
\
\ The batch half lives here rather than in test/aot-seed-batch-suite.f (which owns
\ the batch-only cases) because the comparison needs both modes in ONE process,
\ this file is the tree's one cross-platform PTY driver, and its pipe vehicle is
\ four lines away. The pty case must run FIRST, before any case types a definition
\ into the interactive dictionary, or it would be comparing two different moments.
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
   PID @ >PID WAIT-BUDGET-MS WAIT-EXIT 0 T-EXIT=
   SEEDNUM-U @ 0 > TTRUE ;

: PTY-SEED-SURFACE ( -- )
   SEED-LINE$ STEP-LN
   SEEDNUM$ EXPECT
   s"  ok" EXPECT ;

: PTY-ARITH ( -- )
   s" 1 2 + ." STEP-LN
   s" 3" EXPECT
   s"  ok" EXPECT
   s" habu> " EXPECT ;

: PTY-UNKNOWN ( -- )
   s" frobnicate" STEP-LN
   s" E-UNDEFINED: frobnicate" EXPECT
   s" ?" EXPECT
   s" habu> " EXPECT
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-SQUARE ( -- )
   s" : SQ dup * ;" STEP-LN
   s"  ok" EXPECT
   s" 7 SQ ." STEP-LN
   s" 49" EXPECT
   s"  ok" EXPECT ;

\ Certified word on an empty interpret stack: named underdepth reject, then
\ the REPL recovers and the next line evaluates (LDIAGRET recovery leg; dot
\ habu-habu-certified-words-84e84eaf).
: PTY-UNDERDEPTH ( -- )
   s" SQ" STEP-LN
   s" hb: interpret stack underdepth: SQ" EXPECT
   s" habu> " EXPECT
   PROBE-BARRIER
   s"  ok" REJECT
   s" 6 SQ ." STEP-LN
   s" 36" EXPECT
   s"  ok" EXPECT ;

: PTY-BACKSPACE ( -- )
   s" 1 2 + .." SEND
   127 SEND-BYTE
   10 SEND-BYTE
   MFD-DRAIN
   s" 3" EXPECT
   s"  ok" EXPECT ;

: PTY-CANCEL ( -- )
   s" garbage" SEND
   3 SEND-BYTE
   MFD-DRAIN
   s" habu> " EXPECT
   PROBE-BARRIER
   s" garbage?" REJECT ;

: PTY-EDIT-SEED ( -- )
   s" 5 ." STEP-LN
   s" 5" EXPECT
   s"  ok" EXPECT ;

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
   s"  ok" EXPECT ;

: PTY-EDIT-HOME ( -- )
   PTY-EDIT-SEED
   PTY-EDIT-LEFT3
   PTY-EDIT-INSERT-RUN ;

: PTY-HISTORY-UP ( -- )
   65 SEND-ESC
   10 SEND-BYTE
   MFD-DRAIN
   s" 103" EXPECT
   s"  ok" EXPECT ;

\ The engine bakes the REPL and nothing else (src/habu/native-runtime.f,
\ src/habu/stdin.f): a session that wants breakpoints, watch cells or the token
\ stepper loads them here, which is also how a REPL user reaches them.
: PTY-DEBUGGER-LOAD ( -- )
   s" require src/habu/debug.f" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-SOURCE ( -- )
   s" : SQB dup * ;" STEP-LN
   s"  ok" EXPECT
   s" : IN1 1 + ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-ARM-ONESHOT ( -- )
   s" ' SQB BP+" STEP-LN
   s"  ok" EXPECT
   s" ' IN1 BP+" STEP-LN
   s"  ok" EXPECT ;

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
   s"  ok" EXPECT ;

: PTY-PB-ARM ( -- )
   s" ' PB BP*" STEP-LN
   s"  ok" EXPECT ;

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
   s"  ok" EXPECT ;

: PTY-BP-PERSISTENT ( -- )
   PTY-PB-SOURCE
   PTY-PB-ARM
   PTY-PB-FIRST
   PTY-PB-SECOND
   PTY-PB-CLEAR ;

: PTY-WATCH-VAR ( -- )
   s" variable WV" STEP-LN
   s"  ok" EXPECT
   s" 17 WV !" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-ADD ( -- )
   s" WV BPW+" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-WORD ( -- )
   s" : WID dup WV @ + ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-SOURCE ( -- )
   PTY-WATCH-VAR
   PTY-WATCH-ADD
   PTY-WATCH-WORD ;

: PTY-WATCH-ARM ( -- )
   s" ' WID BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-RUN ( -- )
   s" 2 WID ." STEP-LN
   s" habu-bp-stack:" EXPECT
   s" habu-bp-watch:" EXPECT
   s" 0000000000000011" EXPECT
   s" 19" EXPECT ;

: PTY-WATCH-CLEAR ( -- )
   s" WV BPW-" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCHPOINT ( -- )
   PTY-WATCH-SOURCE
   PTY-WATCH-ARM
   PTY-WATCH-RUN
   PTY-WATCH-CLEAR ;

: PTY-BPN-ARM ( -- )
   s" 2 ' PB BPN" STEP-LN
   s"  ok" EXPECT ;

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
   s"  ok" EXPECT ;

: PTY-BP-NTH ( -- )
   PTY-BPN-ARM
   PTY-BPN-SKIP
   PTY-BPN-SKIP
   PTY-BPN-FIRE
   PTY-BPN-CLEAR ;

: PTY-DEFINE-F0-F2 ( -- )
   s" : F0 0 ;" STEP-LN
   s"  ok" EXPECT
   s" : F1 1 ;" STEP-LN
   s"  ok" EXPECT
   s" : F2 2 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F3-F4 ( -- )
   s" : F3 3 ;" STEP-LN
   s"  ok" EXPECT
   s" : F4 4 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F0-F4 ( -- )
   PTY-DEFINE-F0-F2
   PTY-DEFINE-F3-F4 ;

: PTY-DEFINE-F5-F6 ( -- )
   s" : F5 5 ;" STEP-LN
   s"  ok" EXPECT
   s" : F6 6 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F7-F8 ( -- )
   s" : F7 7 ;" STEP-LN
   s"  ok" EXPECT
   s" : F8 8 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F5-F8 ( -- )
   PTY-DEFINE-F5-F6
   PTY-DEFINE-F7-F8 ;

: PTY-BP-F0-F2 ( -- )
   s" ' F0 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F1 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F2 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-F3-F4 ( -- )
   s" ' F3 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F4 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-F0-F4 ( -- )
   PTY-BP-F0-F2
   PTY-BP-F3-F4 ;

: PTY-BP-F5-F6 ( -- )
   s" ' F5 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F6 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-F7-F8 ( -- )
   s" ' F7 BP+" STEP-LN
   s"  ok" EXPECT
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
   s"  ok" EXPECT ;

: PTY-STEP-TOKENS ( -- )
   s" step 2 3 + ." STEP-LN
   s" step> 2" EXPECT
   s" step> 3" EXPECT
   s" step> +" EXPECT
   s" 5" EXPECT ;

: PTY-STEP-DEFINE ( -- )
   s" step : SD dup * ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-STEP-RUN ( -- )
   s" 4 SD ." STEP-LN
   s" 16" EXPECT ;

: PTY-STEP-RECOVER ( -- )
   s" 8 ." STEP-LN
   s" 8" EXPECT
   s"  ok" EXPECT ;

: PTY-STEPPER ( -- )
   PTY-STEP-BASELINE
   PTY-STEP-TOKENS
   PTY-STEP-DEFINE
   PTY-STEP-RUN
   PTY-STEP-RECOVER ;

: PTY-THROW-LINE ( -- )
   s" 99 throw" STEP-LN
   s" ?" EXPECT
   s" habu> " EXPECT
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-THROW-AFTER ( -- )
   s" 6 ." STEP-LN
   s" 6" EXPECT
   s"  ok" EXPECT ;

\ tty-REPL parity for the LCOMPILEDIE compile-error family (dot
\ habu-convert-residual-compile-f460b9f2). A recoverable compile die typed at the
\ interactive REPL now recovers the session exactly like an undefined word
\ (PTY-UNKNOWN / LDIAGRET), instead of exiting the tty. Before the parity change
\ the same dup-def line exit-group'd and killed the REPL. dup-def is the
\ rollback-past-a-published-record case: the first PRDUP commits on its own line;
\ the second line's failure rolls back to that line's start, so PRDUP survives.
: PTY-COMPILE-DEF ( -- )
   s" : PRDUP ( -- ) ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-COMPILE-RECOVER ( -- )
   s" : PRDUP ( -- ) ;" STEP-LN
   s" duplicate definition: PRDUP" EXPECT
   s" habu> " EXPECT
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-COMPILE-AFTER ( -- )
   s" 7 ." STEP-LN
   s" 7" EXPECT
   s"  ok" EXPECT ;

: PTY-COMPILE-RECOVERY ( -- )
   PTY-COMPILE-DEF
   PTY-COMPILE-RECOVER
   PTY-COMPILE-AFTER ;

\ Package-scope rollback at the tty REPL (dot habu-recovery-pkg-scope-e0bd98e2). A
\ line that opens a package and then aborts mid-definition must roll the open-package
\ scope back to the line-start scope (global here), NOT leave the package dangling
\ open so later defines land in it silently. Before the fix the aborted
\ `package PRP public : PRFOO NOPEWORD ;` left PRP open, and the next `package PRQ`
\ nest-rejected. Now: the failing line recovers, a FRESH package opens cleanly, and
\ the next define lands global and runs.
: PTY-PKGSCOPE-FAIL ( -- )
   s" package PRP public : PRFOO ( -- ) NOPEWORD ;" STEP-LN
   s" E-UNDEFINED: NOPEWORD" EXPECT
   s" habu> " EXPECT
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-PKGSCOPE-FRESH ( -- )                        \ a dangling PRP would make this `package` nest-reject (no " ok")
   s" package PRQ ;package" STEP-LN
   s"  ok" EXPECT ;

: PTY-PKGSCOPE-GLOBAL ( -- )                       \ the next define lands global and runs; a checked reference confirms checker scope is in step
   s" : PRDONE ( -- n ) 4242 ;  : PRUSE ( -- n ) PRDONE ;  PRUSE ." STEP-LN
   s" 4242" EXPECT
   s"  ok" EXPECT ;

: PTY-PKGSCOPE-RECOVERY ( -- )
   PTY-PKGSCOPE-FAIL
   PTY-PKGSCOPE-FRESH
   PTY-PKGSCOPE-GLOBAL ;

\ The barrier itself, against the live child. An absence claim over a buffer the
\ child has not answered into is granted by the harness's own silence: a drain
\ that returned on its first quiet poll leaves exactly that buffer. One leg has
\ no barrier behind it and must be REFUSED; the other has one and must still
\ catch the text when the child really printed it.
: PTY-REJECT-UNBARRIERED ( -- )
   s" frobnicate" STEP-LN
   BUF-CLEAR                          \ the child's answer is not in the buffer
   s"  ok" WINDOW-ABSENT? 0= TTRUE    \ so the claim is refused
   PROBE-BARRIER
   s"  ok" WINDOW-ABSENT? TTRUE ;     \ and granted once a barrier closes the window

: PTY-REJECT-CATCHES ( -- )
   s" 5 ." STEP-LN                    \ a line that DOES print the trailer
   PROBE-BARRIER
   s"  ok" WINDOW-ABSENT? 0= TTRUE ;  \ the window holds it, so the claim is refuted

: PTY-REJECT-BARRIER ( -- )
   PTY-REJECT-UNBARRIERED
   PTY-REJECT-CATCHES ;

: PTY-THROW-RECOVERY ( -- )
   PTY-THROW-LINE
   PTY-THROW-AFTER ;

\ ^D becomes EOF only while the terminal is RAW, which it is only while the
\ child sits in its line editor. A ^D that arrives while the line before it is
\ still executing is taken by the restored canonical discipline as an
\ empty-line EOF instead, never becomes a key, and leaves the child waiting for
\ one that never comes -- with the harness waiting on its exit, that is a hang,
\ not a failure. Only REDRAW prints the prompt and only after RAW-ON, so a
\ prompt printed after the buffer was cleared is proof that the editor holds
\ the terminal. The key that asks for one is DEL: in the editor it deletes
\ nothing from an empty line and redraws, and in canonical mode it is VERASE
\ over an empty buffer, so neither mode can act on it or read it as a signal.
: PTY-EDITOR-READY ( -- )
   BUF-CLEAR
   127 SEND-BYTE
   s" habu> " EXPECT ;

: PTY-STOP-HB ( -- )
   PTY-EDITOR-READY
   4 SEND-BYTE
   REAP 0 T-EXIT=
   CLOSE-MASTER ;

\ --- a terminal that hangs up mid-line ends the session -----------------------
\ Dot habu-make-key1-end-cabdb980. KEY1 is one 1-byte read of descriptor 0. It
\ used to drop the count, so a read of 0 (the far side hung up; a slave whose pty
\ master has closed reads 0) or -1 (a failed read) handed back the byte still in
\ the key buffer and the child re-ran it for ever instead of leaving the editor.
\ That is a spin, not a failure, so this case bounds its own reap: PROC-WAIT-RC
\ has no timeout and would hang the run the defect is meant to fail. The watch
\ descriptor opens while the child is still alive -- macOS cannot register a
\ process that has already exited (test/proc-watch-smoke.f).
\ This case runs LAST and owns the second child: PTY-STOP-HB reaped the first and
\ closed the master, and the harness holds one pair at a time.

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
   gone TTRUE
   gone if
      CHILD-PID PROC-WAIT-RC MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
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
   PTY-SEED-SURFACE                  \ FIRST at the prompt: before any case types a definition
   PTY-ARITH
   PTY-UNKNOWN
   PTY-SQUARE
   PTY-UNDERDEPTH ;

: PTY-EDITOR ( -- )
   PTY-BACKSPACE
   PTY-CANCEL
   PTY-EDIT-HOME
   PTY-HISTORY-UP ;

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
   PTY-REJECT-BARRIER ;

: PTY-HB ( -- )
   PTY-BASIC
   PTY-EDITOR
   PTY-BREAKPOINTS
   PTY-TOOLS
   PTY-STOP-HB
   PTY-HANGUP ;

: REPORT ( -- )
   #FAIL @ 0 = if s" PASS: process/pty primitives" type cr exit then
   #FAIL @ . s" proc-pty: failures" 1 die ;

CAPTURE-HB
PTY-HB
REPORT

;using

;package
