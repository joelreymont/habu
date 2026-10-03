\ process-pty-tty-smoke.f - the supervisor's pseudo-terminal mode and its public
\ write/await surface (dot habu-give-process-pty-0db35dea).
\
\ Tender's Habu-written standalone gate could not prove REPL recovery: the
\ supervisor connected pipes, so the child engine never installed its line editor
\ and a batch REPL stopped at the first refused definition with exit 70, and the
\ handle exposed no child I/O. Both reproducers are the red-first cases here:
\   - PIPE-STOPS: under SPAWN the refused definition ends the child; the line
\     after it is never evaluated (no "42"), which is the batch contract;
\   - TTY-RECOVERS: under SPAWN-TTY the child prints its prompt, refuses the same
\     definition, evaluates `6 7 * . cr`, answers 42 and prompts again;
\ and both drive the child through PROCESS-PTY:WRITE-LINE / AWAIT-BYTES from
\ outside the package, which used to be E-UNDEFINED.
\
\ Run: bin/hb --load test/process-pty-tty-smoke.f   (HABU_UNDER_TEST names the child)
\
\ The buffer, its compaction, the span search, the wait clock and the
\ never-seen facts are lib/pty-harness.f's. The reading step, the waits and
\ their deadline are this file's own: the bytes come through the supervisor's
\ linear handle, which cannot be handed to the module's own descriptor reader,
\ and a deadline must tear that handle's target down before it throws
\ (DEADLINE), which the harness's WAIT-AFTER-WITHIN does not.

require lib/errors.f
require lib/fmt.f
require lib/process-pty-io.f
require lib/pty-harness.f
require lib/prelude.f
require lib/aio.f
require lib/test.f

package PTY-TTY-SMOKE

using PTY-HARNESS

$64 constant STEP-MS               \ one poll blocks at most this long
$1388 constant EXIT-MS

: HB$ ( -- ptr u8 len )
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" then >LEN ;

: PROMPT$ ( -- ptr u8 n )    s" habu> " ;
: ANYWHERE$ ( -- ptr u8 n )  s" " ;      \ an empty head: the tail may be anywhere

\ Read what is ready within STEP-MS into the harness buffer and append it: the
\ count, 0 for a quiet poll, -1 once the target's side is gone. The linear
\ handle stays on top of the stack, which is why this step is here and not in
\ the module.
: READ-STEP ( process-pty-handle -- process-pty-handle n )
   ROOM$ STEP-MS PROCESS-PTY:AWAIT-BYTES {: n:n :}
   n 0 > if n TOOK then
   n ;

\ What the target said so far: the end of every failed wait's diagnostic, which
\ goes to stdout and leaves stderr to the engine.
: READ-SO-FAR ( -- )
   BUF$ {: ra:ptr ru:n :}
   s" read so far (" type ru FMT:.INT s"  bytes):" type cr ra ru type cr ;

\ A clock that ends while the target still holds its side has not been answered
\ YET: a deadline, not a wrong answer, so it asserts nothing. The linear handle
\ cannot cross the throw, so the target is torn down here; RUN stops the loop
\ and lets E-PROC-TIMEOUT through, so the engine's report of it is the last line
\ on stderr and the gate pool reads the row as a timeout (docs/gate.md). The
\ caller has shown what it waited for.
: DEADLINE ( process-pty-handle -- )
   READ-SO-FAR
   PROCESS-PTY:TEARDOWN
   E-PROC-TIMEOUT throw ;

\ What a wait waited for, and how long.
: WAIT-SHOW ( ptr u8 n ptr u8 n -- ) {: ha:ptr hu:n ta:ptr tu:n :}
   s" expected: " type ta tu type
   hu 0 > if s"  after " type ha hu type then
   s"  (wait budget " type WAIT-BUDGET-MS FMT:.INT s"  ms)" type cr ;

\ Keep reading until the tail appears at or after the head, the target hangs up,
\ or the wait's deadline passes. An empty head waits for the tail anywhere; a
\ real one waits for the two in that order. A child engine answers in as many
\ pieces as the host's scheduling chooses -- a line editor redraws on every
\ keystroke, and a loaded box splits one echo across dozens of reads -- so no
\ count of reads or of quiet polls bounds the wait. A partial read is not an
\ answer and not a failure: only the markers, the hang-up and the clock end it.
\ The hang-up answers whether the tail came before it; the clock is DEADLINE.
\ The linear handle stays on top of the stack for READ-STEP.
: EXPECT-AFTER ( process-pty-handle ptr u8 n ptr u8 n -- process-pty-handle bool )
   {: ha:ptr hu:n ta:ptr tu:n :}
   WAIT-BUDGET-MS WAIT-OPEN
   begin
      ha hu ta tu AFTER? if true exit then
      WAIT-LEFT 0= if ha hu ta tu WAIT-SHOW DEADLINE then
      READ-STEP {: n:n :}
      n 0 < if ha hu ta tu AFTER? exit then
   again ;

\ After the target exited, read everything it left behind until the hang-up. A
\ clock that ends first leaves the claims made over the buffer unproven, so it
\ is a deadline as well.
: DRAIN-ALL ( process-pty-handle -- process-pty-handle )
   WAIT-BUDGET-MS WAIT-OPEN
   begin
      WAIT-LEFT 0= if
         s" expected: the target's hang-up (wait budget " type
         WAIT-BUDGET-MS FMT:.INT s"  ms)" type cr
         DEADLINE
      then
      READ-STEP 0 < if exit then
   again ;

\ The target's exit within EXIT-MS. A target still running when that clock ends
\ has not exited YET, which is a deadline as well.
: EXPECT-EXIT ( process-pty-handle -- process-pty-handle )
   EXIT-MS PROCESS-PTY:AWAIT if exit then
   s" expected: the target's exit (exit budget " type EXIT-MS FMT:.INT s"  ms)" type cr
   DEADLINE ;

\ A wait the target hung up on fails its case with what it waited for and what
\ the target said.
: EXPECT-AFTER! ( process-pty-handle ptr u8 n ptr u8 n -- process-pty-handle )
   {: ha:ptr hu:n ta:ptr tu:n :}
   ha hu ta tu EXPECT-AFTER 0= if ha hu ta tu WAIT-SHOW READ-SO-FAR false TTRUE then ;

: EXPECT! ( process-pty-handle ptr u8 n -- process-pty-handle ) {: ta:ptr tu:n :}
   ANYWHERE$ ta tu EXPECT-AFTER! ;

\ A prompt proves the REPL came back only when it FOLLOWS the answer. The line
\ editor redraws prompt and line on every keystroke (src/habu/repl.f REDRAW,
\ one byte per KEY1), so the echo of the line just typed carries prompts of its
\ own, printed before the child evaluated anything: a bare wait for the prompt
\ is answered by the echo and proves nothing about the line it was typed for.
: PROMPT-AFTER! ( process-pty-handle ptr u8 n -- process-pty-handle ) {: na:ptr nu:n :}
   na nu PROMPT$ EXPECT-AFTER! ;

\ The barrier before a line is typed: wait for the prompt that follows the
\ previous exchange's last answer, then DROP everything read. After it the child
\ is parked in its line editor and prints nothing until the next key, so no
\ byte older than the next line can answer a wait on it -- the boot banner ends
\ in " ok" and a prompt, and the echo of a line carries prompts of its own. A
\ fresh child has echoed nothing, so its first prompt is its own and the head
\ is ANYWHERE$ (test/repl-address-cell-rollback.f PROMPT waits the same way).
: SETTLE ( process-pty-handle ptr u8 n -- process-pty-handle )
   PROMPT-AFTER! BUF-CLEAR ;

\ One line typed at the target. Not SEND: that is the module's own master, and
\ this suite's bytes go through the supervised handle.
: TELL ( process-pty-handle ptr u8 n -- process-pty-handle )
   PROCESS-PTY:WRITE-LINE ;

: BAD-DEF$ ( -- ptr u8 n )  s" : TTY-BAD ( -- n ) drop ;" ;
: ARITH$ ( -- ptr u8 n )    s" 6 7 * . cr" ;

\ ---- red-first case 1: the batch REPL over pipes stops at the refusal --------
: PIPE-STOPS ( -- )
   WATCH-RESET
   BUF-CLEAR
   s" 42" WATCH+ {: answered:watch :}   \ the claim below outlives every compaction
   HB$ PROCESS-PTY:SPAWN
   PROCESS-PTY:LAUNCH
   BAD-DEF$ TELL
   ARITH$ TELL
   PROCESS-PTY:END-INPUT
   s" non-certified definition: tty-bad" EXPECT!
   EXPECT-EXIT
   DRAIN-ALL
   answered NEVER-SEEN? TTRUE
   PROCESS-PTY:TEARDOWN
   s" PASS: batch REPL over pipes stops at the refused definition" type cr ;

\ ---- red-first case 2: the REPL on a pseudo-terminal refuses and recovers ----
: TTY-RECOVERS ( -- )
   BUF-CLEAR
   HB$ PROCESS-PTY:SPAWN-TTY
   PROCESS-PTY:LAUNCH
   ANYWHERE$ SETTLE
   BAD-DEF$ TELL
   s" tty-bad" EXPECT!
   BUF-CLEAR
   ARITH$ TELL
   s" 42" PROMPT-AFTER!
   PROCESS-PTY:ALIVE? TTRUE
   PROCESS-PTY:TEARDOWN
   s" PASS: REPL on a pseudo-terminal refuses, recovers and prompts again" type cr ;

\ A normal checked structure needs no TRUSTED seed. The same constructor and
\ projection are refused as bare tokens and work inside a checked definition.
: TTY-LAYOUT ( -- )
   BUF-CLEAR
   HB$ PROCESS-PTY:SPAWN-TTY
   PROCESS-PTY:LAUNCH
   ANYWHERE$ SETTLE
   s" package PTYP public STRUCTURE point 0 FIELD x n FIELD y n ;STRUCTURE : AT ( n n -- point ) PTYP-POINT:MAKE ; : FIRST ( point -- n ) PTYP-POINT:UNMAKE drop ; : FIRST-X ( -- n ) 2 3 AT FIRST ; ;package" TELL
   s"  ok" SETTLE
   s" 2 3 PTYP:AT PTYP:FIRST" TELL
   s" hb: interpret-mode layout value: PTYP:AT" SETTLE
   s" PTYP:FIRST-X . cr depth . cr" TELL
   S\" \r\n2\r\n" EXPECT!
   S\" \r\n0\r\n" PROMPT-AFTER!
   PROCESS-PTY:ALIVE? TTRUE
   PROCESS-PTY:TEARDOWN
   s" PASS: checked layout calculation works at the REPL after bare-layout refusal" type cr ;

\ An uncaught throw from inside run-in-stack reaches the tty REPL while the
\ callback's guarded allocation is active -- run-in-stack now refuses anything
\ that is not a real guarded mapping (lib/memory.f MEM-ALLOC-GUARDED), so
\ TTY-POOL is one, and TTY-RAISE's throw still escapes uncaught from inside it.
\ Recovery must reinstate the REPL's own allocation, not merely the prompt,
\ and `depth` is the value that says so: src/habu/habu1.f BDEPTH subtracts the
\ CONTENTS of the active base cell (src/habu/layout.f S0-CELL, which is
\ STACK-ABI:BASE-CELL) from XDS, so it reads zero only when the cursor and
\ that cell came back TOGETHER. A recovery that reset the cursor
\ to the boot stack but left TTY-POOL's base installed would print the
\ distance between the two mappings here instead. The four values pushed and
\ printed ahead of it show the recovered stack works at all -- they would
\ also fit inside the pool, which is a whole 64 KB page, so it is the zero
\ and not the four that identifies which stack they landed on.
\ The pool's address lives in a declared pointer cell read back through
\ `TTY-POOL`, because a `constant` is raw storage and a raw cell never holds an
\ address. The backing cell is named short on purpose: this whole line is TYPED
\ AT THE REPL, whose line editor holds 256 bytes (`src/habu/repl.f` LBUF), and a
\ line one byte over that loses its tail silently -- the definitions after the
\ cut are simply never made and the case fails on E-UNDEFINED much later.
: STACK-DEFS$ ( -- ptr u8 n )
   s" require lib/memory.f PTR-VARIABLE POOL-A STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED constant TTY-POOL-CAP POOL-A ! : TTY-POOL ( -- ptr u8 ) POOL-A @ ; : TTY-RAISE ( -- ) 7 throw ; : TTY-CROSS ( -- ) ['] TTY-RAISE TTY-POOL TTY-POOL-CAP run-in-stack ;" ;

: TTY-STACK-RECOVERS ( -- )
   BUF-CLEAR
   HB$ PROCESS-PTY:SPAWN-TTY
   PROCESS-PTY:LAUNCH
   ANYWHERE$ SETTLE
   STACK-DEFS$ TELL
   s" ok" EXPECT!
   BUF-CLEAR
   WATCH-RESET
   s" E-UNDEFINED" WATCH+ {: undef:watch :}
   s" non-certified" WATCH+ {: refused:watch :}
   s" TTY-CROSS" TELL
   s" ?" EXPECT!                          \ the uncaught throw's mark, with nothing refused or undefined
   undef NEVER-SEEN? TTRUE
   refused NEVER-SEEN? TTRUE
   s" ?" PROMPT-AFTER!
   BUF-CLEAR
   s" 1 2 3 4 . . . . depth . cr" TELL
   S\" 4\r\n3\r\n2\r\n1\r\n0\r\n" PROMPT-AFTER!
   PROCESS-PTY:ALIVE? TTRUE
   PROCESS-PTY:TEARDOWN
   s" PASS: REPL recovers its own stack allocation after an uncaught throw inside run-in-stack" type cr ;

\ ---- the prompt barrier itself is under test --------------------------------
\ The line editor redraws prompt and line on every keystroke, so the echo of the
\ line just typed carries prompts of its own. This case reads the buffer the
\ barrier ended on and puts the two in order: the FIRST prompt in it is one the
\ editor redrew, printed before the answer -- that is the one a bare
\ `s" habu> " EXPECT!` takes, and reads append in order, so such a wait can
\ return before the child has evaluated anything -- while the barrier's prompt
\ is a later one that only the finished line could print.
: ECHO-PROMPT-REJECTED ( -- )
   BUF-CLEAR
   HB$ PROCESS-PTY:SPAWN-TTY
   PROCESS-PTY:LAUNCH
   ANYWHERE$ SETTLE
   ARITH$ TELL
   s" 42" PROMPT-AFTER!
   0 PROMPT$ FIND-FROM {: echo-at:n :}
   0 s" 42" FIND-FROM {: ans-at:n :}
   echo-at 0 >= TTRUE                      \ a bare prompt wait had a prompt to take
   echo-at ans-at < TTRUE                  \ and it was the editor's, ahead of the answer
   s" 42" PROMPT$ FIND-AFTER ans-at > TTRUE   \ the barrier took one that follows it
   PROCESS-PTY:ALIVE? TTRUE
   PROCESS-PTY:TEARDOWN
   s" PASS: the prompt barrier is not answered by the echo's own prompt" type cr ;

: CASES ( -- )
   PIPE-STOPS
   TTY-RECOVERS
   ECHO-PROMPT-REJECTED
   TTY-LAYOUT
   TTY-STACK-RECOVERS ;

public

\ AWAIT and AWAIT-BYTES wait on the AIO loop, so the loop runs for the body of
\ RUN: it is started after the last definition, because a live task forbids
\ compilation. It stops however the cases end, so a deadline's throw (DEADLINE)
\ passes on with no loop running.
: RUN ( -- )
   T-RESET
   AIO:START
   [: CASES ;] [: AIO:STOP ;] finally
   T-REPORT
   s" process-pty-tty-smoke: ok" type cr ;

;using

;package

PTY-TTY-SMOKE:RUN
