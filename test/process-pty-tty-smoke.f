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

require lib/process-pty-io.f
require lib/prelude.f
require lib/test.f

package PTY-TTY-SMOKE

$3E8 constant STEP-MS              \ one poll blocks at most this long
$4E20 constant WAIT-MS             \ and one wait runs at most this long on the clock
3 constant QUIET-POLLS             \ polls in a row that bring nothing and so end a drain
$200 constant KEEP-TAIL            \ bytes kept when a full buffer is compacted
$1388 constant EXIT-MS
$4000 constant RCAP

create RBUF RCAP allot
variable RN
variable DEADLINE                  \ absolute monotonic end of the wait in flight
variable QUIET-N

: HB$ ( -- ptr u8 len )
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" then >LEN ;

: RCLR ( -- ) 0 RN ! ;

\ One wait runs at a time, so a single deadline cell serves every loop here. The
\ deadline is absolute and is tested before each poll, so a wait overruns it by
\ at most the one poll already in flight and never restarts its budget.
: WAIT-OPEN ( n -- ) {: ms:n :}
   ms >MS PROC-DEADLINE-AT DEADLINE ! ;

: WAIT-LEFT ( -- n )
   DEADLINE @ PROC-LEFT-MS MS>N ;

: HAS? ( ptr u8 n -- bool ) {: na:ptr nu:n :}   \ is the text in what was read so far?
   RN @ nu < if false exit then
   0 begin dup RN @ nu - <= while
      RBUF over + nu na nu STR= if drop true exit then
      1+
   repeat drop false ;

\ A full buffer keeps its tail rather than dropping everything: a marker split
\ across the compaction survives whole in what is kept, and every older byte has
\ already been searched. KEEP-TAIL is far longer than any marker below.
: KEEP-TAIL! ( -- )
   RN @ RCAP KEEP-TAIL - < if exit then
   RBUF RN @ KEEP-TAIL - + RBUF KEEP-TAIL BYTE-COPY
   KEEP-TAIL RN ! ;

\ Read what is ready within STEP-MS and append it: the count, 0 for a quiet poll,
\ -1 once the target's side is gone.
: READ-STEP ( process-pty-handle -- process-pty-handle n )
   KEEP-TAIL!
   RBUF RN @ + RCAP RN @ - STEP-MS PROCESS-PTY:AWAIT-BYTES {: n:n :}
   n 0 > if RN @ n + RN ! then
   n ;

\ Keep reading until the text appears, the target hangs up, or the wait's
\ deadline passes. A child engine answers in as many pieces as the host's
\ scheduling chooses -- a line editor redraws on every keystroke, and a loaded
\ box splits one echo across dozens of reads -- so no count of reads or of quiet
\ polls bounds the wait. A partial read is not an answer and not a failure: only
\ the marker, the hang-up and the clock end the loop. The linear handle stays on
\ top of the stack for READ-STEP.
: EXPECT ( process-pty-handle ptr u8 n -- process-pty-handle bool ) {: na:ptr nu:n :}
   WAIT-MS WAIT-OPEN
   begin
      na nu HAS? if true exit then
      WAIT-LEFT 0= if false exit then
      READ-STEP {: n:n :}
      n 0 < if na nu HAS? exit then
   again ;

\ After the target exited, read everything it left behind until the hang-up.
: DRAIN-ALL ( process-pty-handle -- process-pty-handle )
   WAIT-MS WAIT-OPEN
   begin WAIT-LEFT 0 > while
      READ-STEP 0 < if exit then
   repeat ;

\ A failed wait shows how long it waited and what the child actually said.
: EXPECT! ( process-pty-handle ptr u8 n -- process-pty-handle ) {: na:ptr nu:n :}
   na nu EXPECT 0= if
      s" expected: " type na nu type s"  (wait budget " type WAIT-MS . s" ms)" type cr
      s" read so far (" type RN @ . s" bytes):" type cr RBUF RN @ type cr
      false TTRUE
   then ;

\ Read until QUIET-POLLS polls in a row bring nothing, the target hangs up, or
\ the deadline passes, and then DROP what was read. A drain is the barrier before
\ a line is typed, so everything it swallowed -- the boot banner among it --
\ must leave the buffer with it: the banner ends in " ok" and a prompt, and a
\ wait that could match those would be answered by bytes older than the line it
\ is waiting on.
: DRAIN ( process-pty-handle -- process-pty-handle )
   WAIT-MS WAIT-OPEN
   0 QUIET-N !
   begin QUIET-N @ QUIET-POLLS <  WAIT-LEFT 0 >  and while
      READ-STEP {: n:n :}
      n 0 < if RCLR exit then
      n 0= if QUIET-N @ 1 + QUIET-N ! else 0 QUIET-N ! then
   repeat
   RCLR ;

: SEND ( process-pty-handle ptr u8 n -- process-pty-handle )
   PROCESS-PTY:WRITE-LINE ;

: BAD-DEF$ ( -- ptr u8 n )  s" : TTY-BAD ( -- n ) drop ;" ;
: ARITH$ ( -- ptr u8 n )    s" 6 7 * . cr" ;

\ ---- red-first case 1: the batch REPL over pipes stops at the refusal --------
: PIPE-STOPS ( -- )
   RCLR
   HB$ PROCESS-PTY:SPAWN
   PROCESS-PTY:LAUNCH
   BAD-DEF$ SEND
   ARITH$ SEND
   PROCESS-PTY:END-INPUT
   s" non-certified definition: tty-bad" EXPECT!
   EXIT-MS PROCESS-PTY:AWAIT TTRUE
   DRAIN-ALL
   s" 42" HAS? 0= TTRUE
   PROCESS-PTY:TEARDOWN
   s" PASS: batch REPL over pipes stops at the refused definition" type cr ;

\ ---- red-first case 2: the REPL on a pseudo-terminal refuses and recovers ----
: TTY-RECOVERS ( -- )
   RCLR
   HB$ PROCESS-PTY:SPAWN-TTY
   PROCESS-PTY:LAUNCH
   DRAIN
   BAD-DEF$ SEND
   s" tty-bad" EXPECT!
   RCLR
   ARITH$ SEND
   s" 42" EXPECT!
   s" habu> " EXPECT!
   PROCESS-PTY:ALIVE? TTRUE
   PROCESS-PTY:TEARDOWN
   s" PASS: REPL on a pseudo-terminal refuses, recovers and prompts again" type cr ;

\ A normal checked structure needs no TRUSTED seed. The same constructor and
\ projection are refused as bare tokens and work inside a checked definition.
: TTY-LAYOUT ( -- )
   RCLR
   HB$ PROCESS-PTY:SPAWN-TTY
   PROCESS-PTY:LAUNCH
   DRAIN
   s" package PTYP public STRUCTURE point 0 FIELD x n FIELD y n ;STRUCTURE : AT ( n n -- point ) PTYP-POINT:MAKE ; : FIRST ( point -- n ) PTYP-POINT:UNMAKE drop ; : FIRST-X ( -- n ) 2 3 AT FIRST ; ;package" SEND
   DRAIN
   s" 2 3 PTYP:AT PTYP:FIRST" SEND
   s" hb: interpret-mode layout value: PTYP:AT" EXPECT!
   DRAIN
   s" PTYP:FIRST-X . cr depth . cr" SEND
   S\" \r\n2\r\n" EXPECT!
   S\" \r\n0\r\n" EXPECT!
   s" habu> " EXPECT!
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
: STACK-DEFS$ ( -- ptr u8 n )
   s" require lib/memory.f STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED constant TTY-POOL-CAP constant TTY-POOL : TTY-RAISE ( -- ) 7 throw ; : TTY-CROSS ( -- ) ['] TTY-RAISE TTY-POOL TTY-POOL-CAP run-in-stack ;" ;

: TTY-STACK-RECOVERS ( -- )
   RCLR
   HB$ PROCESS-PTY:SPAWN-TTY
   PROCESS-PTY:LAUNCH
   DRAIN
   STACK-DEFS$ SEND
   s" ok" EXPECT!
   RCLR
   s" TTY-CROSS" SEND
   s" ?" EXPECT!                          \ the uncaught throw's mark, with nothing refused or undefined
   s" E-UNDEFINED" HAS? 0= TTRUE
   s" non-certified" HAS? 0= TTRUE
   s" habu> " EXPECT!
   RCLR
   s" 1 2 3 4 . . . . depth . cr" SEND
   S\" 4\r\n3\r\n2\r\n1\r\n0\r\n" EXPECT!
   s" habu> " EXPECT!
   PROCESS-PTY:ALIVE? TTRUE
   PROCESS-PTY:TEARDOWN
   s" PASS: REPL recovers its own stack allocation after an uncaught throw inside run-in-stack" type cr ;

public

: RUN ( -- )
   T-RESET
   PIPE-STOPS
   TTY-RECOVERS
   TTY-LAYOUT
   TTY-STACK-RECOVERS
   T-REPORT
   s" process-pty-tty-smoke: ok" type cr ;

;package

PTY-TTY-SMOKE:RUN
