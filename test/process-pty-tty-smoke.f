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

$3E8 constant STEP-MS              \ one poll
12 constant STEPS                  \ up to 12 s of quiet before a wait gives up
200 constant READS-MAX             \ and at most this many reads with data: a chatterbox never satisfies a wait
$1388 constant EXIT-MS
$4000 constant RCAP

create RBUF RCAP allot
variable RN

: HB$ ( -- ptr u8 len )
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" then >LEN ;

: RCLR ( -- ) 0 RN ! ;

: HAS? ( ptr u8 n -- bool ) {: na:ptr nu:n :}   \ is the text in what was read so far?
   RN @ nu < if false exit then
   0 begin dup RN @ nu - <= while
      RBUF over + nu na nu STR= if drop true exit then
      1+
   repeat drop false ;

\ Read what is ready within STEP-MS and append it: the count, 0 for a quiet poll,
\ -1 once the target's side is gone.
: READ-STEP ( process-pty-handle -- process-pty-handle n )
   RBUF RN @ + RCAP RN @ - STEP-MS PROCESS-PTY:AWAIT-BYTES {: n:n :}
   n 0 > if RN @ n + RN ! then
   n ;

variable EXPECT-I   variable EXPECT-READS

\ Keep reading until the text appears, the target hangs up, STEPS QUIET polls
\ pass in a row, or READS-MAX reads with data have gone by; a line editor
\ redraws the line on every keystroke, so reads with data reset the quiet
\ count but still spend the total budget. The counters live in variables: the
\ linear handle stays on top for READ-STEP. A buffer within a read of full is
\ cleared first: the text is either already in it or still to come.
: EXPECT ( process-pty-handle ptr u8 n -- process-pty-handle bool ) {: na:ptr nu:n :}
   0 EXPECT-I !  0 EXPECT-READS !
   begin EXPECT-I @ STEPS <  EXPECT-READS @ READS-MAX <  and while
      na nu HAS? if true exit then
      RN @ RCAP 512 - > if RCLR then
      READ-STEP {: n:n :}
      n 0 < if na nu HAS? exit then
      n 0= if EXPECT-I @ 1 + EXPECT-I ! else 0 EXPECT-I !  EXPECT-READS @ 1 + EXPECT-READS ! then
   repeat
   na nu HAS? ;

\ After the target exited, read everything it left behind until the hang-up.
: DRAIN-ALL ( process-pty-handle -- process-pty-handle )
   0 EXPECT-READS !
   begin EXPECT-READS @ READS-MAX < while
      RN @ RCAP 512 - > if RCLR then
      READ-STEP 0 < if exit then
      EXPECT-READS @ 1 + EXPECT-READS !
   repeat ;

\ A failed wait shows what the child actually said.
: EXPECT! ( process-pty-handle ptr u8 n -- process-pty-handle ) {: na:ptr nu:n :}
   na nu EXPECT 0= if
      s" expected: " type na nu type cr
      s" read so far (" type RN @ . s" bytes):" type cr RBUF RN @ type cr
      false TTRUE
   then ;

\ Read until three quiet polls in a row: the startup chatter before the first line.
: DRAIN ( process-pty-handle -- process-pty-handle )
   0 EXPECT-I !
   begin EXPECT-I @ 3 < while
      RN @ RCAP 512 - > if RCLR then
      READ-STEP {: n:n :}
      n 0 < if exit then
      n 0= if EXPECT-I @ 1 + EXPECT-I ! else 0 EXPECT-I ! then
   repeat ;

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
   DRAIN RCLR
   s" 2 3 PTYP:AT PTYP:FIRST" SEND
   s" hb: interpret-mode layout value: PTYP:AT" EXPECT!
   DRAIN RCLR
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
\ Recovery must reinstate the REPL's own allocation, not merely the prompt:
\ four cells pushed at once only fit the recovered boot stack, not a leaked
\ pool descriptor, and the depth after recovery is zero. A leaked pool
\ descriptor would end the child with exit 102 at some later push and no
\ prompt would follow.
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
