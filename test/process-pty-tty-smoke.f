\ process-pty-tty-smoke.f - the supervisor's pseudo-terminal mode and its public
\ write/await surface, driven through PROCESS-PTY:WRITE-LINE / AWAIT-BYTES from
\ outside the package.
\ Run: bin/hb --load test/process-pty-tty-smoke.f   (HABU_UNDER_TEST names the child)

require src/lib/errors.f
require src/lib/fmt.f
require src/lib/process-pty-io.f
require src/lib/pty-harness.f
require src/lib/prelude.f
require src/lib/string.f
require src/lib/aio.f
require src/lib/test.f
require src/lib/test/wait.f
require src/lib/task.f                 \ TASK:SLEEP between probes of a sleeper's pid
require src/lib/engine-candidate.f

package PTY-TTY-SMOKE

using PTY-HARNESS

$64 constant STEP-MS               \ one poll blocks at most this long
$1388 constant EXIT-MS
$1388 constant GONE-MS             \ a killed sleeper's reaping by init, on a loaded box
$0A constant PROBE-MS              \ the pause between two probes of its pid

\ The engine under test: the gate's freshly built candidate when it exported
\ one, else the running engine itself (src/lib/engine-candidate.f).
: HB$ ( -- ptr u8 n )        ENGINE-CANDIDATE:PATH$ ;

: PROMPT$ ( -- ptr u8 n )    s" habu> " ;
: ANYWHERE$ ( -- ptr u8 n )  s" " ;      \ an empty head: the tail may be anywhere

\ The buffer, compaction, span search, wait clock and never-seen facts are
\ src/lib/pty-harness.f's. The reads and waits are this file's: the bytes come
\ through the supervisor's linear handle, which the module's descriptor reader
\ cannot take, and DEADLINE shows what was read before it throws, which the
\ harness's WAIT-AFTER-WITHIN does not.
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

\ A clock that ends while the target still holds its side is a deadline, not a
\ wrong answer, so it asserts nothing: the wait is named and E-PROC-TIMEOUT
\ thrown (TEST-WAIT:EXPIRED). The scope tears the target down and RUN lets the
\ throw through, so the engine's report is the last line on stderr.
: DEADLINE ( process-pty-handle ptr u8 n -- process-pty-handle ) {: name:ptr len:n :}
   READ-SO-FAR
   name len TEST-WAIT:EXPIRED ;

: WAIT-SHOW ( ptr u8 n ptr u8 n -- ) {: ha:ptr hu:n ta:ptr tu:n :}
   s" expected: " type ta tu type
   hu 0 > if s"  after " type ha hu type then
   s"  (wait budget " type WAIT-BUDGET-MS FMT:.INT s"  ms)" type cr ;

\ Read until the tail appears at or after the head, the target hangs up, or the
\ deadline passes; an empty head waits for the tail anywhere. A child answers in
\ as many pieces as the host's scheduling chooses (the line editor redraws on
\ every keystroke, a loaded box splits one echo across dozens of reads), so no
\ count of reads or quiet polls bounds the wait. The hang-up answers whether the
\ tail came before it.
: EXPECT-AFTER ( process-pty-handle ptr u8 n ptr u8 n -- process-pty-handle bool )
   {: ha:ptr hu:n ta:ptr tu:n :}
   WAIT-BUDGET-MS WAIT-OPEN
   begin
      ha hu ta tu AFTER? if true exit then
      WAIT-LEFT 0= if
         ha hu ta tu WAIT-SHOW
         s" process-pty-tty-smoke EXPECT-AFTER" DEADLINE
      then
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
         s" process-pty-tty-smoke DRAIN-ALL" DEADLINE
      then
      READ-STEP 0 < if exit then
   again ;

\ The target's exit within EXIT-MS. A target still running when that clock ends
\ has not exited YET, which is a deadline as well.
: EXPECT-EXIT ( process-pty-handle -- process-pty-handle )
   EXIT-MS PROCESS-PTY:AWAIT if exit then
   s" expected: the target's exit (exit budget " type EXIT-MS FMT:.INT s"  ms)" type cr
   s" process-pty-tty-smoke EXPECT-EXIT" DEADLINE ;

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

\ PIPE-STOPS: under WITH-TARGET a refused definition ends the child and the next
\ line is never evaluated (the batch contract).
: PIPE-STOPS-BODY ( process-pty-handle -- process-pty-handle )
   WATCH-RESET
   s" 42" WATCH+ {: answered:watch :}   \ the claim below outlives every compaction
   PROCESS-PTY:LAUNCH
   BAD-DEF$ TELL
   ARITH$ TELL
   PROCESS-PTY:END-INPUT
   s" non-certified definition: tty-bad" EXPECT!
   EXPECT-EXIT
   DRAIN-ALL
   answered NEVER-SEEN? TTRUE ;

: PIPE-STOPS ( -- )
   BUF-CLEAR
   HB$ [: PIPE-STOPS-BODY ;] PROCESS-PTY:WITH-TARGET
   s" PASS: batch REPL over pipes stops at the refused definition" type cr ;

\ TTY-RECOVERS: under WITH-TTY-TARGET the child refuses BAD-DEF$, answers
\ `6 7 * . cr` with 42 and prompts again.
: TTY-RECOVERS ( -- )
   BUF-CLEAR
   HB$ [:
      PROCESS-PTY:LAUNCH
      ANYWHERE$ SETTLE
      BAD-DEF$ TELL
      s" tty-bad" EXPECT!
      BUF-CLEAR
      ARITH$ TELL
      s" 42" PROMPT-AFTER!
      PROCESS-PTY:ALIVE? TTRUE
   ;] PROCESS-PTY:WITH-TTY-TARGET
   s" PASS: REPL on a pseudo-terminal refuses, recovers and prompts again" type cr ;

\ A normal checked structure needs no TRUSTED seed. The same constructor and
\ projection are refused as bare tokens and work inside a checked definition.
: TTY-LAYOUT-BODY ( process-pty-handle -- process-pty-handle )
   PROCESS-PTY:LAUNCH
   ANYWHERE$ SETTLE
   s" package PTYP public STRUCTURE point FIELD x n FIELD y n ;STRUCTURE : AT ( n n -- point ) PTYP-POINT:MAKE ; : FIRST ( point -- n ) PTYP-POINT:UNMAKE drop ; : FIRST-X ( -- n ) 2 3 AT FIRST ; ;package" TELL
   s"  ok" SETTLE
   s" 2 3 PTYP:AT PTYP:FIRST" TELL
   s" hb: interpret-mode layout value: PTYP:AT" SETTLE
   s" PTYP:FIRST-X . cr depth . cr" TELL
   S\" \r\n2\r\n" EXPECT!
   S\" \r\n0\r\n" PROMPT-AFTER!
   PROCESS-PTY:ALIVE? TTRUE ;

: TTY-LAYOUT ( -- )
   BUF-CLEAR
   HB$ [: TTY-LAYOUT-BODY ;] PROCESS-PTY:WITH-TTY-TARGET
   s" PASS: checked layout calculation works at the REPL after bare-layout refusal" type cr ;

\ An uncaught throw from inside run-in-stack reaches the tty REPL while the
\ callback's guarded allocation is active; run-in-stack takes only a real
\ guarded mapping (src/lib/memory.f MEM:ALLOC-GUARDED), so TTY-POOL is one.
\ Recovery must reinstate the REPL's own allocation: src/habu/habu1.f BDEPTH
\ subtracts the contents of the active base cell (src/habu/layout.f S0-CELL)
\ from XDS, so `depth` reads zero only when the cursor and that cell came back
\ together. The four values printed ahead of it would also fit inside the 64 KB
\ pool, so the zero identifies the stack. The pool's address lives in a declared
\ pointer cell (a raw cell never holds an address), named short: this line is
\ typed at the REPL, whose LBUF holds 256 bytes (src/habu/repl.f), and a longer
\ line loses its tail silently, failing on E-UNDEFINED much later.
: STACK-DEFS$ ( -- ptr u8 n )
   s" require src/lib/memory.f PTR-VARIABLE POOL-A STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED constant TTY-POOL-CAP POOL-A ! : TTY-POOL ( -- ptr u8 ) POOL-A @ ; : TTY-RAISE ( -- ) 7 throw ; : TTY-CROSS ( -- ) ['] TTY-RAISE TTY-POOL TTY-POOL-CAP run-in-stack ;" ;

: TTY-STACK-RECOVERS-BODY ( process-pty-handle -- process-pty-handle )
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
   PROCESS-PTY:ALIVE? TTRUE ;

: TTY-STACK-RECOVERS ( -- )
   BUF-CLEAR
   HB$ [: TTY-STACK-RECOVERS-BODY ;] PROCESS-PTY:WITH-TTY-TARGET
   s" PASS: REPL recovers its own stack allocation after an uncaught throw inside run-in-stack" type cr ;

\ The target starts a sleeper on its own terminal and prints 48879, then the
\ sleeper's pid. The sleeper leads a group of its own; it ends with the target,
\ and init reaps it.
: SLEEPER$ ( -- ptr u8 n )
   s\" PROC-ARGV-RESET s\" 30\" >LEN PROC-ARGV+ s\" /bin/sleep\" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PID>N $BEEF . . cr" ;

\ The number the target printed on the line after 48879, or -1 when there is
\ none.
: SLEEPER-PID ( -- n )
   0 S\" 48879\r\n" FIND-FROM {: at:n :}
   at 0 < if -1 exit then
   at 7 + {: from:n :}
   from S\" \r" FIND-FROM {: end:n :}
   end 0 < if -1 exit then
   BUF$ drop from + end from - STR>NUMBER? MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

\ Whether nobody has pid within ms: the null signal answers ESRCH. A pid at or
\ below 0 names a group or every process, so it is never probed and answers
\ false. A pid still taken when the clock ends is this test's own deadline, not
\ an answer: the wait names itself and throws E-PROC-TIMEOUT. TREE-ENDS has torn
\ the target down before it waits, so nothing is left open when it throws.
: GONE-WITHIN? ( n n -- bool )
   {: pid:n ms:n :}
   pid 0 <= if false exit then
   ms WAIT-OPEN
   begin
      pid 0 kill-errno ESRCH# negate = if true exit then
      WAIT-LEFT 0= if s" process-pty-tty-smoke GONE-WITHIN" TEST-WAIT:EXPIRED then
      PROBE-MS >MS TASK:SLEEP
   again ;

\ TREE-ENDS holds the scope's teardown to what the target started.
: TREE-ENDS-BODY ( process-pty-handle -- n process-pty-handle )
   PROCESS-PTY:LAUNCH
   ANYWHERE$ SETTLE
   s" require src/lib/process-argv.f" TELL
   s"  ok" SETTLE
   SLEEPER$ TELL
   s" 48879" PROMPT-AFTER!
   SLEEPER-PID swap ;

: TREE-ENDS ( -- )
   BUF-CLEAR
   HB$ [: TREE-ENDS-BODY ;] PROCESS-PTY:WITH-TTY-TARGET {: pid:n :}
   pid 0 > TTRUE
   pid GONE-MS GONE-WITHIN? TTRUE
   s" PASS: teardown ends what the target started" type cr ;

\ The prompt barrier itself: the FIRST prompt in the buffer the barrier ended on
\ is one the editor redrew before the answer, which a bare `s" habu> " EXPECT!`
\ takes; the barrier's prompt is a later one only the finished line could print.
: ECHO-PROMPT-REJECTED-BODY ( process-pty-handle -- process-pty-handle )
   PROCESS-PTY:LAUNCH
   ANYWHERE$ SETTLE
   ARITH$ TELL
   s" 42" PROMPT-AFTER!
   0 PROMPT$ FIND-FROM {: echo-at:n :}
   0 s" 42" FIND-FROM {: ans-at:n :}
   echo-at 0 >= TTRUE                      \ a bare prompt wait had a prompt to take
   echo-at ans-at < TTRUE                  \ and it was the editor's, ahead of the answer
   s" 42" PROMPT$ FIND-AFTER ans-at > TTRUE   \ the barrier took one that follows it
   PROCESS-PTY:ALIVE? TTRUE ;

: ECHO-PROMPT-REJECTED ( -- )
   BUF-CLEAR
   HB$ [: ECHO-PROMPT-REJECTED-BODY ;] PROCESS-PTY:WITH-TTY-TARGET
   s" PASS: the prompt barrier is not answered by the echo's own prompt" type cr ;

: CASES ( -- )
   PIPE-STOPS
   TTY-RECOVERS
   ECHO-PROMPT-REJECTED
   TTY-LAYOUT
   TTY-STACK-RECOVERS
   TREE-ENDS ;

public

\ AWAIT and AWAIT-BYTES wait on the AIO loop, so the loop runs for the body of
\ RUN: it is started after the last definition. It stops however the cases
\ end, so a deadline's throw (DEADLINE) passes on with no loop running.
: RUN ( -- )
   T-RESET
   AIO:START
   [: CASES ;] [: AIO:STOP ;] finally
   T-REPORT
   s" process-pty-tty-smoke: ok" type cr ;

;using

;package

PTY-TTY-SMOKE:RUN
