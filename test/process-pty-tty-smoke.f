\ process-pty-tty-smoke.f - the supervisor's pseudo-terminal mode and its public
\ write/await surface, driven through PROCESS-PTY:WRITE-LINE / AWAIT-BYTES from
\ outside the package.
\ Run: bin/hb --load test/process-pty-tty-smoke.f   (HABU_UNDER_TEST names the child)

require src/lib/error.f
require src/lib/fmt.f
require src/lib/process-pty-io.f
require src/lib/pty-harness.f
require src/lib/prelude.f
require src/lib/string.f
require src/lib/aio.f
require src/lib/test.f
require src/lib/task.f                 \ TASK:SLEEP between probes of a sleeper's pid
require src/lib/engine-candidate.f

package PTY-TTY-SMOKE

using PTY-HARNESS

$64 constant STEP-MS               \ one poll blocks at most this long
$0A constant PROBE-MS              \ the pause between two probes of its pid

\ The engine under test: the gate's freshly built candidate when it exported
\ one, else the running engine itself (src/lib/engine-candidate.f).
: HB$ ( -- ptr u8 n )        ENGINE-CANDIDATE:PATH$ ;

: PROMPT$ ( -- ptr u8 n )    s" habu> " ;
: ANYWHERE$ ( -- ptr u8 n )  s" " ;      \ an empty head: the tail may be anywhere

\ The buffer, compaction, span search and never-seen facts are
\ src/lib/pty-harness.f's. The reads and waits are this file's: the bytes come
\ through the supervisor's linear handle, which the module's descriptor reader
\ cannot take.
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

: WAIT-SHOW ( ptr u8 n ptr u8 n -- ) {: ha:ptr hu:n ta:ptr tu:n :}
   s" expected: " type ta tu type
   hu 0 > if s"  after " type ha hu type then
   cr ;

\ Read until the tail appears at or after the head or the target hangs up; an
\ empty head waits for the tail anywhere. A child answers in
\ as many pieces as the host's scheduling chooses (the line editor redraws on
\ every keystroke, a loaded box splits one echo across dozens of reads), so no
\ count of reads or quiet polls bounds the wait. The hang-up answers whether the
\ tail came before it.
: EXPECT-AFTER ( process-pty-handle ptr u8 n ptr u8 n -- process-pty-handle bool )
   {: ha:ptr hu:n ta:ptr tu:n :}
   begin
      ha hu ta tu AFTER? if true exit then
      READ-STEP {: n:n :}
      n 0 < if ha hu ta tu AFTER? exit then
   again ;

\ After the target exited, read everything it left behind until the hang-up.
: DRAIN-ALL ( process-pty-handle -- process-pty-handle )
   begin
      READ-STEP 0 < if exit then
   again ;

\ The target's exit. A watch that answers with no exit fails the case.
: EXPECT-EXIT ( process-pty-handle -- process-pty-handle )
   -1 PROCESS-PTY:AWAIT TTRUE ;

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
   s" E-LAYOUT-VALUE habu: interpret-mode layout value: PTYP:AT" SETTLE
   s" PTYP:FIRST-X . cr depth . cr" TELL
   S\" \r\n2\r\n" EXPECT!
   S\" \r\n0\r\n" PROMPT-AFTER!
   PROCESS-PTY:ALIVE? TTRUE ;

: TTY-LAYOUT ( -- )
   BUF-CLEAR
   HB$ [: TTY-LAYOUT-BODY ;] PROCESS-PTY:WITH-TTY-TARGET
   s" PASS: checked layout calculation works at the REPL after bare-layout refusal" type cr ;

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

\ Waits until nobody has pid: the null signal answers ESRCH. A pid at or below
\ 0 names a group or every process, so it is never probed.
: AWAIT-GONE ( n -- )
   {: pid:n :}
   pid 0 <= if exit then
   begin
      pid 0 kill-errno PROCESS:ESRCH# negate <>
   while
      PROBE-MS >MS TASK:SLEEP
   repeat ;

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
   pid AWAIT-GONE
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
   TREE-ENDS ;

public

\ AWAIT and AWAIT-BYTES wait on the AIO loop, so the loop runs for the body of
\ RUN: it is started after the last definition. It stops however the cases
\ end, so a case's throw passes on with no loop running.
: RUN ( -- )
   T-RESET
   AIO:START
   [: CASES ;] [: AIO:STOP ;] finally
   T-REPORT
   s" process-pty-tty-smoke: ok" type cr ;

;using

;package

PTY-TTY-SMOKE:RUN
