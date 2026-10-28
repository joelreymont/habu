\ process-pty-io-smoke.f - focused proof of the checked PTY process supervisor
\ (src/lib/process-pty-io.f) built on the landed linear handle registry.
\
\ Proves, through the supervisor's public PROCESS-PTY:* lifecycle:
\   - a spawned short-lived child is watched to exit and the watch fires;
\   - signal 0 on the live (gated, pre-exec) target reports 0 (exists), and after
\     teardown reaps it the same signal reports -ESRCH (gone), so the supervisor
\     distinguishes existence from -EPERM;
\   - teardown erases every handle field exactly once (a clean TEARDOWN-DONE is the
\     registry's own proof it drained) and frees the slot, so more scopes than the
\     registry has slots keep succeeding (registry balance);
\   - a body that throws still tears its target down: the throw reaches the
\     caller, the target's pid is gone and the slot is free, for more throwing
\     scopes than the registry has slots;
\   - a task halted inside a scope tears its target down as the halt unwinds the
\     scope, for more halted tasks than the registry has slots;
\   - a target whose owner dies inside the scope, before LAUNCH, exits on its
\     own, so no gated target outlives its owner under init;
\   - the checker rejects duplicating a handle or teardown token, so a double
\     teardown cannot even be written.
\
\ The pipe-I/O supervisor itself has no OS-divergent code; the one place a
\ lifetime watch DOES diverge by backend is opening it on an already-exited
\ process, which is exactly why the supervisor opens the watch while the target is
\ still gated-alive. That divergence is expressed here to document the design.
\
\ Run: bin/hb --load test/process-pty-io-smoke.f

require src/lib/process-pty-io.f
require src/lib/prelude.f
require src/lib/aio.f
require src/lib/test.f
require src/lib/test/wait.f
require src/lib/task.f

package PTY-IO-SMOKE

0 constant SIG-PROBE               \ the null signal: probe existence, deliver nothing
3 constant ESRCH#                  \ "no such process" (POSIX, identical Linux/macOS)
$1388 constant DEADLINE-MS         \ 5s ceiling for the live->dead watch transition
20 constant CYCLES                 \ > PROCESS-PTY slot capacity (16): proves each teardown frees its slot
-7001 constant BODY-THROW          \ a scope body's own throw code, no library's

create WAIT-BYTE 1 allot
create SIGNAL-BYTES 64 allot
variable TGT-PID
variable ALIVE-R   variable ALIVE-W       \ alive-pipe: child holds write end, closes on exit
variable IN-SCOPE                         \ set by the halted task's body once its target is up
variable STRAND-R  variable STRAND-W      \ stranded-target pipe: the dying owner sends its target's pid
create STRAND-PID 8 allot                 \ that pid, as the pipe carries it
TASK:MIN-STACK TASK:TASK HALTEE

\ A real short-lived executable available on both OSes.
: TRUE-PATH ( -- ptr u8 n )
   s" /usr/bin/true" ;

: SAVE-TGT ( pid -- )
   PID>N TGT-PID ! ;

: CLOSE-OK ( fd -- ) {: f:fd :}
   f FD>N 0 < if exit then
   f FD>N close ;

\ ---- full supervised lifecycle ----------------------------------------------
\ Spawn gated, probe the live target, release it, watch the exit, remember the
\ target pid; the scope's end tears down (which reaps), then the reaped pid is
\ confirmed gone.
: LIFECYCLE ( -- )
   TRUE-PATH [:
      PROCESS-PTY:ALIVE? TTRUE
      PROCESS-PTY:LAUNCH
      DEADLINE-MS PROCESS-PTY:AWAIT 0= if
         s" process-pty-io-smoke LIFECYCLE" TEST-WAIT:EXPIRED
      then
      PROCESS-PTY:TARGET SAVE-TGT
   ;] PROCESS-PTY:WITH-TARGET
   TGT-PID @ SIG-PROBE kill-errno ESRCH# negate T= ;

\ ---- registry balance -------------------------------------------------------
: CYCLE ( -- )
   TRUE-PATH [:
      PROCESS-PTY:LAUNCH
      DEADLINE-MS PROCESS-PTY:AWAIT drop
   ;] PROCESS-PTY:WITH-TARGET ;

\ More cycles than the registry has slots: if any teardown leaked its slot the
\ later spawns would throw E-PROC-PTY-CAPACITY, so completing the loop proves
\ every teardown balanced.
: BALANCE ( -- )
   0 begin dup CYCLES < while
      CYCLE
      1+
   repeat drop ;

\ ---- a throwing body ---------------------------------------------------------
\ The body throws with its target still gated, before LAUNCH. The scope passes
\ the throw on, and by then has reaped the target and freed its slot: a leaked
\ slot would turn a later scope's throw into E-PROC-PTY-CAPACITY.
: THROW-BODY ( process-pty-handle -- process-pty-handle )
   PROCESS-PTY:TARGET SAVE-TGT
   BODY-THROW throw ;

: THROWN ( -- )
   [: TRUE-PATH [: THROW-BODY ;] PROCESS-PTY:WITH-TARGET ;] BODY-THROW TTHROWSQ
   TGT-PID @ SIG-PROBE kill-errno ESRCH# negate T= ;

: THROW-FREES ( -- )
   0 begin dup CYCLES < while
      THROWN
      1+
   repeat drop ;

\ ---- a task halted inside the scope ----------------------------------------
\ The task's body parks in its scope with the target still gated and alive.
\ TASK:KILL halts it at TASK:PAUSE, which unwinds the task through the scope's
\ cleanup; that must reap the target and free its slot, or a later round's spawn
\ throws E-PROC-PTY-CAPACITY.
: PARK-BODY ( process-pty-handle -- process-pty-handle )
   PROCESS-PTY:TARGET SAVE-TGT
   1 IN-SCOPE atomic!
   begin TASK:PAUSE again ;

: PARKED ( -- )
   TRUE-PATH [: PARK-BODY ;] PROCESS-PTY:WITH-TARGET ;

: HALTED ( -- )
   0 IN-SCOPE atomic!
   ['] PARKED HALTEE TASK:ACTIVATE
   [: IN-SCOPE atomic@ 0<> HALTEE TASK:DONE? or ;]
   mono-ns DEADLINE-MS 1000000 * + s" process-pty-io-smoke HALTED" TEST-WAIT:POLL
   HALTEE TASK:KILL
   HALTEE TASK:THROW@ 0 T=
   TGT-PID @ SIG-PROBE kill-errno ESRCH# negate T= ;

: HALT-FREES ( -- )
   0 begin dup CYCLES < while
      HALTED
      1+
   repeat drop ;

\ ---- static double-teardown rejection ---------------------------------------
\ The handle and teardown tokens are linear (noncopyable); duplicating either is a
\ checker rejection, so a second teardown of the same token cannot be written.
: STATIC ( -- )
   s" PIS-DUP-HANDLE ( process-pty-handle -- process-pty-handle process-pty-handle ) dup"
   CHECK-QUIET-CANDIDATE! 0 T=
   s" PIS-DUP-TEARDOWN ( process-pty-teardown -- process-pty-teardown process-pty-teardown ) dup"
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- OS-divergent watch-open-on-dead (the reason spawn gates) ---------------
: DEAD-CHILD ( -- )
   ALIVE-R @ >FD CLOSE-OK             \ child drops the read end; holds only the write end
   s" " 0 die ;                        \ exit closes the write end -> parent read EOFs

: FORK-DEAD ( -- pid )
   PROC-FORK:CHECKED dup PID>N 0= if drop DEAD-CHILD then ;

: DEAD-WATCH ( -- )
   PIPE-PAIR ALIVE-W ! ALIVE-R !
   FORK-DEAD {: cpid:pid :}
   ALIVE-W @ >FD CLOSE-OK
   ALIVE-R @ >FD FD>N WAIT-BYTE 1 read 0 T=       \ block until the child has exited (EOF)
   ALIVE-R @ >FD CLOSE-OK
   cpid PID>N proc-watch-open {: wfd:n :}
   HB-TARGET-LINUX-KERNEL? if
      wfd 0 >= TTRUE                               \ Linux pidfd_open opens on the surviving zombie
      wfd 0 >= if wfd close then
   else
      wfd 0 < TTRUE                                \ macOS kqueue cannot register on the exited process
   then
   cpid PROC-WAIT-STATUS drop ;                    \ reap the zombie

\ ---- a target stranded before LAUNCH ----------------------------------------
\ A forked copy of this process opens a scope and dies inside its body, so the
\ scope's teardown never runs and its target is stranded gated. Nothing signals
\ the target: it must read end of file on its gate, exit and be reaped by init,
\ leaving its pid gone. An expired wait kills it, so a red row leaves no orphan.
\ An owner that dies before it sends a pid fails the row without the wait.
: STRAND-BODY ( process-pty-handle -- process-pty-handle )
   PROCESS-PTY:TARGET PID>N STRAND-PID !
   STRAND-W @ >FD FD>N STRAND-PID 8 write drop
   s" " 0 die ;

: STRAND-CHILD ( -- )
   [: TRUE-PATH [: STRAND-BODY ;] PROCESS-PTY:WITH-TARGET ;] PROC-FORK:CHILD-MAIN ;

: STRAND-GONE? ( -- bool )
   STRAND-PID @ SIG-PROBE kill-errno ESRCH# negate = ;

: STRAND-WAIT ( -- )
   ['] STRAND-GONE? mono-ns DEADLINE-MS 1000000 * +
   s" process-pty-io-smoke STRANDED" TEST-WAIT:POLL ;

\ A pid at or below 1 names no single process: kill(0) and kill(-1) reach groups.
: STRAND-KILL ( -- )
   STRAND-PID @ 1 > if STRAND-PID @ SIGKILL kill-errno drop then ;

: STRAND-CHECK ( -- )
   ['] STRAND-WAIT catch dup 0<> if STRAND-KILL throw then drop ;

: STRANDED ( -- )
   PIPE-PAIR STRAND-W ! STRAND-R !
   PROC-FORK:CHECKED dup PID>N 0= if drop STRAND-CHILD then {: cpid:pid :}
   STRAND-W @ >FD CLOSE-OK
   STRAND-R @ >FD FD>N STRAND-PID 8 read {: got:n :}
   STRAND-R @ >FD CLOSE-OK
   cpid PROC-WAIT-STATUS drop
   got 8 T=
   got 8 = if STRAND-CHECK then ;

\ A gated target stays alive beyond the 100 ms deadline. A 1 kHz profiler storms
\ the process while its watch wait is on the AIO loop; a signal reaches no
\ thread parked in poll(2) any more, so it must neither throw, shorten the wait
\ nor renew time.
: SIGNAL-WATCH-BODY ( process-pty-handle -- process-pty-handle )
   1000 prof-rate 1000000 prof-on
   mono-ns {: started:n :}
   100 PROCESS-PTY:AWAIT {: ready:bool :}
   mono-ns started - {: elapsed:n :}
   prof-off
   ready TFALSE
   elapsed 90000000 >= TTRUE
   elapsed 1000000000 < TTRUE ;

: SIGNAL-WATCH ( -- )
   PROC-ARGV-RESET
   TRUE-PATH [: SIGNAL-WATCH-BODY ;] PROCESS-PTY:WITH-TARGET ;

\ Drive the same storm through the terminal's data descriptor, first a quiet
\ deadline, then real bytes. No input is written to echo back.
: SIGNAL-OUTPUT-BODY ( process-pty-handle -- process-pty-handle )
   PROCESS-PTY:LAUNCH
   1000 prof-rate 1000000 prof-on
   mono-ns {: started:n :}
   SIGNAL-BYTES 64 100 PROCESS-PTY:AWAIT-BYTES {: quiet:n :}
   mono-ns started - {: elapsed:n :}
   \ A negative timeout is POLL's own unbounded wait, signals or not.
   SIGNAL-BYTES 64 -1 PROCESS-PTY:AWAIT-BYTES {: got:n :}
   prof-off
   quiet 0 T=
   elapsed 90000000 >= TTRUE
   elapsed 1000000000 < TTRUE
   SIGNAL-BYTES got s" habu-pty-eintr" T$= ;

: SIGNAL-OUTPUT ( -- )
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   s" sleep 0.3; printf %s habu-pty-eintr" >LEN PROC-ARGV+
   s" /bin/sh" [: SIGNAL-OUTPUT-BODY ;] PROCESS-PTY:WITH-TTY-TARGET ;

\ AWAIT with the loop stopped is E-AIO-STATE, like every other wait on the loop;
\ src/lib/serial-test.f, src/lib/pty-test.f and src/lib/signal-test.f pin that
\ refusal, and this suite does not.
\
\ The loop is started after the last definition (a live task forbids
\ compilation) and stopped at the end.
: RUN ( -- )
   T-RESET
   AIO:START
   LIFECYCLE
   BALANCE
   THROW-FREES
   HALT-FREES
   STATIC
   DEAD-WATCH
   STRANDED
   SIGNAL-WATCH
   SIGNAL-OUTPUT
   AIO:STOP
   T-REPORT
   s" process-pty-io-smoke: ok" type cr ;

RUN

;package
