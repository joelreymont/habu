\ process-pty-io-smoke.f - focused proof of the checked PTY process supervisor
\ (lib/process-pty-io.f) built on the landed linear handle registry.
\
\ Proves, through the supervisor's public PROCESS-PTY:* lifecycle:
\   - a spawned short-lived child is watched to exit and the watch fires;
\   - signal 0 on the live (gated, pre-exec) target reports 0 (exists), and after
\     teardown reaps it the same signal reports -ESRCH (gone), so the supervisor
\     distinguishes existence from -EPERM;
\   - teardown erases every handle field exactly once (a clean TEARDOWN-DONE is the
\     registry's own proof it drained) and frees the slot, so more spawn/teardown
\     cycles than the registry has slots keep succeeding (registry balance);
\   - the checker rejects duplicating a handle or teardown token, so a double
\     teardown cannot even be written.
\
\ The pipe-I/O supervisor itself has no OS-divergent code; the one place a
\ lifetime watch DOES diverge by backend is opening it on an already-exited
\ process, which is exactly why the supervisor opens the watch while the target is
\ still gated-alive. That divergence is expressed here, gated exactly like
\ test/proc-watch-smoke.f, to document the design.
\
\ Run: bin/hb --load test/process-pty-io-smoke.f

require lib/process-pty-io.f
require lib/prelude.f
require lib/test.f
require test/checker-assert.f

package PTY-IO-SMOKE

0 constant SIG-PROBE               \ the null signal: probe existence, deliver nothing
3 constant ESRCH#                  \ "no such process" (POSIX, identical Linux/macOS)
$1388 constant DEADLINE-MS         \ 5s ceiling for the live->dead watch transition
20 constant CYCLES                 \ > PROCESS-PTY slot capacity (16): proves each teardown frees its slot

create WAIT-BYTE 1 allot
variable TGT-PID
variable ALIVE-R   variable ALIVE-W       \ alive-pipe: child holds write end, closes on exit

\ A real short-lived executable available on both OSes.
: TRUE-PATH ( -- ptr u8 len )
   s" /usr/bin/true" >LEN ;

: SAVE-TGT ( pid -- )
   PID>N TGT-PID ! ;

: CLOSE-OK ( fd -- ) {: f:fd :}
   f FD>N 0 < if exit then
   f FD>N close ;

\ ---- full supervised lifecycle ----------------------------------------------
\ Spawn gated, probe the live target, release it, watch the exit, remember the
\ target pid, tear down (which reaps), then confirm the reaped pid is gone.
: LIFECYCLE ( -- )
   TRUE-PATH PROCESS-PTY:SPAWN
   PROCESS-PTY:ALIVE? TTRUE
   PROCESS-PTY:LAUNCH
   DEADLINE-MS PROCESS-PTY:AWAIT TTRUE
   PROCESS-PTY:TARGET SAVE-TGT
   PROCESS-PTY:TEARDOWN
   TGT-PID @ SIG-PROBE kill-errno ESRCH# negate T= ;

\ ---- registry balance -------------------------------------------------------
: CYCLE ( -- )
   TRUE-PATH PROCESS-PTY:SPAWN
   PROCESS-PTY:LAUNCH
   DEADLINE-MS PROCESS-PTY:AWAIT drop
   PROCESS-PTY:TEARDOWN ;

\ More cycles than the registry has slots: if any teardown leaked its slot the
\ later spawns would throw E-PROC-PTY-CAPACITY, so completing the loop proves
\ every teardown balanced.
: BALANCE ( -- )
   0 begin dup CYCLES < while
      CYCLE
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
   PROC-FORK dup PID>N 0= if drop DEAD-CHILD then ;

: DEAD-WATCH ( -- )
   PIPE-PAIR ALIVE-W ! ALIVE-R !
   FORK-DEAD {: cpid:pid :}
   ALIVE-W @ >FD CLOSE-OK
   ALIVE-R @ >FD FD>N WAIT-BYTE 1 read 0 T=       \ block until the child has exited (EOF)
   ALIVE-R @ >FD CLOSE-OK
   cpid PID>N proc-watch-open {: wfd:n :}
   HB-TARGET-LINUX? if
      wfd 0 >= TTRUE                               \ Linux pidfd_open opens on the surviving zombie
      wfd 0 >= if wfd close then
   else
      wfd 0 < TTRUE                                \ macOS kqueue cannot register on the exited process
   then
   cpid PROC-WAIT-STATUS drop ;                    \ reap the zombie

: RUN ( -- )
   T-RESET
   LIFECYCLE
   BALANCE
   STATIC
   DEAD-WATCH
   T-REPORT
   s" process-pty-io-smoke: ok" type cr ;

RUN

;package
