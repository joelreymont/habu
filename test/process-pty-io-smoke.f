\ process-pty-io-smoke.f - the checked PTY process supervisor
\ (src/lib/process-pty-io.f) through its public PROCESS-PTY:* lifecycle.

require src/lib/process-pty-io.f
require src/lib/prelude.f
require src/lib/aio.f
require src/lib/test.f
require src/lib/test/wait.f
require src/lib/task.f

package PTY-IO-SMOKE

0 constant SIG-PROBE               \ the null signal: probe existence, deliver nothing
3 constant ESRCH#                  \ "no such process" (POSIX, identical Linux/macOS)
20 constant CYCLES                 \ > PROCESS-PTY slot capacity (16): proves each teardown frees its slot
-7001 constant BODY-THROW          \ a scope body's own throw code, no library's

create WAIT-BYTE 1 allot
$200 constant FDS-CAP              \ room for one `ls /dev/fd` listing
create FDS-BASE FDS-CAP allot      \ the listing with no target alive
create FDS-SEEN FDS-CAP allot      \ the listing while a target is gated
create FDS-ERR FDS-CAP allot
variable FDS-BASE-U
create SIGNAL-BYTES 64 allot
variable TGT-PID
variable ALIVE-R   variable ALIVE-W       \ alive-pipe: child holds write end, closes on exit
variable IN-SCOPE                         \ set by the halted task's body once its target is up
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
\ Teardown reaps: the target's pid then answers the null signal with -ESRCH
\ (gone), which the supervisor tells apart from -EPERM.
: LIFECYCLE ( -- )
   TRUE-PATH [:
      PROCESS-PTY:ALIVE? TTRUE
      PROCESS-PTY:LAUNCH
      -1 PROCESS-PTY:AWAIT TTRUE
      PROCESS-PTY:TARGET SAVE-TGT
   ;] PROCESS-PTY:WITH-TARGET
   TGT-PID @ SIG-PROBE kill-errno ESRCH# negate T= ;

\ ---- registry balance -------------------------------------------------------
\ Teardown erases every handle field exactly once; a clean TEARDOWN-DONE is the
\ registry's own proof it drained.
: CYCLE ( -- )
   TRUE-PATH [:
      PROCESS-PTY:LAUNCH
      -1 PROCESS-PTY:AWAIT drop
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
   [: IN-SCOPE atomic@ 0<> HALTEE TASK:DONE? or ;] TEST-WAIT:POLL
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
\ Opening a lifetime watch on an exited process diverges by backend, so the
\ supervisor opens it while the target is still gated and alive.
: DEAD-CHILD ( -- )
   ALIVE-R @ >FD CLOSE-OK             \ child drops the read end; holds only the write end
   s" " 0 die ;                        \ exit closes the write end -> parent read EOFs

: FORK-DEAD ( -- pid )
   PROC-FORK:CHECKED dup PID>N 0= if drop DEAD-CHILD then ;

: DEAD-WATCH ( -- )
   FD:PIPE-PAIR ALIVE-W ! ALIVE-R !
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
   cpid PROCESS:WAIT drop ;                           \ reap the zombie

\ A gated target stays alive beyond the 100 ms deadline. A 1 kHz profiler storms
\ the process while its watch wait is on the AIO loop; no signal reaches a
\ thread parked in poll(2), so the wait must neither throw, shorten nor renew
\ time.
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

\ ---- no other spawn inherits the target's descriptors -------------------------
\ `ls /dev/fd` lists its open descriptors, its own directory's among them. While
\ a target is gated, its listing is compared with a baseline listing taken with
\ no target alive: any pipe or terminal end the scope opened and the probe
\ inherited would be an extra entry.
: OPEN-FDS ( ptr u8 -- n ) {: buf:ptr :}
   PROC-ARGV-RESET
   s" /dev/fd" >LEN PROC-ARGV+
   s" /bin/ls" >LEN buf FDS-CAP >LEN FDS-ERR FDS-CAP >LEN
   PROCESS:WAIT-FOREVER RUN-ARGV-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE drop LEN>N ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         s" ls /dev/fd exits cleanly" T-LABEL
         rc RC>N 0 T=
         outu LEN>N
      ENDOF
   ;MATCH ;

: NO-LEAK-BODY ( process-pty-handle -- process-pty-handle )
   FDS-SEEN OPEN-FDS {: n:n :}
   s" a spawn beside a gated target inherits none of its descriptors" T-LABEL
   FDS-SEEN n FDS-BASE FDS-BASE-U @ T$= ;

: NO-LEAK ( -- )
   FDS-BASE OPEN-FDS FDS-BASE-U !
   TRUE-PATH [: NO-LEAK-BODY ;] PROCESS-PTY:WITH-TARGET
   TRUE-PATH [: NO-LEAK-BODY ;] PROCESS-PTY:WITH-TTY-TARGET ;

\ AWAIT with the loop stopped is E-AIO-STATE, like every other wait on the loop;
\ src/lib/serial-test.f and src/lib/signal-test.f pin that
\ refusal, and this suite does not.
\
\ The loop is started after the last definition and stopped at the end.
: RUN ( -- )
   T-RESET
   AIO:START
   LIFECYCLE
   BALANCE
   THROW-FREES
   HALT-FREES
   STATIC
   DEAD-WATCH
   SIGNAL-WATCH
   SIGNAL-OUTPUT
   NO-LEAK
   AIO:STOP
   T-REPORT
   s" process-pty-io-smoke: ok" type cr ;

RUN

;package
