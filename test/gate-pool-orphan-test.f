\ gate-pool-orphan-test.f - pool children die on parent death: the production
\ death-pipe reaper kills a spawned child's group when its pool parent is
\ SIGKILLed, the case the pool's own GT-POOL-KILL-ALL cleanup never reaches.
\ Every wait has a hard deadline so a broken mechanism fails instead of hanging.

\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/memory.f
\ src/lib/process.f src/lib/process-fork.f test/gate-pool-orphan-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/process.f
require src/lib/process-fork.f

300 constant GPO-SETTLE-MS
100 constant GPO-POLL-MS
50 constant GPO-OBSERVE-STEPS
200 constant GPO-IDLE-MS
150 constant GPO-IDLE-STEPS

create GPO-SLEEP-PFD 8 allot

: GPO-SLEEP ( n -- ) {: ms:n :}
   GPO-SLEEP-PFD 0 ms poll drop ;

\ Bounded idle: block in poll steps up to a hard cap so a child that is never
\ reaped (mechanism broken) still self-terminates instead of leaking forever.
: GPO-IDLE ( -- )
   0 begin dup GPO-IDLE-STEPS < while
      GPO-IDLE-MS GPO-SLEEP
      1+
   repeat drop ;

: GPO-EXIT ( n -- )
   s" " rot die ;

\ A child here runs until it is killed. One that cannot fork what its part
\ needs - its reaper, or the child it watches - must not throw: that would
\ unwind its copy of this test's stack and run the test on in the child. It
\ exits GPO-REFUSED-RC instead, and its parent reports that status.
3 constant GPO-REFUSED-RC

: GPO-REFUSED ( n -- ) {: code:n :}
   s" gate-pool-orphan-test: a child ended on throw " type code .
   GPO-REFUSED-RC GPO-EXIT ;

: GPO-KILLED? ( PROCESS:status -- bool )
   MATCH PROCESS:status
     exited OF
        s" gate-pool-orphan-test: a child exited before it was killed, code " type .
        false
     ENDOF
     signaled OF drop true ENDOF
   ;MATCH ;

\ Observe a child's death via alive-pipe EOF within a hard deadline.
\ PROCESS:POLL-IN reports the read end ready (POLLHUP) once its last write end
\ closes; nobody ever writes AW, so a ready poll can only mean EOF.
: GPO-OBSERVE-DEAD? ( fd -- bool ) {: ar:fd :}
   0 begin dup GPO-OBSERVE-STEPS < while
      ar GPO-POLL-MS >MS PROCESS:POLL-IN COUNT>N 0 > if
         drop 0 0= exit
      then
      1+
   repeat drop 1 0= ;

\ ---- spawned-child co-located reaper (PROC-FORK:SPAWN-REAPER) ----------------
\ A spawned pool child is not in the pool parent's group, so SPAWN-REAPER forks
\ a reaper R that joins the child's group (setpgid(0,childpid)) and watches the
\ pool-death read end. T forks P (holds its write end WR); P forks the hanging
\ child C as its own group leader and arms R; C keeps only the alive-pipe write
\ end AW. T SIGKILLs P -> R reads EOF -> R SIGKILLs C's group; T sees C's death
\ as AW's EOF.
: GSR-CHILD ( fd fd fd fd -- ) {: rd:fd wr:fd ar:fd aw:fd :}
   rd FD>N close
   wr FD>N close
   ar FD>N close
   GPO-IDLE
   0 GPO-EXIT ;

\ Stack-preserving under catch: the watched fd and the child are the window.
: GSR-ARM ( fd pid -- fd pid ) {: rd:fd cpid:pid :}
   rd cpid PROC-FORK:SPAWN-REAPER drop
   rd cpid ;

\ No reaper watches C once P's arm is refused, so P ends C before it exits.
: GSR-REFUSED ( n pid -- ) {: code:n cpid:pid :}
   cpid PROCESS:SIGKILL PROCESS:KILL-RAW drop
   cpid PID>N wait-status drop
   code GPO-REFUSED ;

\ P makes C its own group leader before arming, so the reaper's setpgid join
\ always succeeds. A refused fork of C ends P first: a negative pid must never
\ reach the kill.
: GSR-PARENT ( fd fd fd fd -- ) {: rd:fd wr:fd ar:fd aw:fd :}
   PROC-FORK:RAW {: cpid:pid :}
   cpid PID>N 0 < if E-PROC-SPAWN GPO-REFUSED then
   cpid PID>N 0= if rd wr ar aw GSR-CHILD then
   cpid cpid PROC-FORK:SET-PGID drop
   ar FD>N close
   aw FD>N close
   rd cpid [: GSR-ARM ;] catch {: arm:n :}
   2drop
   arm 0<> if arm cpid GSR-REFUSED then
   rd FD>N close
   GPO-IDLE
   0 GPO-EXIT ;

: GSR-RUN ( -- bool )
   PROC-FORK:DEATH-PIPE {: rd:fd wr:fd :}
   FD:PIPE-PAIR {: ar:fd aw:fd :}
   PROC-FORK:CHECKED {: ppid:pid :}
   ppid PID>N 0= if rd wr ar aw GSR-PARENT then
   rd FD>N close
   wr FD>N close
   aw FD>N close
   GPO-SETTLE-MS GPO-SLEEP
   ppid PROCESS:SIGKILL PROCESS:KILL-RAW drop
   ppid PROCESS:WAIT GPO-KILLED?
   ar GPO-OBSERVE-DEAD? and
   ar FD>N close ;

: GPO-MAIN ( -- )
   T-RESET
   s" spawned pool child + reaper reaped when parent is SIGKILLed" T-LABEL
   GSR-RUN TTRUE
   T-REPORT
   s" gate-pool-orphan-test: ok" type cr ;

GPO-MAIN
