\ gate-pool-orphan-test.f - regression: pool children die on parent death.
\
\ Proves the death-pipe reaper design that fixes forked-pool-worker orphans: a
\ worker in its own process group forks a reaper that watches its parent's
\ death-pipe write end. When the watched parent is SIGKILLed - the exact case
\ where the pool's own GT-POOL-KILL-ALL cleanup can never run - the reaper reaps
\ the worker's group, so no orphan keeps spinning. Built only from
\ pipe/read/close/kill/setpgid, so it is portable (no PR_SET_PDEATHSIG / kqueue).
\ The mechanism is defined here (GPO-*) rather than promoted to lib/process-fork.f
\ until the live pool wiring lands (habu-reap-spawned-pool-fc4dc468), so this
\ regression pins the design independently.
\
\ Topology (T = this test):
\   T forks P (holds the death-pipe write end WR) and W (its own group).
\   W forks R (reaper, in W's group): R closes every fd but RD and blocks on it.
\   T SIGKILLs P -> WR closes -> R reads EOF -> R SIGKILLs W's group.
\   T observes W's death through an alive-pipe EOF (immediate, no zombie wait),
\   all within a hard deadline so a broken mechanism FAILS instead of hanging.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/process.f lib/process-fork.f test/gate-pool-orphan-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/process.f
require lib/process-fork.f

300 constant GPO-SETTLE-MS
100 constant GPO-POLL-MS
50 constant GPO-OBSERVE-STEPS
200 constant GPO-IDLE-MS
150 constant GPO-IDLE-STEPS
1024 constant GPO-MAXFD

create GPO-SLEEP-PFD 8 allot
create GPO-DEATH-BUF 8 allot
variable GPO-DEATH-GOT

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

\ ---- death-pipe reaper (design under test) ---------------------------------
\ Both ends close-on-exec so an exec'd grandchild never wedges the EOF.
: GPO-DEATH-PIPE ( -- fd fd )
   PIPE-PAIR {: rd:fd wr:fd :}
   rd FD-CLOEXEC!
   wr FD-CLOEXEC!
   rd wr ;

\ A forked reaper inherits every fd the worker held; if it kept a capture-pipe
\ write end open it would wedge the pool's EOF, so it closes all but RD.
: GPO-CLOSE-EXCEPT ( fd -- ) {: keep:fd :}
   0 begin dup GPO-MAXFD < while
      dup keep FD>N <> if dup close then
      1+
   repeat drop ;

\ Reap the reaper's own process group (kill(0, SIGKILL)).
: GPO-KILL-GROUP ( -- )
   0 >PID SIGKILL PROC-KILL-RAW drop ;

\ Reaper body: block reading RD; EOF (<= 0) means the parent died, so reap the
\ group. Never returns.
: GPO-WATCH ( fd -- ) {: rd:fd :}
   begin
      rd FD>N GPO-DEATH-BUF 1 read GPO-DEATH-GOT !
      GPO-DEATH-GOT @ 0 <= if GPO-KILL-GROUP exit then
   again ;

\ Fork a reaper into the caller's current process group. Never returns in the
\ reaper child; returns the reaper pid to the caller.
: GPO-FORK-REAPER ( fd fd -- pid ) {: rd:fd wr:fd :}
   PROC-FORK-RAW {: pid:pid :}
   pid PID>N 0= if
      rd GPO-CLOSE-EXCEPT
      rd GPO-WATCH
   then
   pid ;

\ ---- test topology ----------------------------------------------------------
\ P: hold only WR and idle until SIGKILLed; its death is the trigger.
: GPO-PARENT ( fd fd fd fd -- ) {: rd:fd wr:fd ar:fd aw:fd :}
   rd FD>N close
   ar FD>N close
   aw FD>N close
   GPO-IDLE
   0 GPO-EXIT ;

\ W: become its own group leader, arm the reaper in that group (it closes every
\ inherited fd but RD, so it never holds AW), keep only AW, and idle.
: GPO-WORKER ( fd fd fd fd -- ) {: rd:fd wr:fd ar:fd aw:fd :}
   0 >PID 0 >PID PROC-SETPGID drop
   rd wr GPO-FORK-REAPER drop
   rd FD>N close
   wr FD>N close
   ar FD>N close
   GPO-IDLE
   0 GPO-EXIT ;

\ Observe W's death via alive-pipe EOF within a hard deadline. POLL-IN reports
\ the read end ready (POLLHUP) once its last write end closes; nobody ever writes
\ AW, so a ready poll can only mean EOF.
: GPO-OBSERVE-DEAD? ( fd -- bool ) {: ar:fd :}
   0 begin dup GPO-OBSERVE-STEPS < while
      ar GPO-POLL-MS >MS POLL-IN COUNT>N 0 > if
         drop 0 0= exit
      then
      1+
   repeat drop 1 0= ;

: GPO-RUN ( -- bool )
   GPO-DEATH-PIPE {: rd:fd wr:fd :}
   PIPE-PAIR {: ar:fd aw:fd :}
   PROC-FORK-RAW {: ppid:pid :}
   ppid PID>N 0= if rd wr ar aw GPO-PARENT then
   PROC-FORK-RAW {: wpid:pid :}
   wpid PID>N 0= if rd wr ar aw GPO-WORKER then
   rd FD>N close
   wr FD>N close
   aw FD>N close
   GPO-SETTLE-MS GPO-SLEEP
   ppid SIGKILL PROC-KILL-RAW drop
   ppid PROC-WAIT-STATUS drop
   ar GPO-OBSERVE-DEAD?
   wpid SIGKILL PROC-KILL-GROUP drop
   wpid PROC-WAIT-STATUS drop
   ar FD>N close ;

\ ---- spawned-child co-located reaper (PROC-SPAWN-REAPER, live mechanism) -----
\ Unlike the forked-worker case above, a spawned (exec'd) pool child is not the
\ pool parent's process group; PROC-SPAWN-REAPER forks a reaper that JOINS the
\ child's group with setpgid(0,childpid) and watches the pool-death read end. On
\ pool-parent death the reaper SIGKILLs the child's group, so a hanging spawned
\ child cannot orphan-spin. This exercises the promoted word directly.
\
\ Topology (T = this test):
\   T forks P (the pool-parent surrogate, holds the death-pipe write end WR).
\   P forks C (a hanging child) and makes it its own group leader, then arms the
\   reaper R via PROC-SPAWN-REAPER (R joins C's group, watches RD). C keeps only
\   the alive-pipe write end AW.
\   T SIGKILLs P -> WR closes -> R reads EOF -> R SIGKILLs C's group (C and R).
\   T observes C's death through the alive-pipe EOF within a hard deadline.

\ C: drop the death pipe and the alive read end, keep only AW, and idle so it
\ hangs until the reaper kills its group. A live AW is what T watches for EOF.
: GSR-CHILD ( fd fd fd fd -- ) {: rd:fd wr:fd ar:fd aw:fd :}
   rd FD>N close
   wr FD>N close
   ar FD>N close
   GPO-IDLE
   0 GPO-EXIT ;

\ P: fork the hanging child, make it its own group leader (deterministically,
\ before arming so the reaper's setpgid join always succeeds), drop the alive
\ pipe, arm the co-located reaper, keep only WR, and idle until T SIGKILLs it.
: GSR-PARENT ( fd fd fd fd -- ) {: rd:fd wr:fd ar:fd aw:fd :}
   PROC-FORK-RAW {: cpid:pid :}
   cpid PID>N 0= if rd wr ar aw GSR-CHILD then
   cpid cpid PROC-SETPGID drop
   ar FD>N close
   aw FD>N close
   rd cpid PROC-SPAWN-REAPER drop
   rd FD>N close
   GPO-IDLE
   0 GPO-EXIT ;

: GSR-RUN ( -- bool )
   PROC-DEATH-PIPE {: rd:fd wr:fd :}
   PIPE-PAIR {: ar:fd aw:fd :}
   PROC-FORK-RAW {: ppid:pid :}
   ppid PID>N 0= if rd wr ar aw GSR-PARENT then
   rd FD>N close
   wr FD>N close
   aw FD>N close
   GPO-SETTLE-MS GPO-SLEEP
   ppid SIGKILL PROC-KILL-RAW drop
   ppid PROC-WAIT-STATUS drop
   ar GPO-OBSERVE-DEAD?
   ar FD>N close ;

: GPO-MAIN ( -- )
   T-RESET
   s" pool worker reaped when its parent is SIGKILLed" T-LABEL
   GPO-RUN TTRUE
   s" spawned pool child + reaper reaped when parent is SIGKILLed" T-LABEL
   GSR-RUN TTRUE
   T-REPORT
   s" gate-pool-orphan-test: ok" type cr ;

GPO-MAIN
