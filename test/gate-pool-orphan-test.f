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
require lib/process-argv.f
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

\ ---- capture-spawn reaper (PROC-REAP-ARM seam, live mechanism) ---------------
\ A pool worker publishes its worker-alive read end as PROC-REAP-WATCH-FD; a
\ capture spawn then arms a co-located reaper in the LEAF's group watching that
\ fd, so a quiet leaf dies when the worker dies even though the leaf leads its
\ own group (the worker group-kill misses it) and writes nothing (no SIGPIPE
\ bound). Topology (T = this test):
\   T forks W. W: own group; worker-alive pipe WA-RD/WA-WR; publishes WA-RD as
\   PROC-REAP-WATCH-FD; spawns quiet hanging leaf L (/bin/sh -c "sleep 30")
\   through the REAL capture seam (PROC-CAPTURE-BEGIN + PROC-ARGV-PREPARE +
\   PROC-SPAWN-ARGV-CAPTURE -> PROC-CAPTURE-PID! arms the reaper), reports L's
\   pid + the reaper pid to T, then idles mid-capture.
\   T SIGKILLs W's group -> WA-WR closes -> L's reaper EOFs -> L's group dies.
\   T observes L's death by kill(L,0) polling within a hard deadline.
\ The CONTROL repeats the topology without publishing the watch fd: no reaper
\ is armed and L must SURVIVE W's death (proves the observation is real);
\ T then kills L directly. Two in-process legs prove both capture terminators
\ disarm: a timeout capture and two back-to-back completed captures each leave
\ PROC-REAP-PID at the no-reaper sentinel.

256 constant GCR-CAP
200 constant GCR-SHORT-MS
5000 constant GCR-LONG-MS
60000 constant GCR-HANG-MS
create GCR-PID-BUF 16 allot
create GCR-OUT-BUF GCR-CAP allot
create GCR-ERR-BUF GCR-CAP allot
variable GCR-LPID   variable GCR-RPID

: GCR-SLEEP-ARGV ( -- )
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   s" sleep 30" >LEN PROC-ARGV+ ;

: GCR-NOOP-ARGV ( -- )
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   s" :" >LEN PROC-ARGV+ ;

\ W: arm (or not), spawn the quiet leaf through the real capture seam, report
\ the leaf + reaper pids, and idle mid-capture until T kills the group. The
\ control arm EXPLICITLY clears the watch fd: under the gate this test runs
\ inside a pool worker that already publishes its own watch fd, and the forked
\ W inherits that cell -- "not set here" is not "unarmed".
: GCR-WORKER ( fd fd n -- ) {: pp-rd:fd pp-wr:fd armed:n :}
   0 >PID 0 >PID PROC-SETPGID drop
   pp-rd FD>N close
   PROC-DEATH-PIPE {: wa-rd:fd wa-wr:fd :}
   armed 0 <> if
      wa-rd FD>N PROC-REAP-WATCH-FD !
   else
      -1 PROC-REAP-WATCH-FD !
   then
   GCR-SLEEP-ARGV
   s" /bin/sh" >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   GCR-HANG-MS >MS PROC-CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-CAPTURE
   PROC-PID @ PID>N GCR-PID-BUF !
   PROC-REAP-PID @ PID>N GCR-PID-BUF 8 + !
   pp-wr FD>N GCR-PID-BUF 16 write drop
   pp-wr FD>N close
   GPO-IDLE
   0 GPO-EXIT ;

: GCR-READ-PIDS ( fd -- ) {: pp-rd:fd :}
   pp-rd FD>N GCR-PID-BUF 16 read 16 <> if E-PROC-OUTPUT throw then
   GCR-PID-BUF @ GCR-LPID !
   GCR-PID-BUF 8 + @ GCR-RPID ! ;

: GCR-DEAD? ( n -- bool ) {: lpid:n :}
   lpid >PID 0 PROC-KILL-RAW RC>N 0 < ;

: GCR-OBSERVE-DEAD? ( n -- bool ) {: lpid:n :}
   0 begin dup GPO-OBSERVE-STEPS < while
      lpid GCR-DEAD? if drop 0 0= exit then
      GPO-POLL-MS GPO-SLEEP
      1+
   repeat drop 1 0= ;

\ Fork W (armed or control), harvest the reported pids, then SIGKILL W's group
\ mid-capture and wait it -- the trigger both cases observe from.
: GCR-LAUNCH ( n -- ) {: armed:n :}
   PIPE-PAIR {: pp-rd:fd pp-wr:fd :}
   PROC-FORK-RAW {: wpid:pid :}
   wpid PID>N 0= if pp-rd pp-wr armed GCR-WORKER then
   pp-wr FD>N close
   pp-rd GCR-READ-PIDS
   pp-rd FD>N close
   GPO-SETTLE-MS GPO-SLEEP
   wpid SIGKILL PROC-KILL-GROUP drop
   wpid PROC-WAIT-STATUS drop ;

: GCR-ARMED? ( -- bool bool )   \ ( -- reaper-armed leaf-reaped )
   1 GCR-LAUNCH
   GCR-RPID @ 0 >
   GCR-LPID @ GCR-OBSERVE-DEAD? ;

: GCR-CONTROL? ( -- bool bool )   \ ( -- no-reaper leaf-survived ) + cleanup
   0 GCR-LAUNCH
   GCR-RPID @ 0 <
   GPO-SETTLE-MS GPO-SLEEP
   GCR-LPID @ GCR-DEAD? 0=
   GCR-LPID @ >PID SIGKILL PROC-KILL-RAW drop ;

\ The in-process legs run in whatever context hosts this test (under the gate:
\ a pool worker with its own live watch fd), so they save and RESTORE the cell
\ instead of clobbering it to -1 -- later suites in the same worker keep their
\ reaper coverage.
variable GCR-SAVED-FD

: GCR-TIMEOUT-DISARMED? ( -- bool bool )   \ timeout terminator disarms
   PROC-REAP-WATCH-FD @ GCR-SAVED-FD !
   PIPE-PAIR {: dw-rd:fd dw-wr:fd :}
   dw-rd FD>N PROC-REAP-WATCH-FD !
   GCR-SLEEP-ARGV
   s" /bin/sh" >LEN GCR-OUT-BUF GCR-CAP >LEN GCR-ERR-BUF GCR-CAP >LEN
   GCR-SHORT-MS >MS RUN-ARGV-CAPTURE-OUTCOME
   MATCH outcome
     exited OF drop 0 0= 0= ENDOF
     signaled OF drop 0 0= 0= ENDOF
     timeout OF 0 0= ENDOF
   ;MATCH nip nip
   GCR-SAVED-FD @ PROC-REAP-WATCH-FD !
   dw-rd FD>N close
   dw-wr FD>N close
   PROC-REAP-PID @ PID>N 0 < ;

: GCR-DONE-DISARMED? ( -- bool bool )   \ completion terminator disarms, twice
   PROC-REAP-WATCH-FD @ GCR-SAVED-FD !
   PIPE-PAIR {: dw-rd:fd dw-wr:fd :}
   dw-rd FD>N PROC-REAP-WATCH-FD !
   GCR-NOOP-ARGV
   s" /bin/sh" >LEN GCR-OUT-BUF GCR-CAP >LEN GCR-ERR-BUF GCR-CAP >LEN
   GCR-LONG-MS >MS RUN-ARGV-CAPTURE {: o1:len e1:len r1:rc :}
   GCR-NOOP-ARGV
   s" /bin/sh" >LEN GCR-OUT-BUF GCR-CAP >LEN GCR-ERR-BUF GCR-CAP >LEN
   GCR-LONG-MS >MS RUN-ARGV-CAPTURE {: o2:len e2:len r2:rc :}
   GCR-SAVED-FD @ PROC-REAP-WATCH-FD !
   dw-rd FD>N close
   dw-wr FD>N close
   r1 RC>N 0 = r2 RC>N 0 = and
   PROC-REAP-PID @ PID>N 0 < ;

: GPO-MAIN ( -- )
   T-RESET
   s" pool worker reaped when its parent is SIGKILLed" T-LABEL
   GPO-RUN TTRUE
   s" spawned pool child + reaper reaped when parent is SIGKILLed" T-LABEL
   GSR-RUN TTRUE
   s" capture leaf reaper arms via the spawn seam and reaps on worker death" T-LABEL
   GCR-ARMED? TTRUE TTRUE
   s" unarmed control: no reaper and the leaf survives worker death" T-LABEL
   GCR-CONTROL? TTRUE TTRUE
   s" timeout terminator disarms the capture reaper" T-LABEL
   GCR-TIMEOUT-DISARMED? TTRUE TTRUE
   s" completion terminator disarms the capture reaper (twice)" T-LABEL
   GCR-DONE-DISARMED? TTRUE TTRUE
   T-REPORT
   s" gate-pool-orphan-test: ok" type cr ;

GPO-MAIN
