\ gate-pool-orphan-test.f - regression: pool children die on parent death.
\
\ Proves the production death-pipe reaper that fixes forked-pool-worker orphans:
\ a worker in its own process group calls PROC-FORK:DEATH-PIPE and
\ PROC-FORK:FORK-REAPER to watch its pool parent's death pipe and its own
\ worker-alive pipe. When the watched parent is SIGKILLed - the exact case where
\ the pool's own GT-POOL-KILL-ALL cleanup can never run - the reaper kills the
\ worker's group, so no orphan keeps spinning.
\
\ Topology (T = this test):
\   T forks P (holds the death-pipe write end WR) and W (its own group).
\   W creates its worker-alive pipe and arms the production reaper R.
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

\ True when the wait status is a kill; a child that exited is reported.
: GPO-KILLED? ( n -- bool )
   PROC-STATUS>OUTCOME MATCH outcome
     exited OF
        s" gate-pool-orphan-test: a child exited before it was killed, code " type .
        false
     ENDOF
     signaled OF drop true ENDOF
     timeout OF false ENDOF
   ;MATCH ;

\ ---- test topology ----------------------------------------------------------
\ P: hold only WR and idle until SIGKILLed; its death is the trigger.
: GPO-PARENT ( fd fd fd fd -- ) {: rd:fd wr:fd ar:fd aw:fd :}
   rd FD>N close
   ar FD>N close
   aw FD>N close
   GPO-IDLE
   0 GPO-EXIT ;

\ Stack-preserving under catch: the two watched fds are the quotation's window.
: GPO-ARM ( fd fd -- fd fd ) {: rd:fd wa:fd :}
   rd wa PROC-FORK:FORK-REAPER
   rd wa ;

\ W: become its own group leader, create a worker-alive pipe, arm the production
\ reaper on both death watches, keep only the write ends, and idle.
: GPO-WORKER ( fd fd fd fd -- ) {: rd:fd wr:fd ar:fd aw:fd :}
   0 >PID 0 >PID PROC-FORK:SET-PGID drop
   PROC-FORK:DEATH-PIPE {: wa-rd:fd wa-wr:fd :}
   rd wa-rd [: GPO-ARM ;] catch {: arm:n :}
   2drop
   arm 0<> if arm GPO-REFUSED then
   rd FD>N close
   wr FD>N close
   wa-rd FD>N close
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
   PROC-FORK:DEATH-PIPE {: rd:fd wr:fd :}
   PIPE-PAIR {: ar:fd aw:fd :}
   PROC-FORK:CHECKED {: ppid:pid :}
   ppid PID>N 0= if rd wr ar aw GPO-PARENT then
   PROC-FORK:CHECKED {: wpid:pid :}
   wpid PID>N 0= if rd wr ar aw GPO-WORKER then
   rd FD>N close
   wr FD>N close
   aw FD>N close
   GPO-SETTLE-MS GPO-SLEEP
   ppid SIGKILL PROC-KILL-RAW drop
   ppid PROC-WAIT-STATUS drop
   ar GPO-OBSERVE-DEAD?
   wpid SIGKILL PROC-FORK:KILL-GROUP drop
   wpid PROC-WAIT-STATUS GPO-KILLED? and
   ar FD>N close ;

\ ---- spawned-child co-located reaper (PROC-FORK:SPAWN-REAPER, live mechanism) -----
\ Unlike the forked-worker case above, a spawned (exec'd) pool child is not the
\ pool parent's process group; PROC-FORK:SPAWN-REAPER forks a reaper that JOINS the
\ child's group with setpgid(0,childpid) and watches the pool-death read end. On
\ pool-parent death the reaper SIGKILLs the child's group, so a hanging spawned
\ child cannot orphan-spin. This exercises the promoted word directly.
\
\ Topology (T = this test):
\   T forks P (the pool-parent surrogate, holds the death-pipe write end WR).
\   P forks C (a hanging child) and makes it its own group leader, then arms the
\   reaper R via PROC-FORK:SPAWN-REAPER (R joins C's group, watches RD). C keeps only
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

\ Stack-preserving under catch: the watched fd and the child are the window.
: GSR-ARM ( fd pid -- fd pid ) {: rd:fd cpid:pid :}
   rd cpid PROC-FORK:SPAWN-REAPER drop
   rd cpid ;

\ No reaper watches C once P's arm is refused, so P ends C before it exits.
: GSR-REFUSED ( n pid -- ) {: code:n cpid:pid :}
   cpid SIGKILL PROC-KILL-RAW drop
   cpid PROC-WAIT-STATUS-RAW drop
   code GPO-REFUSED ;

\ P: fork the hanging child, make it its own group leader (deterministically,
\ before arming so the reaper's setpgid join always succeeds), drop the alive
\ pipe, arm the co-located reaper, keep only WR, and idle until T SIGKILLs it.
\ A refused fork of C ends P first: a negative pid must never reach the kill.
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
   PIPE-PAIR {: ar:fd aw:fd :}
   PROC-FORK:CHECKED {: ppid:pid :}
   ppid PID>N 0= if rd wr ar aw GSR-PARENT then
   rd FD>N close
   wr FD>N close
   aw FD>N close
   GPO-SETTLE-MS GPO-SLEEP
   ppid SIGKILL PROC-KILL-RAW drop
   ppid PROC-WAIT-STATUS GPO-KILLED?
   ar GPO-OBSERVE-DEAD? and
   ar FD>N close ;

\ ---- capture-spawn reaper (PROC-REAP-ARM seam, live mechanism) ---------------
\ A pool worker publishes its worker-alive read end as PROC-FORK:REAP-WATCH-FD; a
\ capture spawn then arms a co-located reaper in the LEAF's group watching that
\ fd, so a quiet leaf dies when the worker dies even though the leaf leads its
\ own group (the worker group-kill misses it) and writes nothing (no SIGPIPE
\ bound). Topology (T = this test):
\   T forks W. W: own group; worker-alive pipe WA-RD/WA-WR; publishes WA-RD as
\   PROC-FORK:REAP-WATCH-FD; spawns quiet hanging leaf L (/bin/sh -c "sleep 30")
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

\ Spawn the quiet leaf through the real capture seam, which arms its reaper
\ when the watch fd is set.
: GCR-SPAWN-LEAF ( -- )
   GCR-SLEEP-ARGV
   s" /bin/sh" >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   GCR-HANG-MS >MS PROC-CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-CAPTURE ;

\ W: arm (or not), spawn the quiet leaf through the real capture seam, report
\ the leaf + reaper pids, and idle mid-capture until T kills the group. The
\ control arm EXPLICITLY clears the watch fd: under the gate this test runs
\ inside a pool worker that already publishes its own watch fd, and the forked
\ W inherits that cell -- "not set here" is not "unarmed".
: GCR-WORKER ( fd fd n -- ) {: pp-rd:fd pp-wr:fd armed:n :}
   0 >PID 0 >PID PROC-FORK:SET-PGID drop
   pp-rd FD>N close
   PROC-FORK:DEATH-PIPE {: wa-rd:fd wa-wr:fd :}
   armed 0 <> if
      wa-rd FD>N PROC-FORK:REAP-WATCH-FD !
   else
      -1 PROC-FORK:REAP-WATCH-FD !
   then
   [: GCR-SPAWN-LEAF ;] catch {: arm:n :}
   arm 0<> if arm GPO-REFUSED then
   PROC-PID @ GCR-PID-BUF !
   PROC-REAP-PID @ GCR-PID-BUF 8 + !
   pp-wr FD>N GCR-PID-BUF 16 write drop
   pp-wr FD>N close
   GPO-IDLE
   0 GPO-EXIT ;

\ False when W ended before it reported its pids.
: GCR-READ-PIDS ( fd -- bool ) {: pp-rd:fd :}
   pp-rd FD>N GCR-PID-BUF 16 read 16 <> if false exit then
   GCR-PID-BUF @ GCR-LPID !
   GCR-PID-BUF 8 + @ GCR-RPID !
   true ;

: GCR-DEAD? ( n -- bool ) {: lpid:n :}
   lpid >PID 0 PROC-KILL-RAW RC>N 0 < ;

: GCR-OBSERVE-DEAD? ( n -- bool ) {: lpid:n :}
   0 begin dup GPO-OBSERVE-STEPS < while
      lpid GCR-DEAD? if drop 0 0= exit then
      GPO-POLL-MS GPO-SLEEP
      1+
   repeat drop 1 0= ;

\ Fork W (armed or control), harvest the reported pids, then SIGKILL W's group
\ mid-capture and wait it -- the trigger both cases observe from. True when W
\ reported its pids and ran until that kill.
: GCR-LAUNCH ( n -- bool ) {: armed:n :}
   PIPE-PAIR {: pp-rd:fd pp-wr:fd :}
   PROC-FORK:CHECKED {: wpid:pid :}
   wpid PID>N 0= if pp-rd pp-wr armed GCR-WORKER then
   pp-wr FD>N close
   pp-rd GCR-READ-PIDS
   pp-rd FD>N close
   GPO-SETTLE-MS GPO-SLEEP
   wpid SIGKILL PROC-FORK:KILL-GROUP drop
   wpid PROC-WAIT-STATUS GPO-KILLED? and ;

: GCR-ARMED? ( -- bool bool )   \ ( -- reaper-armed leaf-reaped )
   1 GCR-LAUNCH 0= if false false exit then
   GCR-RPID @ 0 >
   GCR-LPID @ GCR-OBSERVE-DEAD? ;

: GCR-CONTROL? ( -- bool bool )   \ ( -- no-reaper leaf-survived ) + cleanup
   0 GCR-LAUNCH 0= if false false exit then
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
   PROC-FORK:REAP-WATCH-FD @ GCR-SAVED-FD !
   PIPE-PAIR {: dw-rd:fd dw-wr:fd :}
   dw-rd FD>N PROC-FORK:REAP-WATCH-FD !
   GCR-SLEEP-ARGV
   s" /bin/sh" >LEN GCR-OUT-BUF GCR-CAP >LEN GCR-ERR-BUF GCR-CAP >LEN
   GCR-SHORT-MS >MS RUN-ARGV-CAPTURE-OUTCOME
   MATCH outcome
     exited OF drop 0 0= 0= ENDOF
     signaled OF drop 0 0= 0= ENDOF
     timeout OF 0 0= ENDOF
   ;MATCH nip nip
   GCR-SAVED-FD @ PROC-FORK:REAP-WATCH-FD !
   dw-rd FD>N close
   dw-wr FD>N close
   PROC-REAP-PID @ 0 < ;

: GCR-CODE ( result<pcap:captured,pcap:failed> -- n )   \ completion code: 0 on a clean exit, else nonzero
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop 0 ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} c RC>N ENDOF
   ;MATCH ;

: GCR-DONE-DISARMED? ( -- bool bool )   \ completion terminator disarms, twice
   PROC-FORK:REAP-WATCH-FD @ GCR-SAVED-FD !
   PIPE-PAIR {: dw-rd:fd dw-wr:fd :}
   dw-rd FD>N PROC-FORK:REAP-WATCH-FD !
   GCR-NOOP-ARGV
   s" /bin/sh" >LEN GCR-OUT-BUF GCR-CAP >LEN GCR-ERR-BUF GCR-CAP >LEN
   GCR-LONG-MS >MS RUN-ARGV-CAPTURE GCR-CODE {: r1:n :}
   GCR-NOOP-ARGV
   s" /bin/sh" >LEN GCR-OUT-BUF GCR-CAP >LEN GCR-ERR-BUF GCR-CAP >LEN
   GCR-LONG-MS >MS RUN-ARGV-CAPTURE GCR-CODE {: r2:n :}
   GCR-SAVED-FD @ PROC-FORK:REAP-WATCH-FD !
   dw-rd FD>N close
   dw-wr FD>N close
   r1 0 = r2 0 = and
   PROC-REAP-PID @ 0 < ;

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
