\ process-fork.f - checked fork wrappers.
\
\ Load after native refresh: older engines do not have the fork/setpgid
\ primitives.

require lib/errors.f
require lib/process.f

: PROC-FORK-RAW ( -- pid )
   fork >PID ;

: PROC-FORK ( -- pid )
   PROC-FORK-RAW {: pid:pid :}
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

\ setpgid(pid, pgid): place pid into process group pgid. `0 0 PROC-SETPGID`
\ makes the caller its own group leader (pgid == pid) so a later group signal
\ reaches its whole subtree.
: PROC-SETPGID ( pid pid -- rc ) {: pid:pid pgid:pid :}
   pid PID>N pgid PID>N setpgid >RC ;

\ Signal the process group led by pid (kill(-pid, sig)); pid must be a group
\ leader (its child set `0 0 PROC-SETPGID`), so this reaches its grandchildren.
: PROC-KILL-GROUP ( pid n -- rc ) {: pid:pid sig:n :}
   pid PID>N negate >PID sig PROC-KILL-RAW ;

\ ---- parent-death reaper ----------------------------------------------------
\ A pool worker in its own process group forks a reaper that watches the pool
\ parent's death-pipe write end. When the parent is SIGKILLed - the exact case
\ where the pool's own group-kill cleanup can never run - the write end closes,
\ the reaper's blocking read EOFs, and it SIGKILLs its own group, reaping the
\ orphaned worker subtree. Built only from pipe/read/close/kill/setpgid, so it
\ is portable (no PR_SET_PDEATHSIG/kqueue, which are not primitives here).

$400 constant PROC-MAXFD
$FFFF constant PROC-REAP-EVMASK

create PROC-REAP-PFD 16 allot   \ two pollfd cells (8 bytes each)

\ Death pipe: parent keeps the write end, each worker's reaper keeps the read
\ end. Both ends are close-on-exec so an exec'd grandchild never inherits a
\ write-end copy that would wedge the reaper's EOF.
: PROC-DEATH-PIPE ( -- fd fd )
   PIPE-PAIR {: rd:fd wr:fd :}
   rd FD-CLOEXEC!
   wr FD-CLOEXEC!
   rd wr ;

\ A forked reaper inherits every fd the worker held; keeping any capture-pipe
\ or death-pipe write end open would wedge a downstream EOF, so it closes all
\ but the two read ends it watches.
: PROC-CLOSE-EXCEPT2 ( fd fd -- ) {: k1:fd k2:fd :}
   0 begin dup PROC-MAXFD < while
      dup k1 FD>N <> over k2 FD>N <> and if dup close then
      1+
   repeat drop ;

\ Reap the reaper's own process group (kill(0, SIGKILL)).
: PROC-REAP-KILL-GROUP ( -- )
   0 >PID SIGKILL PROC-KILL-RAW drop ;

: PROC-REAP-PFD-SLOT ( idx -- ptr a ) {: idx:idx :}
   idx IDX>N 8 * PROC-REAP-PFD + ;

: PROC-REAP-PFD! ( fd n idx -- ) {: fd:fd events:n idx:idx :}
   events 32 lshift fd FD>N $FFFFFFFF and or idx PROC-REAP-PFD-SLOT ! ;

: PROC-REAP-REVENTS ( idx -- n )
   PROC-REAP-PFD-SLOT @ 48 rshift PROC-REAP-EVMASK and ;

\ Reaper body: block until the parent-death read end (pd) or the worker-alive
\ read end (wa) reports EOF. Neither pipe is ever written, so a readable poll
\ means its write end closed. If pd closed the pool parent died - reap the
\ worker's process group. If wa closed the worker exited normally - just
\ return so the reaper exits without signalling. Loops only over EINTR.
: PROC-REAP-WATCH2 ( fd fd -- ) {: pd:fd wa:fd :}
   begin
      pd POLLIN 0 >IDX PROC-REAP-PFD!
      wa POLLIN 1 >IDX PROC-REAP-PFD!
      PROC-REAP-PFD 2 -1 poll {: rc:n :}
      rc 0 > if
         0 >IDX PROC-REAP-REVENTS 0 <> if PROC-REAP-KILL-GROUP then
         exit
      then
   again ;

\ Arm a parent-death reaper that is INVISIBLE to the worker's own wait(-1):
\ a throwaway intermediate forks the real reaper (a grandchild) and exits, so
\ the reaper reparents to init rather than staying the worker's child. A worker
\ body may itself call wait(-1) expecting no children (e.g. a PROC-WAIT error
\ test), and a reaper child would make that wait block forever. The reaper
\ still inherits the worker's process group, so a timeout group-kill reaps it
\ and a parent death lets it kill the group. It also watches the worker-alive
\ pipe (wa) so it exits on its own when the worker finishes, leaving no orphan
\ - the worker never has to track or kill it. The worker must already be its
\ own group leader; it keeps the wa write end open (dropped only at its exit).
: PROC-FORK-REAPER ( fd fd -- ) {: pd:fd wa:fd :}
   PROC-FORK-RAW {: ipid:pid :}
   ipid PID>N 0= if
      PROC-FORK-RAW {: r2:pid :}
      r2 PID>N 0= if
         pd wa PROC-CLOSE-EXCEPT2
         pd wa PROC-REAP-WATCH2
      then
      s" " 0 die
   then
   ipid PROC-WAIT-STATUS drop ;

\ ---- co-located reaper for a SPAWNED child ---------------------------------
\ Reaper body for a spawned (exec'd) child: block until the single parent-death
\ read end (pd) EOFs, then reap the reaper's own process group. The reaper has
\ already joined the child's group, so this kills the child subtree. Loops only
\ over EINTR.
: PROC-REAP-WATCH1 ( fd -- ) {: pd:fd :}
   begin
      pd POLLIN 0 >IDX PROC-REAP-PFD!
      PROC-REAP-PFD 1 -1 poll {: rc:n :}
      rc 0 > if
         0 >IDX PROC-REAP-REVENTS 0 <> if PROC-REAP-KILL-GROUP then
         exit
      then
   again ;

\ Co-located reaper for a SPAWNED (exec'd) child the pool tracks by pid. Unlike
\ PROC-FORK-REAPER (forked workers, invisible to the worker's own wait(-1)), the
\ pool parent that arms this reaper reaps by specific pid and never wait(-1)s, so
\ this reaper is a single-fork direct child the pool kills+waits by the returned
\ pid on slot cleanup - no double-fork or worker-alive pipe is needed.
\
\ The spawn primitive makes the child lead its own process group before exec.
\ The reaper joins that group with setpgid(0,cpid) and ONLY then may signal. If
\ the join fails because the child is already gone, the reaper exits WITHOUT
\ killing, so it can never signal the arming parent's own group. After a
\ successful join it closes every inherited fd but the pool-death read end and
\ blocks; when the pool parent is SIGKILLed the write end EOFs and the reaper
\ SIGKILLs the child's group.
: PROC-SPAWN-REAPER ( fd pid -- pid ) {: pd:fd cpid:pid :}
   PROC-FORK-RAW {: rpid:pid :}
   rpid PID>N 0= if
      0 >PID cpid PROC-SETPGID RC>N 0 < if
         s" " 0 die
      then
      pd pd PROC-CLOSE-EXCEPT2
      pd PROC-REAP-WATCH1
      s" " 0 die
   then
   rpid ;

\ ---- capture-spawn reaper context -------------------------------------------
\ A worker that owns a death-watch read end (its worker-alive pipe: the write
\ end is held only by the worker, so EOF == this worker died, whatever killed
\ it) publishes the fd here; every capture spawn (PROC-RUN-CAPTURE family, via
\ PROC-CAPTURE-PID!) then arms a co-located PROC-SPAWN-REAPER in the child's
\ group watching that fd. Spawned capture children are their own group leaders,
\ so neither the worker's group-kill nor its parent-death reaper reaches them;
\ this closes that gap. -1 = no context: PROC-REAP-ARM-ON arms nothing, so
\ ordinary tools keep zero extra processes. The fd cell is per-process state:
\ forked children inherit a copy and nested pool workers overwrite it with
\ their own worker-alive read end.
variable PROC-REAP-WATCH-FD   -1 PROC-REAP-WATCH-FD !

: PROC-REAP-ARM-ON ( pid -- pid ) {: cpid:pid :}
   PROC-REAP-WATCH-FD @ dup 0 < if drop -1 >PID exit then
   >FD cpid PROC-SPAWN-REAPER ;

: PROC-REAP-ARM-INSTALL ( -- )
   [: PROC-REAP-ARM-ON ;] is PROC-REAP-ARM ;
PROC-REAP-ARM-INSTALL
