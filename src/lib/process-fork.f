\ process-fork.f - checked fork wrappers (package PROC-FORK) and the reapers
\ that kill a worker's process group when its pool parent dies.
\
\ FORK-REAPER arms through a double fork: an intermediate forks the reaper and
\ exits, so the reaper reparents to init and a worker's wait(-1) with no child
\ of its own fails at once instead of blocking on the reaper. The reaper keeps
\ the worker's process group, so a group kill ends it and the pool parent's
\ death lets it kill the group. The intermediate answers through its exit
\ status: a throw would unwind its copy of the worker's stack.
\ CHECKED and SET-PGID are not named FORK and SETPGID: those tails would
\ case-insensitively shadow the core `fork` / `setpgid` their bodies call.
\ STORAGE CLASS. PROCESS-WIDE: the reaper's pollfd pair and REAP-WATCH-FD
\ belong to the process, not to a task.

require src/lib/error.f
require src/lib/task.f                       \ TASK:CHILD-RESET, registered below
require src/core/dynamic-storage.f
require src/habu/address-cells.f
require src/lib/process.f
require src/lib/fd.f                         \ FD:PIPE, the death pipe
require src/lib/fd-lock.f                    \ the third lock RAW holds while it forks
require src/lib/fs-mutate.f                  \ FS:CLEANUP-RESET, registered below
require src/lib/process-tree.f               \ PROC-TREE:CHILD-RESET, registered below
require src/lib/fork-child.f                 \ the resets RAW runs in the child
require src/lib/memory.f                     \ MEM:WITH-BYTES holds RAW's two masks
require src/lib/ffi-abi.f                    \ pthread_sigmask is declared here, not assumed

package PROC-FORK

private

\ An engine or AOT linker closure module never requires fork-child.f (no claim
\ owns its cells), so the one fork registers its reset here.
: REGISTER-RESETS ( -- )
   [: TASK:CHILD-RESET ;] FORK-CHILD:REGISTER
   [: FS:CLEANUP-RESET ;] FORK-CHILD:REGISTER
   [: PROC-TREE:CHILD-RESET ;] FORK-CHILD:REGISTER ;
REGISTER-RESETS

public

\ Runs under FORK-HELD's locks: no hook, buffer, quotation or spawn.
defer FORK-CALL ( -- n )

: FORK-CALL-DEFAULT ( -- )
   [: fork ;] is FORK-CALL ;
FORK-CALL-DEFAULT

private

\ The fork, made with the engine's two registry locks held. Each guards a
\ table that is unmapped and republished over several stores, so no reset in
\ the child could tell where a task it interrupted had stopped; held here,
\ nobody is inside one when the process is copied. DYNAMIC-STORAGE's nests
\ outside ADDRESS-CELLS's, and neither takes the other. Only ACROSS-FORK runs a
\ caller's word under one of them, so the thread forking here holds neither
\ already. A fork made on a task holds the task-local address-cell lock, not
\ the registrar's (ADDRESS-CELLS:ACROSS-FORK).
\ Outside both, FD-LOCK:WITH (src/lib/fd-lock.f): the child copies
\ no descriptor another task has created but not yet marked close-on-exec. The
\ child is the forking thread's copy, so the same finally releases it there.
: FORK-REGISTRIES-HELD ( -- n )
   [: [: FORK-CALL ;] ADDRESS-CELLS:ACROSS-FORK ;] DYNAMIC-STORAGE:ACROSS-FORK ;

: FORK-HELD ( -- n )
   [: FORK-REGISTRIES-HELD ;] FD-LOCK:WITH ;

\ glibc's sigset_t is 128 bytes and macOS's 4: the larger holds either.
128 constant SIGSET-BYTES

\ SIGSEGV, SIGILL, SIGFPE and SIGTRAP carry the same number on both hosts;
\ SIGBUS and the two `how` values do not.
11 constant SIGSEGV
4 constant SIGILL
8 constant SIGFPE
5 constant SIGTRAP

: SIGBUS ( -- n )
   HB-TARGET-LINUX-KERNEL? if 7 exit then
   HB-TARGET-MACOS? if 10 exit then
   E-PROC-HOST throw ;

: SIG-BLOCK ( -- n )
   HB-TARGET-LINUX-KERNEL? if 0 exit then
   HB-TARGET-MACOS? if 1 exit then
   E-PROC-HOST throw ;

: SIG-SETMASK ( -- n )
   HB-TARGET-LINUX-KERNEL? if 2 exit then
   HB-TARGET-MACOS? if 3 exit then
   E-PROC-HOST throw ;

PROCESS-SYMBOLS

FUNCTION: SIGMASK-CALL pthread_sigmask ( n ptr u8 ptr u8 -- i32 )
   2 SIGSET-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: SIGFILL-CALL sigfillset ( ptr u8 -- i32 )
   0 SIGSET-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: SIGDEL-CALL sigdelset ( ptr u8 n -- i32 )
   0 SIGSET-BYTES WRITES-BYTES
;FUNCTION

: MASK-OK ( n -- )
   0 <> if E-PROC-SPAWN throw then ;

: UNBLOCK ( ptr u8 n -- )
   SIGDEL-CALL MASK-OK ;

\ The engine's crash and breakpoint handlers stay reachable: a fault raised
\ while blocked kills the process on Linux and refaults for ever on macOS.
: ASYNC-SET ( ptr u8 -- ) {: set:ptr :}
   set SIGFILL-CALL MASK-OK
   set SIGSEGV UNBLOCK
   set SIGBUS UNBLOCK
   set SIGILL UNBLOCK
   set SIGFPE UNBLOCK
   set SIGTRAP UNBLOCK ;

\ The thread's old mask follows the set in RAW's buffer.
: OLD-MASK ( ptr u8 -- ptr u8 )
   SIGSET-BYTES + ;

: BLOCK-ASYNC ( ptr u8 -- ) {: set:ptr :}
   set ASYNC-SET
   SIG-BLOCK set set OLD-MASK SIGMASK-CALL MASK-OK ;

\ The answer is dropped: the call cannot fail once BLOCK-ASYNC's did not, and
\ the child runs this before RAW has returned.
: RESTORE-MASK ( ptr u8 -- ) {: set:ptr :}
   SIG-SETMASK set OLD-MASK set SIGMASK-CALL drop ;

: FORK-MASKED ( ptr u8 -- pid ) {: set:ptr :}
   set BLOCK-ASYNC
   [: FORK-HELD >PID dup PID>N 0= if FORK-CHILD:RESET then ;]
   [: set RESTORE-MASK ;] finally ;

public

\ A caught stop signal reaches the child after its resets, under the default
\ action. A refusal throws before the fork, but a refused fork answers pid < 0.
: RAW ( -- pid )
   SIGSET-BYTES 2 * MEM:BYTES-ALLOC-LEN
   [: drop FORK-MASKED ;] MEM:WITH-BYTES ;

: CHECKED ( -- pid )
   RAW {: pid:pid :}
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

\ Runs q as the process root of a child RAW or CHECKED just forked, and never
\ returns: the child exits 0 once q returns, and a throw out of q ends it as an
\ uncaught throw ends a fresh process, through habu2.f LUNCAUGHT - the engine's
\ report on fd 2 and layout.f's UNCAUGHT-RC (67), or the code itself from 1 to
\ 255. The fork copied the parent's catch frames (HND-CELL), so a throw
\ would otherwise resume the parent's catch in the child; they are cleared
\ before q runs. The exit
\ hook stays: RAW's child resets ran FS:CLEANUP-RESET, so the inherited
\ CLEANUP-AT-EXIT removes only what the child itself registers.
: CHILD-MAIN ( [ -- ] -- ) {: q :}
   0 data-base HND-CELL + !
   \ A refusal the parent caught would make a throw of its code exit RC-REJECT.
   0 data-base REFUSAL-ABI:CODE-CELL + !
   q execute
   s" " 0 die ;

\ setpgid(pid, pgid): place pid into process group pgid. `0 0 PROC-FORK:SET-PGID`
\ makes the caller its own group leader (pgid == pid) so a later group signal
\ reaches its whole subtree.
: SET-PGID ( pid pid -- rc ) {: pid:pid pgid:pid :}
   pid PID>N pgid PID>N setpgid >RC ;

\ Signal the process group led by pid (kill(-pid, sig)); pid must be a group
\ leader (its child set `0 0 PROC-FORK:SET-PGID`), so this reaches its grandchildren.
\ A pid at or below 1 leads no such group: kill(0) is the caller's own group and
\ kill(-1) every process the caller may signal, so it is refused, as
\ src/lib/process-tree.f KILL-TREE refuses one.
: KILL-GROUP ( pid n -- rc ) {: pid:pid sig:n :}
   pid PID>N 1 <= if E-PROC-OUTPUT throw then
   pid PID>N negate >PID sig PROCESS:KILL-RAW ;

\ A pool worker in its own process group forks a reaper that watches the pool
\ parent's death-pipe write end. When the parent is SIGKILLed - the exact case
\ where the pool's own group-kill cleanup can never run - the write end closes,
\ the reaper's blocking read EOFs, and it SIGKILLs its own group, reaping the
\ orphaned worker subtree. Built only from pipe/read/close/kill/setpgid, so it
\ is portable (no PR_SET_PDEATHSIG/kqueue, which are not primitives here).

$400 constant MAXFD

private

$FFFF constant PROC-REAP-EVMASK

create PROC-REAP-PFD 16 allot   \ two pollfd cells (8 bytes each)

public

\ Death pipe: parent keeps the write end, each worker's reaper keeps the read
\ end. Both ends are close-on-exec so an exec'd grandchild never inherits a
\ write-end copy that would wedge the reaper's EOF.
: DEATH-PIPE ( -- fd fd )
   FD:PIPE ;

\ A forked reaper inherits every fd the worker held; keeping any capture-pipe
\ or death-pipe write end open would wedge a downstream EOF, so it closes all
\ but the two read ends it watches.
: CLOSE-EXCEPT2 ( fd fd -- ) {: k1:fd k2:fd :}
   0 begin dup MAXFD < while
      dup k1 FD>N <> over k2 FD>N <> and if dup close then
      1+
   repeat drop ;

private

: PROC-REAP-KILL-GROUP ( -- )
   0 >PID PROCESS:SIGKILL PROCESS:KILL-RAW drop ;

: PROC-REAP-PFD-SLOT ( idx -- ptr n ) {: idx:idx :}
   idx IDX>N 8 * PROC-REAP-PFD + ;

: PROC-REAP-PFD! ( fd n idx -- ) {: fd:fd events:n idx:idx :}
   events 32 lshift fd FD>N $FFFFFFFF and or idx PROC-REAP-PFD-SLOT ! ;

: PROC-REAP-REVENTS ( idx -- n )
   PROC-REAP-PFD-SLOT @ 48 rshift PROC-REAP-EVMASK and ;

\ Reaper body: block until the parent-death read end (pd) or the worker-alive
\ read end (wa) reports EOF. Neither pipe is ever written, so a readable poll
\ means its write end closed. If pd closed the pool parent died - reap the
\ worker's process group. If wa closed the worker exited normally - just
\ return so the reaper exits without signalling. Loops only over EINTR: the wait
\ is indefinite, so poll can only come back with an event or an error, and any
\ errno other than EINTR means the watch itself is broken - the reaper leaves
\ without signalling rather than spinning on a descriptor it can no longer read.
: PROC-REAP-WATCH2 ( fd fd -- ) {: pd:fd wa:fd :}
   begin
      pd FD:POLLIN 0 >IDX PROC-REAP-PFD!
      wa FD:POLLIN 1 >IDX PROC-REAP-PFD!
      PROC-REAP-PFD 2 -1 poll {: rc:n :}
      rc 0 > if
         0 >IDX PROC-REAP-REVENTS 0 <> if PROC-REAP-KILL-GROUP then
         exit
      then
      rc FD:EINTR# negate <> if exit then
   again ;

\ FORK-REAPER's intermediate exits with this when it cannot fork the reaper.
1 constant REAPER-REFUSED-RC

public

\ The worker is its own group leader and holds wa's write end until it exits.
\ CHECKED, so a refused fork's -1 never reaches the wait as wait4(-1).
: FORK-REAPER ( fd fd -- ) {: pd:fd wa:fd :}
   CHECKED {: ipid:pid :}
   ipid PID>N 0= if
      RAW {: r2:pid :}
      r2 PID>N 0= if
         pd wa CLOSE-EXCEPT2
         pd wa PROC-REAP-WATCH2
      then
      r2 PID>N 0 < if s" " REAPER-REFUSED-RC die then
      s" " 0 die
   then
   ipid PROCESS:WAIT PROCESS:STATUS>RC RC>N 0<> if E-PROC-SPAWN throw then ;

\ Reaper body for a spawned (exec'd) child: block until the single parent-death
\ read end (pd) EOFs, then reap the reaper's own process group. The reaper has
\ already joined the child's group, so this kills the child subtree. Loops only
\ over EINTR; any other errno means the watch is broken, and the reaper leaves
\ without signalling rather than spinning.

private

: PROC-REAP-WATCH1 ( fd -- ) {: pd:fd :}
   begin
      pd FD:POLLIN 0 >IDX PROC-REAP-PFD!
      PROC-REAP-PFD 1 -1 poll {: rc:n :}
      rc 0 > if
         0 >IDX PROC-REAP-REVENTS 0 <> if PROC-REAP-KILL-GROUP then
         exit
      then
      rc FD:EINTR# negate <> if exit then
   again ;

public

\ Co-located reaper for a SPAWNED (exec'd) child the pool tracks by pid. Unlike
\ FORK-REAPER (forked workers, invisible to the worker's own wait(-1)), the
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
\ SIGKILLs the child's group. A failed fork throws E-PROC-SPAWN; no reaper
\ watches the child, and its caller ends it (PROCESS:CAPTURE-PID! kills the
\ capture child, the gate pool kills every slot).
: SPAWN-REAPER ( fd pid -- pid ) {: pd:fd cpid:pid :}
   CHECKED {: rpid:pid :}
   rpid PID>N 0= if
      0 >PID cpid SET-PGID RC>N 0 < if
         s" " 0 die
      then
      pd pd CLOSE-EXCEPT2
      pd PROC-REAP-WATCH1
      s" " 0 die
   then
   rpid ;

\ A worker that owns a death-watch read end (its worker-alive pipe: the write
\ end is held only by the worker, so EOF == this worker died, whatever killed
\ it) publishes the fd here; every capture spawn (PROCESS:RUN-CAPTURE family,
\ via PROCESS:CAPTURE-PID!) then arms a co-located SPAWN-REAPER in the child's
\ group watching that fd. Spawned capture children are their own group leaders,
\ so neither the worker's group-kill nor its parent-death reaper reaches them;
\ this closes that gap. -1 = no context: PROC-REAP-ARM-ON arms nothing, so
\ ordinary tools keep zero extra processes. The fd cell is per-process state:
\ forked children inherit a copy and nested pool workers overwrite it with
\ their own worker-alive read end.
variable REAP-WATCH-FD   -1 REAP-WATCH-FD !

private

: PROC-REAP-ARM-ON ( pid -- pid ) {: cpid:pid :}
   REAP-WATCH-FD @ dup 0 < if drop -1 >PID exit then
   >FD cpid SPAWN-REAPER ;

: PROC-REAP-ARM-INSTALL ( -- )
   [: PROC-REAP-ARM-ON ;] is PROCESS:REAP-ARM ;
PROC-REAP-ARM-INSTALL

;package
