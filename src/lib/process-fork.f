\ process-fork.f - checked fork wrappers (package PROC-FORK) and the reaper
\ that kills a spawned pool child's process group when its pool parent dies.
\ CHECKED and SET-PGID are not named FORK and SETPGID: those tails would
\ case-insensitively shadow the core `fork` / `setpgid` their bodies call.
\ STORAGE CLASS. PROCESS-WIDE: the reaper's pollfd belongs to the process, not
\ to a task.

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
   [: PROC-TREE:CHILD-RESET ;] FORK-CHILD:REGISTER
   [: prof-child-reset ;] FORK-CHILD:REGISTER ;
REGISTER-RESETS

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
   [: [: fork ;] ADDRESS-CELLS:ACROSS-FORK ;] DYNAMIC-STORAGE:ACROSS-FORK ;

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
   HB-TARGET-LINUX-KERNEL? if 7 else 10 then ;

: SIG-BLOCK ( -- n )
   HB-TARGET-LINUX-KERNEL? if 0 else 1 then ;

: SIG-SETMASK ( -- n )
   HB-TARGET-LINUX-KERNEL? if 2 else 3 then ;

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

\ A reaper in a spawned pool child's process group watches the pool parent's
\ death-pipe write end. When the parent is SIGKILLed - the exact case where the
\ pool's own group-kill cleanup can never run - the write end closes, the
\ reaper's blocking read EOFs, and it SIGKILLs its own group, reaping the
\ orphaned child subtree. Built only from pipe/read/close/kill/setpgid, so it
\ is portable (no PR_SET_PDEATHSIG/kqueue, which are not primitives here).

$400 constant MAXFD

private

$FFFF constant PROC-REAP-EVMASK

create PROC-REAP-PFD 8 allot    \ one pollfd (8 bytes)

public

\ Death pipe: parent keeps the write end, each worker's reaper keeps the read
\ end. Both ends are close-on-exec so an exec'd grandchild never inherits a
\ write-end copy that would wedge the reaper's EOF.
: DEATH-PIPE ( -- fd fd )
   FD:PIPE ;

\ A forked reaper inherits every fd the worker held; an open death-pipe write
\ end would wedge its EOF, so it closes every fd but the death pipe's read end.
: CLOSE-EXCEPT ( fd -- ) {: keep:fd :}
   0 begin dup MAXFD < while
      dup keep FD>N <> if dup close then
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

\ Reaper body for a spawned (exec'd) child: block until the single parent-death
\ read end (pd) EOFs, then reap the reaper's own process group. The reaper has
\ already joined the child's group, so this kills the child subtree. Loops only
\ over EINTR; any other errno means the watch is broken, and the reaper leaves
\ without signalling rather than spinning.
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

\ Co-located reaper for a SPAWNED (exec'd) child the pool tracks by pid. The
\ pool parent that arms this reaper reaps by specific pid and never wait(-1)s, so
\ this reaper is a single-fork direct child the pool kills+waits by the returned
\ pid on slot cleanup.
\
\ The spawn primitive makes the child lead its own process group before exec.
\ The reaper joins that group with setpgid(0,cpid) and ONLY then may signal. If
\ the join fails because the child is already gone, the reaper exits WITHOUT
\ killing, so it can never signal the arming parent's own group. After a
\ successful join it closes every inherited fd but the pool-death read end and
\ blocks; when the pool parent is SIGKILLed the write end EOFs and the reaper
\ SIGKILLs the child's group. A failed fork throws E-PROC-SPAWN; no reaper
\ watches the child, and its caller ends it (the gate pool kills every slot).
: SPAWN-REAPER ( fd pid -- pid ) {: pd:fd cpid:pid :}
   CHECKED {: rpid:pid :}
   rpid PID>N 0= if
      0 >PID cpid SET-PGID RC>N 0 < if
         s" " 0 die
      then
      pd CLOSE-EXCEPT
      pd PROC-REAP-WATCH1
      s" " 0 die
   then
   rpid ;

;package
