\ fork-child-test.f - a forked child does not inherit another task's hold.
\ Run: bin/hb --load src/lib/fork-child-test.f
\ Each case holds process-wide state in its own task (src/lib/test/fork-hold.f),
\ forks from the main thread, and has the child reach that state through its
\ owner's public words; the child must end in time and exit 0. Whether RAW holds
\ a lock across the fork or the child resets it is src/lib/fork-child.f's call.

\ Failures caught:
\  1. The child waits for ever: a lock another task held at the fork stays held.
\  2. The child uses what it did not inherit: on Darwin the main thread's park
\     is a Mach port, which does not cross a fork; the first wake throws
\     E-TASK-THREAD.
\  3. The parent loses its lock: a reset on the parent's side frees the holder's
\     lock, and a third task could take it beside it.
\  4. The fork waits for ever: PROC-FORK:RAW holds the engine's three registry
\     locks while it forks; out of nesting order or never given back, they hang
\     the forking thread or the holder until the gate's row deadline.
\ Not covered: a table another task is part way through republishing, a window
\ of a few instructions no fork can be aimed at. A fork made on a task holds the
\ task-local address-cell lock, not the registrar's (ADDRESS-CELLS
\ finds its lock through data-base).
\ A holder takes its lock with its owner's word. PROC-TREE's is private, so that
\ case reopens its package; TASK is sealed and IMAGE-LIFECYCLE, DYNAMIC-STORAGE
\ and ADDRESS-CELLS are baked into the engine, so their holders take and free
\ the lock through a public word over and over while children fork into it.
\ TASK's one-time park setup (MAIN-PARK-READY at 1) has no public word that
\ holds it: the park case forks after the setup, the state Darwin cannot carry.

require src/lib/errors.f
require src/lib/test.f
require src/lib/task.f
require src/lib/image-lifecycle.f
require src/lib/process.f
require src/lib/process-fork.f
require src/lib/process-tree.f
require src/lib/test/fork-hold.f

T-RESET

package PROC-TREE

private

: FORK-UNDER-WALK ( -- )
   [: WALK-GET [: FORK-HOLD:HOLDING ;] [: WALK-RELEASE ;] finally ;] FORK-HOLD:HOLD
   s" a child forked under a held walk ended its query"
   [: getpid >PID CPU-NS drop ;] 1 FORK-HOLD:FORK-CHECK
   WALKING atomic@ {: walking:n :}
   FORK-HOLD:RELEASE
   s" the parent's walk was still the holder's" T-LABEL
   walking 1 T= ;

FORK-UNDER-WALK

;package

package FORK-CHILD-TEST

private

32 constant FORKS                        \ children forked into a lock taken over and over

TASK:MIN-STACK TASK:TASK SPARE           \ never activated: its exit chain is all it is for
DYNAMIC-BUFFER CHURN u8                  \ the holder's, reserved and released over and over
DYNAMIC-BUFFER FRESH u8                  \ each child's first reservation
create MARKED 1 cells allot              \ the holder's, declared over and over
create FIRST-MARK 1 cells allot          \ each child's first declaration

\ The holder registers one quotation again and again: after the first time the
\ chain already holds it, so each call is the lock, a look and the unlock.
: FORK-INTO-EXIT-LOCK ( -- )
   [: [: [: ;] SPARE TASK:AT-EXIT ;] FORK-HOLD:HAMMER ;] FORK-HOLD:HOLD
   s" children forked into a busy exit-chain lock registered a cleanup"
   [: [: ;] SPARE TASK:AT-EXIT ;] FORKS FORK-HOLD:FORK-CHECK
   FORK-HOLD:RELEASE ;

\ The parent makes the main thread's park and takes the wake it posted; the
\ child then parks and wakes as the parent did.
: FORK-AFTER-PARK ( -- )
   TASK:SELF TASK:WAKE TASK:STOP
   s" a child forked after the main thread's park was made parked on its own"
   [: TASK:SELF TASK:WAKE TASK:STOP ;] 1 FORK-HOLD:FORK-CHECK ;

: FORK-INTO-LIFECYCLE ( -- )
   [: [: IMAGE-LIFECYCLE:COUNT drop ;] FORK-HOLD:HAMMER ;] FORK-HOLD:HOLD
   s" children forked into a busy lifecycle lock counted its hooks"
   [: IMAGE-LIFECYCLE:COUNT drop ;] FORKS FORK-HOLD:FORK-CHECK
   FORK-HOLD:RELEASE ;

\ A buffer's first reservation and its release each take the registry's lock.
: CHURN-ONCE ( -- )
   1 CHURN-RESERVE
   CHURN-RELEASE ;

: FORK-INTO-DYNAMIC-STORAGE ( -- )
   [: [: CHURN-ONCE ;] FORK-HOLD:HAMMER ;] FORK-HOLD:HOLD
   s" children forked into a busy dynamic-storage registry reserved a buffer"
   [: 1 FRESH-RESERVE ;] FORKS FORK-HOLD:FORK-CHECK
   FORK-HOLD:RELEASE ;

\ Every declaration takes the registrar's lock, a repeated one included.
: FORK-INTO-REGISTRAR ( -- )
   [: [: MARKED ptr-cell-mark ;] FORK-HOLD:HAMMER ;] FORK-HOLD:HOLD
   s" children forked into a busy address-cell registrar declared a cell"
   [: FIRST-MARK ptr-cell-mark ;] FORKS FORK-HOLD:FORK-CHECK
   FORK-HOLD:RELEASE ;

FORK-INTO-EXIT-LOCK
FORK-AFTER-PARK
FORK-INTO-LIFECYCLE
FORK-INTO-DYNAMIC-STORAGE
FORK-INTO-REGISTRAR

T-REPORT
s" fork-child-test: ok" type cr

;package
