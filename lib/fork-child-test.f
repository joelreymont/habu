\ fork-child-test.f - a forked child does not inherit another task's hold.
\
\     bin/hb --load lib/fork-child-test.f
\
\ fork copies only the thread that calls it. Each case holds one piece of
\ process-wide state in a task of its own (lib/test/fork-hold.f), forks, and
\ has the child reach that state through its owner's public words; the child
\ must end in time and exit 0.
\
\ THE WAYS THIS CAN FAIL, written down before the fix:
\
\  1. THE CHILD WAITS FOR EVER. A lock another task held at the fork is still
\     held in the child, and nothing there lets it go.
\  2. THE CHILD USES WHAT IT DID NOT INHERIT. On Darwin the main thread's park
\     is a Mach port, and ports do not cross a fork: in the child the name the
\     record holds is no semaphore, and the first wake throws E-TASK-THREAD.
\  3. THE PARENT LOSES ITS LOCK. A reset run on the parent's side of the fork
\     frees the holder's lock, and a third task could then take it beside it.
\  4. THE FORK WAITS FOR EVER. PROC-FORK:RAW holds the engine's three registry
\     locks while it forks; taken out of their nesting order, or never given
\     back, they leave the forking thread or the holder waiting. Nothing here
\     can end that wait: the case hangs, and the gate's row deadline reports it.
\
\ Whether RAW holds a lock across the fork or the child resets it is its
\ owner's call (lib/fork-child.f); a case shows only that the child finds the
\ state free. A held lock's other half - that the child never sees a table
\ another task was part way through republishing - lasts a few instructions,
\ which no fork here can be aimed at.
\
\ Every case forks from the main thread. A fork made on a task holds the
\ task-local address-cell lock, not the registrar's, until dot bb84a527:
\ ADDRESS-CELLS finds its lock through data-base, a task's own context there.
\
\ A holder takes its lock with the word its owner takes it with. PROC-TREE's
\ and SERIAL's are private, so those cases reopen their packages. TASK seals
\ its package, and IMAGE-LIFECYCLE, DYNAMIC-STORAGE and ADDRESS-CELLS are baked
\ into the engine (test/baked-owner.f), so none of them is reopened: their
\ holders take and free the lock through a public word over and over, and many
\ children fork into it. TASK's one-time park setup, which holds
\ MAIN-PARK-READY at 1 while it runs, has no public word that holds it at all;
\ the park case forks after the setup instead, which is the state Darwin cannot
\ carry across a fork, and the child's reset of the record is the same for both.

require lib/errors.f
require lib/test.f
require lib/task.f
require lib/image-lifecycle.f
require lib/process.f
require lib/process-fork.f
require lib/process-tree.f
require lib/serial.f
require lib/test/fork-hold.f

T-RESET

package PROC-TREE

private

: FORK-UNDER-WALK ( -- )
   [: WALK-GET [: FORK-HOLD:HOLDING ;] [: WALK-RELEASE ;] finally ;] FORK-HOLD:HOLD
   s" a child forked under a held walk ended its query"
   [: getpid >PID CPU-NS drop ;] 1 FORK-HOLD:FORK-CHECK
   s" the parent's walk was still the holder's" T-LABEL
   WALKING atomic@ 1 T=
   FORK-HOLD:RELEASE ;

FORK-UNDER-WALK

;package

package SERIAL

private

variable FORK-READY-WAS

\ INIT holds READY at 1 while it resolves the symbols, so a task opening the
\ first port holds it there.
: HOLD-SETUP ( -- )
   READY atomic@ FORK-READY-WAS !
   1 READY atomic!
   [: FORK-HOLD:HOLDING ;] [: FORK-READY-WAS @ READY atomic! ;] finally ;

\ No such device: the open fails, after INIT.
: OPEN-NONE ( -- )
   s" /nonexistent/fork-child-serial" 9600 BAUD OPEN8N1
   MATCH open-result
      opened OF HANDLE>N drop ENDOF
      failed OF ERRNO>N drop ENDOF
      unsupported OF ENDOF
   ;MATCH ;

: FORK-UNDER-SETUP ( -- )
   [: HOLD-SETUP ;] FORK-HOLD:HOLD
   s" a child forked while a task set serial up made its own setup"
   [: OPEN-NONE ;] 1 FORK-HOLD:FORK-CHECK
   FORK-HOLD:RELEASE ;

FORK-UNDER-SETUP

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
