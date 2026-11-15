\ at-exit.f - the actions a process runs when it exits. This file owns the
\ engine's process-exit vector: the engine clears it and calls it once just
\ before the exit_group of a normal top-level exit, a `die`, an uncaught throw
\ or a stripped program's return from MAIN, and the vector runs the actions.
\ A forked child keeps the actions and starts with no exit owner, so when it
\ exits it runs the actions not yet taken, even when it forked while another
\ thread ran them. A build's space loads a second copy, whose RUN goes to the
\ space's own vector cell, which only the linker calls.
\ STORAGE CLASS. PROCESS-WIDE action table.

require src/lib/prelude.f
require src/lib/error.f

package AT-EXIT
private

\ ACTIONS holds N actions, oldest first; a 65th refuses E-LAYOUT-BOUNDS. A built
\ program starts with none: N is a scratch cell, written as 0 in the target.
64 constant CAP
CAP TYPED-BUFFER ACTIONS [ -- ]

\ Atomic cells require native cell alignment.
here data-base - negate 7 and allot
variable N   N scratch-cell-mark
variable MUTEX

: LOCK ( -- )
   begin 0 1 MUTEX atomic-cas 0= until ;

: UNLOCK ( -- ) 0 MUTEX atomic! ;

\ The vector is an engine DATA cell holding a code pointer; only a private CAST:
\ gives an address a quotation pointee.
CAST: >VECTOR ( n -- ^[ -- ] )
CAST: XT>N ( [ -- ] -- n )

: VECTOR ( -- ^[ -- ] )
   BOOT:EXIT-VECTOR NULL-PTR - >VECTOR ;

\ The newest action, taken off the table, or false when none is left.
: TAKE ( -- [ -- ] bool )
   LOCK
   N @ 1- {: last:n :}
   last 0< if UNLOCK [: ;] false exit then
   last N !
   last ACTIONS @
   UNLOCK true ;

\ RUNNER holds RUN, which the vector calls: a typed cell, so the native
\ compiler knows the call's convention, and never empty once this file loads.
TYPED-VARIABLE RUNNER [ -- ]

: ARM ( -- )
   VECTOR @ XT>N 0= if RUNNER @ VECTOR ! then ;

\ The engine clears the vector before it calls RUN, and RUN arms it again
\ before each action: an action that calls `die` or throws re-enters RUN, the
\ actions left still run, and the process ends with that action's code.
: RUN ( -- )
   begin TAKE while
      ARM
      execute
   repeat
   drop ;

: RUNNER-INIT ( -- )
   [: RUN ;] RUNNER ! ;
RUNNER-INIT

: VECTOR-INIT ( -- )
   RUNNER @ VECTOR xt! ;
VECTOR-INIT

\ The slot is stored before the count that admits it, and the count with a
\ store-release, so a fork in the middle of an ADD leaves the child the whole
\ action or none of it.
: APPEND ( [ -- ] -- ) {: q :}
   N @ 0 ?do
      i ACTIONS @ XT>N q XT>N = if unloop exit then
   loop
   N @ CAP = if E-LAYOUT-BOUNDS throw then
   q N @ ACTIONS !
   N @ 1+ N atomic! ;

public

\ Adds an action the process runs once when any thread exits, after every
\ action added later; an action already added is not added again. An action
\ must not wait on another thread: one that exits while the actions run blocks
\ until the process ends.
: ADD ( [ -- ] -- )
   LOCK [: APPEND ;] [: UNLOCK ;] finally
   ARM ;

\ The lock alone: APPEND publishes its count after the slot, and TAKE counts
\ down before it reads.
: CHILD-RESET ( -- ) UNLOCK ;

;package
