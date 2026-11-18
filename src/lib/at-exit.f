split-str
\ at-exit.f - the actions a process runs when it exits. This file owns the
\ engine's process-exit vector: the engine clears it and calls it once just
\ before the exit_group of a normal top-level exit, a `die`, an uncaught throw
\ or a stripped program's return from MAIN, and the vector runs the actions.
\ A forked child keeps the actions and starts with no exit owner, so when it
\ exits it runs the actions not yet taken, even when it forked while another
\ thread ran them. A build's space loads a second copy, whose RUN goes to the
\ space's own vector cell, which only the linker calls.
\ STORAGE CLASS. PROCESS-WIDE: one list of action rows.

require src/lib/prelude.f
require src/lib/error.f
require src/core/rejection.f
require src/lib/arena.f

package AT-EXIT
private

\ An action's row: the address of the row added before it, 0 for none, and
\ the action.
STRUCTURE row DERIVE addr
   FIELD older n
   FIELD run [ -- ]
;STRUCTURE
ARENA ROWS row

CAST: >ROW ( n -- ^row )
CAST: ROW>N ( ^row -- n )
CAST: >ARENA ( n -- ^ARENA:arena )
CAST: ARENA>N ( ^ARENA:arena -- n )

\ The rows' arena, 0 until an ADD makes one, never released: its growth only
\ maps, so nothing under the lock can die, and no row moves under a TAKE.
variable POOL   POOL scratch-cell-mark

\ Atomic cells require native cell alignment.
here data-base - negate 7 and allot
\ NEWEST is the newest row not yet taken, 0 for none. Both are scratch numbers,
\ so a built program starts with no actions: a typed address cell is never scratch.
variable NEWEST   NEWEST scratch-cell-mark
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

\ The newest action, taken off the list, or false when none is left.
: TAKE ( -- [ -- ] bool )
   LOCK
   NEWEST @ {: at:n :}
   at 0= if UNLOCK [: ;] false exit then
   at >ROW {: r:^ :}
   r ROW-OLDER @ NEWEST !
   r ROW-RUN @
   UNLOCK true ;

\ RUNNER holds RUN, which the vector calls: a typed cell, so the native
\ compiler knows the call's convention, and never empty once this file loads.
TYPED-VARIABLE RUNNER [ -- ]

: ARM ( -- )
   VECTOR @ XT>N 0= if RUNNER @ VECTOR ! then ;

\ The engine clears the vector before it calls RUN, and RUN arms it again
\ before each action, which runs as a process root: an action that calls `die`
\ or throws re-enters RUN, the actions left still run, and the process ends
\ with that action's code.
: RUN ( -- )
   begin TAKE while
      ARM
      ERROR:TOP-LEVEL
   repeat
   drop ;

: RUNNER-INIT ( -- )
   [: RUN ;] RUNNER ! ;
RUNNER-INIT

: VECTOR-INIT ( -- )
   RUNNER @ VECTOR xt! ;
VECTOR-INIT

\ The row is written before NEWEST admits it with a store-release, so a child
\ forked mid-ADD has the whole action or none of it.
: APPEND ( [ -- ] -- ) {: q :}
   NEWEST @ begin dup 0<> while
      >ROW dup ROW-RUN @ XT>N q XT>N = if drop exit then
      ROW-OLDER @
   repeat drop
   POOL @ 0= if ARENA:NEW ARENA>N POOL ! then
   POOL @ >ARENA ROWS-NEW {: r:^ :}
   q r ROW-RUN !
   NEWEST @ r ROW-OLDER !
   r ROW>N NEWEST atomic! ;

public

\ Adds an action the process runs once when any thread exits or RUN-NOW runs,
\ after every action added later; an action already added is not added again.
\ An action must not wait on another thread: one that exits while an exit runs
\ the actions blocks until the process ends.
: ADD ( [ -- ] -- )
   LOCK [: APPEND ;] [: UNLOCK ;] finally
   ARM ;

\ The lock, and POOL: a child forked mid-allocation has the arena half changed,
\ so its next ADD makes its own.
: CHILD-RESET ( -- ) 0 POOL ! UNLOCK ;

private

: REPORT ( n -- )
   {: code:n :}
   code 0= if exit then
   REJECTION:RELEASE
   code ERROR:UNCAUGHT ;

public

\ As RUN, for a process about to end some other way: a throw is reported on fd 2
\ and the rest still run; a thread that exits meanwhile ends the process itself.
: RUN-NOW ( -- )
   begin TAKE while
      ARM
      catch REPORT
   repeat
   drop ;

;package
