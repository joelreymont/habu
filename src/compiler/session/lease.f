\ lease.f - one scoped admission to the legacy native compiler's shared state.

require lib/prelude.f
require lib/errors.f

package NLEASE
public

NEWTYPE lease 0

-9430 constant E-NLEASE-FIRST
E-NLEASE-FIRST constant E-BUSY
-9431 constant E-TASK
-9432 constant E-STATE
-9439 constant E-NLEASE-LAST

private

CAST: MINT ( n -- NLEASE:lease )
CAST: SERIAL ( NLEASE:lease -- n )

variable NEXT
variable CURRENT
variable WORKING

: TASK-CK ( -- )
   data-base TASKS-LIVE-CELL + @ 0 <> if E-TASK throw then ;

: QUIET-CK ( -- )
   TASK-CK
   CURRENT @ 0 <> if E-BUSY throw then ;

: ACQUIRE ( -- NLEASE:lease )
   QUIET-CK
   NEXT @ $7FFFFFFFFFFFFFFF = if E-STATE throw then
   NEXT @ 1+ dup NEXT ! dup CURRENT ! MINT ;

: RELEASE ( -- )
   0 WORKING !
   0 CURRENT ! ;

: WORK-END ( -- )
   0 WORKING ! ;

public

: IDLE-CK ( -- )
   QUIET-CK ;

: CHECK ( NLEASE:lease -- )
   TASK-CK
   SERIAL dup 0= swap CURRENT @ <> or if E-STATE throw then ;

: WORK-CK ( NLEASE:lease -- )
   CHECK
   WORKING @ 0= if E-STATE throw then ;

: QUIET ( NLEASE:lease -- )
   CHECK
   WORKING @ 0 <> if E-BUSY throw then ;

: LIVE? ( NLEASE:lease -- bool )
   SERIAL dup 0 <> swap CURRENT @ = and ;

\ Admission precedes all compiler mutation. A refused child never installs
\ cleanup and cannot release the parent's lease or its active provider work.
: WITH ( R [ R NLEASE:lease -- S ] -- S )
   {: body :}
   ACQUIRE body [: RELEASE ;] finally ;

: WORK ( R NLEASE:lease [ R -- S ] -- S )
   {: body :}
   CHECK
   WORKING @ 0 <> if E-BUSY throw then
   -1 WORKING !
   body [: WORK-END ;] finally ;

;package
