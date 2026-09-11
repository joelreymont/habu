\ Process resources are reset before an executable image captures live data.
require lib/prelude.f
\ HOOKS below is a checked quotation store; the optimizing tier lowers such a
\ store through QUOTATION-STORAGE:STORE, so this file owns that dependency.
require src/core/quotation-storage.f

package IMAGE-LIFECYCLE
private

DYNAMIC-BUFFER HOOKS [ -- ]
variable COUNT
\ Atomic cells require native cell alignment. This dictionary storage is shared
\ by every task, unlike the engine's per-task DATA header.
here data-base - negate 7 and allot
variable MUTEX

: LOCK ( -- )
   begin 0 1 MUTEX atomic-cas 0= until ;

: UNLOCK ( -- ) 0 MUTEX atomic! ;

: APPEND ( [ -- ] -- )
   COUNT @ 1+ HOOKS-RESERVE
   COUNT @ HOOKS !
   1 COUNT +! ;

public

\ Register when acquiring process-local state. Serialize growth and append;
\ allocation failure must also release the lock for the next registration.
: REGISTER ( [ -- ] -- )
   LOCK [: APPEND ;] [: UNLOCK ;] finally ;

private


\ Cleanup may register another resource. Keep those entries when removing
\ the completed callback, and retain the original entry if it throws.
: REMOVE ( n -- )
   1+ COUNT @ swap ?do
      i HOOKS @ i 1- HOOKS !
   loop
   -1 COUNT +! ;

public


\ Capture runs after application tasks stop. Reverse order releases dependents
\ first; cleanup can register again, and a failed callback remains for retry.
: PREPARE ( -- )
   begin COUNT @ 0 > while
      COUNT @ 1- {: at:n :}
      at HOOKS @ execute
      at REMOVE
   repeat
   HOOKS-RELEASE ;

;package
