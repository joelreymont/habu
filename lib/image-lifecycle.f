\ Process resources are reset before an executable image captures live data.
require lib/prelude.f

package IMAGE-LIFECYCLE
private

DYNAMIC-BUFFER HOOKS [ -- ]
variable COUNT

public


\ Register when acquiring process-local state. Cleanup resets the owner's
\ initialization flag so first use in a restored process acquires it again.
: REGISTER ( [ -- ] -- )
   COUNT @ 1+ HOOKS-RESERVE
   COUNT @ HOOKS !
   1 COUNT +! ;

private


\ Cleanup may register another resource. Keep those entries when removing
\ the completed callback, and retain the original entry if it throws.
: REMOVE ( n -- )
   1+ COUNT @ swap ?do
      i HOOKS @ i 1- HOOKS !
   loop
   -1 COUNT +! ;

public


\ Reverse registration order releases dependents first. A failed cleanup
\ aborts capture and remains registered for retry.
: PREPARE ( -- )
   begin COUNT @ 0 > while
      COUNT @ 1- {: at:n :}
      at HOOKS @ execute
      at REMOVE
   repeat
   HOOKS-RELEASE ;

;package
