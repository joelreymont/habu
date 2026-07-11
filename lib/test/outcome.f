\ outcome.f - checked assertions over the process outcome sum.
\ One MATCH per assert so every consumer of the -OUTCOME capture API asserts
\ completion by variant (exit code / signal / timeout) without kind ints.

require lib/errors.f
require lib/test/assert.f
require lib/process.f

: T-OUTCOME-EXITED= ( outcome n -- ) {: want:n :}   \ completed by exit with this code
   MATCH outcome
     exited OF want T= ENDOF
     signaled OF drop 1 0 T= ENDOF
     timeout OF 1 0 T= ENDOF
   ;MATCH ;

: T-OUTCOME-SIGNALED= ( outcome n -- ) {: want:n :}   \ died by this signal
   MATCH outcome
     exited OF drop 1 0 T= ENDOF
     signaled OF want T= ENDOF
     timeout OF 1 0 T= ENDOF
   ;MATCH ;

: T-OUTCOME-TIMEOUT ( outcome -- )   \ hit the capture deadline
   MATCH outcome
     exited OF drop 1 0 T= ENDOF
     signaled OF drop 1 0 T= ENDOF
     timeout OF 0 0= TTRUE ENDOF
   ;MATCH ;
