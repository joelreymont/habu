\ outcome.f - checked assertions over the process outcome sum.
\ One MATCH per assert so every consumer of the -OUTCOME capture API asserts
\ completion by variant (exit code / signal / timeout) without kind ints.
\ A deadline is no verdict on the child: an assert that wants an exit or a
\ signal throws E-PROC-TIMEOUT when the capture's deadline expired instead of
\ failing, so the row reports a timeout (lib/process.f, the outcome sum).
\ T-OUTCOME-TIMEOUT is the assert for a test that expects the deadline.

require lib/errors.f
require lib/test/assert.f
require lib/process.f

: T-OUTCOME-EXITED= ( outcome n -- ) {: want:n :}   \ completed by exit with this code
   MATCH outcome
     exited OF want T= ENDOF
     signaled OF drop 1 0 T= ENDOF
     timeout OF E-PROC-TIMEOUT throw ENDOF
   ;MATCH ;

: T-OUTCOME-SIGNALED= ( outcome n -- ) {: want:n :}   \ died by this signal
   MATCH outcome
     exited OF drop 1 0 T= ENDOF
     signaled OF want T= ENDOF
     timeout OF E-PROC-TIMEOUT throw ENDOF
   ;MATCH ;

: T-OUTCOME-TIMEOUT ( outcome -- )   \ hit the capture deadline
   MATCH outcome
     exited OF drop 1 0 T= ENDOF
     signaled OF drop 1 0 T= ENDOF
     timeout OF 0 0= TTRUE ENDOF
   ;MATCH ;
