\ Throw and task halt close one frame for an entire initialized table.
require lib/test.f
require lib/memory.f
require lib/c2-memory.f
require lib/task.f
require test/c2-records-types.f

package C2-RECORDS-LIFECYCLE
private

TASK:MIN-STACK TASK:TASK WORKER
variable READY

: FAIL ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:pair> )
   -7351 throw ;

: THROW-BODY ( mut-view<b,l,a,u8> -- mut-view<b,l,a,u8> )
   4 3 5 C2--RECORDS--TYPES-PAIR:MAKE
   [: FAIL ;] C2-MEM:WITH-RECORDS ;

: THROW-CALL ( -- )
   64 MEM:BYTES-ALLOC-LEN [: THROW-BODY ;] C2-MEM:WITH-MUT ;

: PAUSE-INSIDE ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:pair> )
   1 READY atomic!
   begin TASK:PAUSE again ;

: HALT-BODY ( mut-view<b,l,a,u8> -- mut-view<b,l,a,u8> )
   4 3 5 C2--RECORDS--TYPES-PAIR:MAKE
   [: PAUSE-INSIDE ;] C2-MEM:WITH-RECORDS ;

: HALT-CALL ( -- )
   64 MEM:BYTES-ALLOC-LEN [: HALT-BODY ;] C2-MEM:WITH-MUT ;

: HALT-RESULT ( -- bool )
   0 READY atomic!
   ['] HALT-CALL WORKER TASK:ACTIVATE
   begin READY atomic@ 0= while TASK:PAUSE repeat
   WORKER TASK:HALT
   WORKER TASK:JOIN
   MATCH result
      ok OF drop false ENDOF
      err OF E-TASK-NO-RESULT = ENDOF
   ;MATCH ;

public

: RUN ( -- )
   T-RESET
   s" a thrown table callback preserves its error through cleanup" T-LABEL
   ['] THROW-CALL catch -7351 = TTRUE
   s" task halt drains a live table frame without per-element nesting" T-LABEL
   HALT-RESULT TTRUE
   T-REPORT
   s" c2-records-lifecycle: ok" type cr ;

;package

C2-RECORDS-LIFECYCLE:RUN
