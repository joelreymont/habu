\ A throwing or halted field callback closes the borrowed span and its owners.
require lib/test.f
require lib/memory.f
require lib/c2-memory.f
require lib/task.f
require test/c2-field-loan-wrapper.f

package C2-FIELD-LOAN-LIFECYCLE
private

TASK:MIN-STACK TASK:TASK WORKER
variable READY

: FAIL ( mut-view<b,j,c,init<i,C2-FIELD-LOAN-WRAPPER:pair>> -- mut-view<b,j,c,init<i,C2-FIELD-LOAN-WRAPPER:pair>> )
   -7361 throw ;

: THROW-OUTER ( mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> -- mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> )
   [: FAIL ;] C2-MEM:WITH-FIELD inner ;

: THROW-BODY ( mut-view<b,l,a,u8> -- mut-view<b,l,a,u8> )
   17 11 29 C2--FIELD--LOAN--WRAPPER-PAIR:MAKE
   73 C2--FIELD--LOAN--WRAPPER-OUTER:MAKE
   [: THROW-OUTER ;] C2-MEM:WITH-INIT ;

: THROW-CALL ( -- )
   32 MEM:BYTES-ALLOC-LEN [: THROW-BODY ;] C2-MEM:WITH-MUT ;

: THROWS-CLOSE? ( -- bool )
   40 0 do
      ['] THROW-CALL catch -7361 <> if false unloop exit then
   loop true ;

: PAUSE-INSIDE ( mut-view<b,j,c,init<i,C2-FIELD-LOAN-WRAPPER:pair>> -- mut-view<b,j,c,init<i,C2-FIELD-LOAN-WRAPPER:pair>> )
   1 READY atomic!
   begin TASK:PAUSE again ;

: HALT-OUTER ( mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> -- mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> )
   [: PAUSE-INSIDE ;] C2-MEM:WITH-FIELD inner ;

: HALT-BODY ( mut-view<b,l,a,u8> -- mut-view<b,l,a,u8> )
   17 11 29 C2--FIELD--LOAN--WRAPPER-PAIR:MAKE
   73 C2--FIELD--LOAN--WRAPPER-OUTER:MAKE
   [: HALT-OUTER ;] C2-MEM:WITH-INIT ;

: HALT-CALL ( -- )
   32 MEM:BYTES-ALLOC-LEN [: HALT-BODY ;] C2-MEM:WITH-MUT ;

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
   s" forty thrown field callbacks release every owner frame" T-LABEL
   THROWS-CLOSE? TTRUE
   s" task halt drains a live field loan" T-LABEL
   HALT-RESULT TTRUE
   T-REPORT
   s" c2-field-loan-lifecycle: ok" type cr ;
;package

C2-FIELD-LOAN-LIFECYCLE:RUN
