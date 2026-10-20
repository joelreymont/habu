\ Repeated references to one shared DATA cell cross control-flow, quotation,
\ exception and Task boundaries in the stripped image.
require src/lib/task.f

package STRIPPED-ADDRESS-POOL-SUBJECT
private

$4A constant FAILURE-RC
variable SHARED
TASK:MIN-STACK TASK:TASK WORKER

: EXPECT ( bool -- )
   0= if s" stripped-address-pool: mismatch" FAILURE-RC die then ;

: WORK ( -- )
   1 SHARED atomic-add drop
   0 TASK:RETURN ;

: BRANCH ( n -- n )
   SHARED @ drop
   SHARED @ drop
   dup 0 > if SHARED @ + else SHARED @ - then ;

: INNER ( -- n )
   [: SHARED @ ;] execute ;

: NESTED ( -- n )
   SHARED @ drop
   [: INNER SHARED @ + ;] execute ;

: FAIL ( -- )
   SHARED @ drop
   17 throw ;

: AFTER-THROW ( -- bool )
   [: FAIL ;] catch 17 =
   SHARED @ 42 = and ;

public
: RUN ( -- )
   41 SHARED !
   ['] WORK WORKER TASK:ACTIVATE
   WORKER TASK:JOIN MATCH result
      ok OF 0 = EXPECT ENDOF
      err OF drop false EXPECT ENDOF
   ;MATCH
   WORKER TASK:KILL
   0 BRANCH -42 = EXPECT
   1 BRANCH 43 = EXPECT
   NESTED 84 = EXPECT
   AFTER-THROW EXPECT
   s" stripped-address-pool: ok" type cr ;
;package
