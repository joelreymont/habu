\ Tier 1 first: quotation-store lowering belongs to the optimizing backend
\ under test.
1 set-tier

require lib/test.f
require lib/task.f
require lib/image-lifecycle.f

package IMAGE-LIFECYCLE-TASK-TEST
private

here data-base - negate 7 and allot
variable READY
variable START
variable CLEANED
TASK:MIN-STACK TASK:TASK WORKER-A
TASK:MIN-STACK TASK:TASK WORKER-B
TASK:MIN-STACK TASK:TASK WORKER-C
TASK:MIN-STACK TASK:TASK WORKER-D
\ Inspected only by the main thread after its worker has stopped.
: ADDRESS-ROWS ( -- n ) data-base SNAP-RELOC:XTCELL-N-CELL + @ ;

\ A typed quotation cell owns its relocation row from its definition on
\ (src/core/layout-buffer.f STORAGE-ALLOT); the stores below reuse that row.
ADDRESS-ROWS constant ROWS-BEFORE-SHARED
TYPED-VARIABLE SHARED-ACTION [ -- ]
ADDRESS-ROWS ROWS-BEFORE-SHARED - constant SHARED-ROWS
TASK:#USER CELL TASK:+USER LOCAL-CELL drop

\ The task allocator returns a raw cell; this test declares its quotation type.
TRUSTED: LOCAL-ACTION ( -- ptr [ -- ] ) LOCAL-CELL ;

: CLEAN ( -- ) 1 CLEANED +! ;

: EMPTY-WORK ( -- ) ;

: LOCAL-WORK ( -- )
   [: CLEAN ;] LOCAL-ACTION !
   LOCAL-ACTION @ execute ;

: SHARED-WORK ( -- )
   [: CLEAN ;] SHARED-ACTION !
   SHARED-ACTION @ execute ;

: REGISTER-MANY ( -- )
   1 READY atomic-add drop
   begin START atomic@ 0= while TASK:PAUSE repeat
   1024 0 ?do [: CLEAN ;] IMAGE-LIFECYCLE:REGISTER loop ;

: JOIN ( ptr n -- ) {: worker:ptr :}
   begin worker TASK:DONE? 0= while TASK:PAUSE repeat
   worker TASK:KILL ;

: STORE-OWNERS ( -- )
   \ Establish the TCB's own dispatch rows before measuring application stores.
   ['] EMPTY-WORK WORKER-A TASK:ACTIVATE WORKER-A JOIN
   ADDRESS-ROWS {: before:n :}
   SHARED-ROWS 1 T=
   0 CLEANED !
   ['] LOCAL-WORK WORKER-A TASK:ACTIVATE WORKER-A JOIN
   CLEANED @ 1 T=
   ADDRESS-ROWS before T=
   \ The shared cell's one row was declared with it; a store adds none.
   ['] SHARED-WORK WORKER-A TASK:ACTIVATE WORKER-A JOIN
   CLEANED @ 2 T=
   ADDRESS-ROWS before T=
   \ Repeated stores keep that one row, and the main thread sees the callback.
   ['] SHARED-WORK WORKER-A TASK:ACTIVATE WORKER-A JOIN
   SHARED-ACTION @ execute
   CLEANED @ 4 T=
   ADDRESS-ROWS before T= ;

: ROUND ( -- )
   0 READY ! 0 START ! 0 CLEANED !
   ['] REGISTER-MANY WORKER-A TASK:ACTIVATE
   ['] REGISTER-MANY WORKER-B TASK:ACTIVATE
   ['] REGISTER-MANY WORKER-C TASK:ACTIVATE
   ['] REGISTER-MANY WORKER-D TASK:ACTIVATE
   begin READY atomic@ 4 < while TASK:PAUSE repeat
   1 START atomic!
   WORKER-A JOIN WORKER-B JOIN WORKER-C JOIN WORKER-D JOIN
   IMAGE-LIFECYCLE:PREPARE
   CLEANED @ 4096 T=
   IMAGE-LIFECYCLE:PREPARE CLEANED @ 4096 T= ;

: RUN ( -- )
   T-RESET
   s" task-local quotations stay transient; shared stores register once" T-LABEL
   STORE-OWNERS
   s" concurrent registration preserves every callback through growth and reuse" T-LABEL
   3 0 ?do ROUND loop
   T-REPORT ;

RUN
;package
