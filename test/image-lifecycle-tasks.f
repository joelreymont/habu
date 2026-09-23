\ Tier 1 first: quotation-store lowering belongs to the optimizing backend
\ under test.
1 set-tier

require lib/test.f
require lib/task.f
require lib/image-lifecycle.f
require lib/ffi-abi.f
require src/habu/task-abi.f

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

\ ---- the TCB a capture reaches, and what a killed task leaves in it ----------
\ THE TCB IS RAW STORAGE, so these rows read it as cells at named offsets. Three
\ of the five cleared cells have a TASK-ABI row because the engine's task entry
\ loads them; TCB.THREAD and TCB.RET have none - nothing in the engine reads a
\ pthread_t or a join's answer - so they are counted from the rows on either
\ side, which lib/task.f's own layout check pins against TASK-ABI.
TASK-ABI:XT-OFF CELL + constant THREAD-OFF
TASK-ABI:STATUS-OFF CELL 2 * + constant RET-OFF

: TCB@ ( ptr n n -- n ) {: tcb:ptr off:n :}
   tcb off + @ ;

: STATE@ ( ptr n -- n )
   TASK-ABI:STATUS-OFF TCB@ ;

\ The cell one past the TCB: this task's chain link, holding the address of the
\ TCB declared before it (lib/task.f, TASK-CHAIN).
: LINK@ ( ptr n -- n )
   TASK-ABI:TCB-BYTES TCB@ ;

: TCB-N ( ptr n -- n )
   FFI:>CELL ;

\ THE CHAIN IS EVERY TCB A PROGRAM DECLARED AND NOTHING BEFORE THEM: newest
\ first, and the first worker this file declared ends it. lib/task.f is loaded by
\ every one of its users and declares no task of its own, so nothing but the
\ program's own workers is ever on the chain the capture sweep walks.
: CHAIN-ROWS ( -- )
   WORKER-D LINK@ WORKER-C TCB-N T=
   WORKER-C LINK@ WORKER-B TCB-N T=
   WORKER-B LINK@ WORKER-A TCB-N T=
   WORKER-A LINK@ 0 T= ;

\ A PREPARED TASK HOLDS THIS PROCESS'S MAPPINGS AND NO THREAD, so the capture
\ gives them back and leaves the task EMPTY - and an EMPTY task is one ACTIVATE
\ away from running, which is what the next two lines run.
: PREPARED-RELEASED ( -- )
   WORKER-B TASK:PREPARE
   WORKER-B STATE@ TASK-ABI:CONSTRUCTED T=
   IMAGE-LIFECYCLE:PREPARE
   WORKER-B STATE@ TASK-ABI:EMPTY T=
   0 CLEANED !
   ['] LOCAL-WORK WORKER-B TASK:ACTIVATE WORKER-B JOIN
   CLEANED @ 1 T=
   WORKER-B STATE@ TASK-ABI:EMPTY T= ;

\ AFTER THE KILL THE TCB HOLDS NOTHING THIS PROCESS TOOK: the thread handle, the
\ join's answer, and the data base, record count and code end PREPARE recorded
\ are all zero, which is what lets a task that ran at load time be captured at
\ all (lib/task.f TASK-RELEASE-MEM). The activated TCB is read first: the
\ handle pthread_create stored and the data base PREPARE recorded are set, so
\ the offsets reach the cells the kill clears, not a cell that was zero all
\ along. Both are the main thread's own stores; the worker never writes them.
: KILLED-CLEARED ( -- )
   ['] EMPTY-WORK WORKER-A TASK:ACTIVATE
   WORKER-A THREAD-OFF TCB@ 0 T<>
   WORKER-A TASK-ABI:DBASE-OFF TCB@ 0 T<>
   WORKER-A JOIN
   WORKER-A STATE@ TASK-ABI:EMPTY T=
   WORKER-A THREAD-OFF TCB@ 0 T=
   WORKER-A RET-OFF TCB@ 0 T=
   WORKER-A TASK-ABI:DBASE-OFF TCB@ 0 T=
   WORKER-A TASK-ABI:NDICT-OFF TCB@ 0 T=
   WORKER-A TASK-ABI:CP-OFF TCB@ 0 T= ;

: RUN ( -- )
   T-RESET
   s" task-local quotations stay transient; shared stores register once" T-LABEL
   STORE-OWNERS
   s" concurrent registration preserves every callback through growth and reuse" T-LABEL
   3 0 ?do ROUND loop
   s" every declared TCB is on the chain the capture sweep walks" T-LABEL
   CHAIN-ROWS
   s" a capture releases a prepared task and leaves it ready to activate" T-LABEL
   PREPARED-RELEASED
   s" a killed task's TCB keeps no address this process took" T-LABEL
   KILLED-CLEARED
   T-REPORT ;

RUN
;package
