\ A task operation at load time caches the MAKER's resolved addresses in package
\ FFI's symbol table for lib/task.f's FUNCTION: rows. That cache is process
\ state, so the stripped link must run IMAGE-LIFECYCLE:PREPARE - which is what
\ runs FFI's FORGET-SYMBOLS - before it captures, and the restored image must
\ resolve the symbols afresh on its own first task operation.
require lib/task.f

package STRIPPED-LIFECYCLE-PREPARE-SUBJECT
private

$4A constant FAILURE-RC

TASK:MIN-STACK TASK:TASK WORKER
variable RAN

: STEP ( -- ) 1 RAN +! ;

: CYCLE ( -- )
   0 RAN !
   ['] STEP WORKER TASK:ACTIVATE
   begin WORKER TASK:DONE? 0= while TASK:PAUSE repeat
   WORKER TASK:KILL ;

\ A WHOLE TASK CYCLE AT LOAD, which is both halves of this subject: it caches
\ FFI's resolved symbols for lib/task.f's FUNCTION: rows, and it leaves behind
\ the TCB a killed task leaves - mappings returned and the five process-local
\ cells cleared (lib/task.f TASK-RELEASE-MEM). Before that clear the stripped
\ link refused this file at the TCB cell holding the pthread_t, which no
\ lifecycle hook owned and which a `create … does>` body gives the refusal no
\ name for.
CYCLE

public

\ Only after the worker's body ran in THIS process: the symbols the capture
\ forgot have to be resolved afresh by this operation, and a stale address
\ would fail the activation instead.
: RUN ( -- )
   CYCLE
   RAN @ 1 <> if
      s" stripped-lifecycle-prepare: worker did not run" FAILURE-RC die
   then
   s" stripped-lifecycle-prepare: ok" type cr ;

;package
