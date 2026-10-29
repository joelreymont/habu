\ A task operation at load time caches the MAKER's resolved addresses in package
\ FFI's symbol table for src/lib/task.f's FUNCTION: rows. That cache is process
\ state, so the stripped link must run IMAGE-LIFECYCLE:PREPARE - which is what
\ runs FFI's FORGET-SYMBOLS - before it captures, and the restored image must
\ resolve the symbols afresh on its own first task operation.
require src/lib/task.f

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

\ A whole task cycle at load, both halves of this subject: it caches FFI's
\ resolved symbols for src/lib/task.f's FUNCTION: rows, and leaves the TCB a
\ killed task leaves, mappings returned and the five process-local cells cleared
\ (src/lib/task.f TASK-RELEASE-MEM). Uncleared, the TCB cell holding the
\ pthread_t has no lifecycle hook, and in a `create … does>` body the refusal
\ has no name for it.
CYCLE

public

\ Only after the worker's body ran in this process: the symbols the capture
\ forgot have to be resolved afresh by this operation, and a stale address
\ would fail the activation instead.
: RUN ( -- )
   CYCLE
   RAN @ 1 <> if
      s" stripped-lifecycle-prepare: worker did not run" FAILURE-RC die
   then
   s" stripped-lifecycle-prepare: ok" type cr ;

;package
