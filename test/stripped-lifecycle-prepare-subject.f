\ A task operation at load time caches the MAKER's dlsym addresses in
\ lib/task.f's eight XT cells (LOAD-SYMBOLS, through TASK-SYMBOLS). Those cells
\ are application data, so the stripped link must run IMAGE-LIFECYCLE:PREPARE -
\ which is what arms lib/task.f RESET-SYMBOLS - before it captures them, and the
\ restored image must load them afresh on its own first task operation.
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

\ A WHOLE TASK CYCLE AT LOAD, which is both halves of this subject: it caches the
\ maker's dlsym addresses in the eight XT cells, and it leaves behind the TCB a
\ killed task leaves - mappings returned and the five process-local cells cleared
\ (lib/task.f TASK-RELEASE-MEM). Before that clear the stripped link refused this
\ file at the TCB cell holding the pthread_t, which no lifecycle hook owned and
\ which a `create … does>` body gives the refusal no name for.
CYCLE

public

\ Only after the worker's body ran in THIS process: the eight XT cells the
\ capture zeroed have to be reloaded by this operation, and a stale one would
\ fail the activation instead.
: RUN ( -- )
   CYCLE
   RAN @ 1 <> if
      s" stripped-lifecycle-prepare: worker did not run" FAILURE-RC die
   then
   s" stripped-lifecycle-prepare: ok" type cr ;

;package

: MAIN ( -- )
   STRIPPED-LIFECYCLE-PREPARE-SUBJECT:RUN ;
