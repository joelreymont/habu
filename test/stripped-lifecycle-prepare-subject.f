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

\ TASK:PAUSE on the main thread is sched_yield and nothing else (lib/task.f
\ PAUSE), so it reaches TASK-SYMBOLS - the point of this line - while leaving no
\ mapping behind: an activated worker's TCB keeps its stack pointer past the
\ join, and that cell, which no lifecycle hook owns, would be refused first.
TASK:PAUSE

public

\ Only after the worker's body ran in THIS process: the eight XT cells the
\ capture zeroed have to be reloaded by this operation, and a stale one would
\ fail the activation instead.
: RUN ( -- )
   0 RAN !
   ['] STEP WORKER TASK:ACTIVATE
   begin WORKER TASK:DONE? 0= while TASK:PAUSE repeat
   WORKER TASK:KILL
   RAN @ 1 <> if
      s" stripped-lifecycle-prepare: worker did not run" FAILURE-RC die
   then
   s" stripped-lifecycle-prepare: ok" type cr ;

;package

: MAIN ( -- )
   STRIPPED-LIFECYCLE-PREPARE-SUBJECT:RUN ;
