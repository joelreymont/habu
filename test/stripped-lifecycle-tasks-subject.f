\ A task PREPAREd at load time and never activated. Its TCB holds the stack,
\ return, loop and region mappings THIS process took, which no image restores, so
\ the capture sweep lib/task.f registers has to release them and leave the task
\ EMPTY before the stripped link reads the window. The restored image then
\ activates the same task, which prepares mappings of its own.
require lib/task.f
require lib/adt/result.f

package STRIPPED-LIFECYCLE-TASKS-SUBJECT
private

$4A constant FAILURE-RC
$2A constant ANSWER

TASK:MIN-STACK TASK:TASK WORKER
variable RAN

: STEP ( -- )
   1 RAN +!
   ANSWER TASK:RETURN ;

: ARM ( -- )
   WORKER TASK:PREPARE ;

ARM

: ANSWERED? ( result<n,n> -- bool )
   MATCH result
     ok  OF ANSWER = ENDOF
     err OF drop false ENDOF
   ;MATCH ;

public

\ The image's own first use of the task: an ACTIVATE on a TCB the sweep emptied
\ prepares it again, and the worker's answer proves the task ran in THIS process.
: RUN ( -- )
   0 RAN !
   ['] STEP WORKER TASK:ACTIVATE
   WORKER TASK:JOIN ANSWERED? 0= if
      s" stripped-lifecycle-tasks: worker did not answer" FAILURE-RC die
   then
   WORKER TASK:KILL
   RAN @ 1 <> if
      s" stripped-lifecycle-tasks: worker did not run" FAILURE-RC die
   then
   s" stripped-lifecycle-tasks: ok" type cr ;

;package

: MAIN ( -- )
   STRIPPED-LIFECYCLE-TASKS-SUBJECT:RUN ;
