\ A task PREPAREd at load time and never activated. Its TCB holds the stack,
\ return, loop and region mappings THIS process took, which no image restores, so
\ the capture sweep src/lib/task.f registers has to release them and leave the task
\ EMPTY before the stripped link reads the window. The restored image then
\ activates the same task, which prepares mappings of its own, and reads the
\ script arguments main reads: an application's convention is process-wide.
\ Tasks TASK:NEW made, ran and gave back at load leave a pool of records in
\ chunks THIS process mapped; the sweep gives the chunks back and empties the
\ pool, so the image's own TASK:NEW maps afresh.
require src/lib/task.f
require src/lib/adt/result.f

package STRIPPED-LIFECYCLE-TASKS-SUBJECT
private

$4A constant FAILURE-RC
$2A constant ANSWER

$30 constant POOL-N
TASK:MIN-STACK TASK:TASK WORKER
POOL-N TYPED-BUFFER POOL ptr n
variable RAN
variable EXIT-RAN
variable POOL-EXIT-RAN
variable MAIN-ARGC

: EXIT-STEP ( -- ) 1 EXIT-RAN +! ;

: STEP ( -- )
   1 RAN +!
   SCRIPT-ARGC MAIN-ARGC @ <> if 0 TASK:RETURN exit then
   ANSWER TASK:RETURN ;

: POOL-STEP ( -- ) ANSWER TASK:RETURN ;

: POOL-EXIT-STEP ( -- ) 1 POOL-EXIT-RAN +! ;

: ANSWERED? ( result<n,n> -- bool )
   MATCH result
     ok  OF ANSWER = ENDOF
     err OF drop false ENDOF
   ;MATCH ;

\ More tasks than one pool chunk holds, all running at once, then given back.
\ A slot is cleared once its task is given back: the handle is dead, and the
\ capture unmaps the record it names. The first task's cleanup row goes onto
\ the free AT-EXIT rows when it is given back, so the load-time run leaves the
\ image a non-empty free list, which the image's own run registers from.
: POOL-RUN ( -- )
   0 POOL-EXIT-RAN !
   POOL-N 0 ?do TASK:MIN-STACK TASK:NEW i POOL ! loop
   ['] POOL-EXIT-STEP 0 POOL @ TASK:AT-EXIT
   POOL-N 0 ?do ['] POOL-STEP i POOL @ TASK:ACTIVATE loop
   true
   POOL-N 0 ?do
      i POOL @ TASK:JOIN ANSWERED? and
      i POOL @ TASK:FREE
      NULL-PTR i POOL !
   loop
   0= if s" stripped-lifecycle-tasks: pool task did not run" FAILURE-RC die then
   POOL-EXIT-RAN @ 1 <> if
      s" stripped-lifecycle-tasks: pool exit callback lost" FAILURE-RC die
   then ;

: ARM ( -- )
   WORKER TASK:PREPARE
   ['] EXIT-STEP WORKER TASK:AT-EXIT
   POOL-RUN ;

ARM

public

\ The image's own first use of the task: an ACTIVATE on a TCB the sweep emptied
\ prepares it again, and the worker's answer proves the task ran in THIS process.
: RUN ( -- )
   0 RAN !
   0 EXIT-RAN !
   SCRIPT-ARGC MAIN-ARGC !
   ['] STEP WORKER TASK:ACTIVATE
   WORKER TASK:JOIN ANSWERED? 0= if
      s" stripped-lifecycle-tasks: worker did not answer or lost the arguments" FAILURE-RC die
   then
   WORKER TASK:KILL
   RAN @ 1 <> if
      s" stripped-lifecycle-tasks: worker did not run" FAILURE-RC die
   then
   EXIT-RAN @ 1 <> if
      s" stripped-lifecycle-tasks: exit callback lost" FAILURE-RC die
   then
   POOL-RUN
   s" stripped-lifecycle-tasks: ok" type cr ;

;package
