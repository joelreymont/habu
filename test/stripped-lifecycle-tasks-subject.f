\ A task PREPAREd at load and never activated holds stack, return, loop and
\ region mappings this process took, which no image restores: the capture sweep
\ src/lib/task.f registers releases them and leaves the task EMPTY before the
\ stripped link reads the window. The restored image activates the task again
\ and reads main's script arguments: an application's convention is
\ process-wide.
require src/lib/task.f
require src/lib/adt/result.f

package STRIPPED-LIFECYCLE-TASKS-SUBJECT
private

$4A constant FAILURE-RC
$2A constant ANSWER

$30 constant POOL-N
$81 constant HOLD-N                  \ one more task than AT-EXIT table rows
TASK:MIN-STACK TASK:TASK WORKER
POOL-N TYPED-BUFFER POOL ptr n
HOLD-N TYPED-BUFFER HOLD ptr n
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
\ The chunks are this process's mappings: the sweep gives them back and empties
\ the pool, so the image's own TASK:NEW maps afresh. A slot is cleared once its
\ task is given back: the handle is dead, and the capture unmaps the record it
\ names. The first task's cleanup row goes onto the free AT-EXIT rows when it is
\ given back, so the image gets a non-empty free list, which its own run
\ registers from.
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

\ Tasks that never run, each holding a cleanup: with no row given back before,
\ they fill the AT-EXIT table and take a row past it. Freed in order, the last
\ leaves its chunk row at the head of the free rows the capture sweeps. The
\ worker's cleanup, registered while they are held, takes a row past the table
\ in a chunk this process mapped; the capture drops the free chunk rows and
\ moves the held one into the table the image carries.
: HOLD-ROWS ( -- )
   HOLD-N 0 ?do
      TASK:MIN-STACK TASK:NEW dup i HOLD !
      ['] POOL-EXIT-STEP swap TASK:AT-EXIT
   loop ;

: HOLD-FREE ( -- )
   HOLD-N 0 ?do i HOLD @ TASK:FREE NULL-PTR i HOLD ! loop ;

: ARM ( -- )
   WORKER TASK:PREPARE
   HOLD-ROWS
   ['] EXIT-STEP WORKER TASK:AT-EXIT
   HOLD-FREE
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
   HOLD-ROWS HOLD-FREE
   POOL-RUN
   s" stripped-lifecycle-tasks: ok" type cr ;

;package
