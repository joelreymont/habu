\ task-left-main.f - its top level starts a task that waits forever, so the
\ task still runs at the file's end and the build refuses the file.

require src/lib/task.f

package FILE-OBJECTS-TASK-LEFT
public
TASK:MIN-STACK TASK:TASK WORKER
: PARKED ( -- ) begin TASK:WAIT again ;
;package

: MAIN ( -- ) ;

' FILE-OBJECTS-TASK-LEFT:PARKED FILE-OBJECTS-TASK-LEFT:WORKER TASK:ACTIVATE
