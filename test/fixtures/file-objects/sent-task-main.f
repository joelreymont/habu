\ sent-task-main.f - its top level runs a task that sends a rejection, which
\ fails the build though main sent none.

require src/lib/task.f

TASK:MIN-STACK TASK:TASK SENDER
: SEND ( -- ) s" E-SENT-TASK" REJECTION:OPEN  s" a task sent it" REJECTION:EMIT-STR  REJECTION:KEEP ;
: GO ( -- ) [: SEND ;] SENDER TASK:ACTIVATE
   begin SENDER TASK:DONE? 0= while TASK:PAUSE repeat  SENDER TASK:KILL ;
GO

: MAIN ( -- ) ;
