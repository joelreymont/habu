\ wait.f - a test's own waits; neither has a deadline, so a hang shows as the
\ test's WAIT line.
\
\ STORAGE CLASS. CALLER-OWNED: no module state.

require src/lib/error.f
require src/lib/task.f

package TEST-WAIT

public

\ Asks the predicate until it answers true, pausing the task between asks.
\ The predicate's own throws pass through untouched.
: POLL ( R [ R -- R bool ] -- R )
   {: q :}
   begin
      q execute 0= while
      TASK:PAUSE
   repeat ;

\ Waits until the cell holds at least want. The task is the cell's setter. DONE?
\ is read before the cell, so a store it made before ending is seen; ending
\ without it throws the task's code here.
: FOR-CELL ( ^n n ^n -- )
   {: cell:^ want:n tcb:^ :}
   begin
      tcb TASK:DONE?
      cell atomic@ want < while
      if
         tcb TASK:THROW@ dup 0= if drop E-TASK-NO-RESULT then
         throw
      then
      TASK:PAUSE
   repeat
   drop ;

;package
