\ wait.f - a test's own waits; neither has a deadline, so a hang shows as the
\ test's WAIT line. EXPIRED names a library call's own timeout no case expects:
\ it throws E-PROC-TIMEOUT, so the test ends as a timeout and not as a FAIL.
\
\ STORAGE CLASS. CALLER-OWNED: no module state.

require src/lib/error.f
require src/lib/task.f

package TEST-WAIT

public

\ No assertion counter and no test label is read or changed.
: EXPIRED ( ptr u8 n -- )
   type s" : a deadline expired" type cr
   E-PROC-TIMEOUT throw ;

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
: FOR-CELL ( ptr n n ptr n -- )
   {: cell:ptr want:n tcb:ptr :}
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
