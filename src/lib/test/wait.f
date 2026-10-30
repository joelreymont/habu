\ wait.f - a test's own waits. POLL's expired deadline is no assertion result
\ (docs/gate.md "How a suite runs, and what that demands of its files"): it names
\ itself on stdout and throws E-PROC-TIMEOUT, so the row ends as a timeout and
\ not as a FAIL. FOR-CELL has no deadline: its worker's end ends it.
\
\ STORAGE CLASS. CALLER-OWNED: no module state.

require src/lib/errors.f
require src/lib/task.f

package TEST-WAIT

public

\ No assertion counter and no test label is read or changed.
: EXPIRED ( ptr u8 n -- )
   type s" : a deadline expired" type cr
   E-PROC-TIMEOUT throw ;

\ Asks the predicate until it answers true, pausing the task between asks. At
\ the absolute mono-ns deadline the wait is EXPIRED under its name. The
\ predicate's own throws pass through untouched.
: POLL ( R [ R -- R bool ] n ptr u8 n -- R )
   {: q deadline:n name:ptr len:n :}
   begin
      q execute 0= while
      mono-ns deadline >= if name len EXPIRED then
      TASK:PAUSE
   repeat ;


\ The task is the cell's setter. DONE? is read before the cell, so a store it
\ made before ending is seen; ending without it throws the task's code here.
: FOR-CELL ( ptr n n ptr n -- )
   {: cell:ptr want:n tcb:ptr :}
   begin
      tcb TASK:DONE?
      cell atomic@ want <> while
      if
         tcb TASK:THROW@ dup 0= if drop E-TASK-NO-RESULT then
         throw
      then
      TASK:PAUSE
   repeat
   drop ;

;package
