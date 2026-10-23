\ wait.f - a test's own bounded wait. A deadline that expires is no assertion
\ result (docs/gate.md): the wait names itself on stdout and throws
\ E-PROC-TIMEOUT, so the row ends as a timeout and not as a FAIL.
\
\ STORAGE CLASS. CALLER-OWNED: no module state.

require src/lib/errors.f
require src/lib/task.f

package TEST-WAIT

public

\ Prints "<name>: a deadline expired" and throws E-PROC-TIMEOUT. No assertion
\ counter and no test label is read or changed.
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

;package
