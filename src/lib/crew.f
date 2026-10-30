\ crew.f - a task to each job. START runs a body with a value on a fresh
\ TASK:NEW task; STOP runs a quotation with the value of every task still held,
\ halts them all and waits for them. A halted body ends at its next TASK:PAUSE,
\ so STOP waits as long as a body runs without one. A task that ended is joined
\ and given back lazily, by the next START or by STOP, so a quiet crew keeps
\ its last ended tasks' stacks until then. A task that ends by a throw reports
\ it as it ends, as one stderr line `crew: task <value> failed, throw <code>`
\ built in its own string builder.
\ STORAGE CLASS. PROCESS-WIDE: a row to each task START made and nobody has
\ given back, and the STOPPING mark, read and written under one TASK:FACILITY.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fd.f
require src/lib/adt/result.f
require src/lib/task.f

package CREW
public
-9620 constant E-FIRST
-9629 constant E-LAST
-9620 constant E-STATE    \ START while a STOP runs, or a STOP while one runs
private

\ No task is live while this file loads, so the lock is made ready here.
TASK:FACILITY LOCK
LOCK TASK:FACILITY-INIT
variable ROW-COUNT
TYPED-VARIABLE STOPPING bool
DYNAMIC-BUFFER ROW-TCB ptr n
DYNAMIC-BUFFER ROW-VALUE n
DYNAMIC-BUFFER ROW-BODY [ n -- ]
DYNAMIC-BUFFER ROW-LIVE bool        \ the row holds a task not yet given back

\ The first free row, else -1; under LOCK.
: FREE-ROW ( -- n )
   ROW-COUNT @ 0 ?do
      i ROW-LIVE @ if else i unloop exit then
   loop -1 ;

\ A free row at the table's end; under LOCK.
: GROW ( -- n )
   ROW-COUNT @ {: count:n :}
   count 1+ ROW-TCB-RESERVE
   count 1+ ROW-VALUE-RESERVE
   count 1+ ROW-BODY-RESERVE
   count 1+ ROW-LIVE-RESERVE
   false count ROW-LIVE !
   count 1+ ROW-COUNT !
   count ;

\ The live row holding the task at that distance from NULL-PTR, else -1; under
\ LOCK.
: ROW-OF ( n -- n )
   {: self:n :}
   ROW-COUNT @ 0 ?do
      i ROW-LIVE @ if
         i ROW-TCB @ NULL-PTR - self = if i unloop exit then
      then
   loop -1 ;

: ROWS ( -- n ) LOCK TASK:GET ROW-COUNT @ LOCK TASK:RELEASE ;

: ROW@ ( n -- ptr n n bool )
   {: idx:n :}
   LOCK TASK:GET
   idx ROW-TCB @ idx ROW-VALUE @ idx ROW-LIVE @
   LOCK TASK:RELEASE ;

: CLEAR ( n -- ) LOCK TASK:GET false swap ROW-LIVE ! LOCK TASK:RELEASE ;


: REPORT ( n n -- )
   {: value:n code:n :}
   SB-RESET
   s" crew: task " SB-APPEND
   value FMT:SB-INT
   s"  failed, throw " SB-APPEND
   code FMT:SB-INT
   STR-LF SB-APPEND-C
   FD:STDERR FD>N SB$ write drop ;

\ An exit cleanup of the ending task, so the string builder is that task's.
: REPORT-END ( -- )
   TASK:SELF TASK:THROW@ {: code:n :}
   code 0= if exit then
   LOCK TASK:GET
   TASK:SELF NULL-PTR - ROW-OF ROW-VALUE @
   LOCK TASK:RELEASE
   code REPORT ;

\ A task's root. START fills the row and marks it live before it releases
\ LOCK, so the row is found.
: RUN-ROW ( -- )
   [: REPORT-END ;] TASK:SELF TASK:AT-EXIT
   LOCK TASK:GET
   TASK:SELF NULL-PTR - ROW-OF {: idx:n :}
   idx ROW-VALUE @ idx ROW-BODY @
   LOCK TASK:RELEASE
   execute ;

\ The task reported a throw that ended it (REPORT-END).
: GIVE-BACK ( ptr n -- )
   {: tcb:ptr :}
   tcb TASK:JOIN MATCH result
      ok OF drop ENDOF
      err OF drop ENDOF
   ;MATCH
   tcb TASK:FREE ;

\ Gives back every live row's task that has ended; under LOCK.
: REAP ( -- )
   ROW-COUNT @ 0 ?do
      i ROW-LIVE @ if
         i ROW-TCB @ TASK:DONE? if
            i ROW-TCB @ GIVE-BACK
            false i ROW-LIVE !
         then
      then
   loop ;

\ Under LOCK, which the caller releases.
: PLACE ( n n [ n -- ] -- )
   {: size:n value:n body :}
   STOPPING @ if E-STATE throw then
   REAP
   FREE-ROW dup 0< if drop GROW then {: idx:n :}
   size TASK:NEW {: tcb:ptr :}
   tcb idx ROW-TCB !
   value idx ROW-VALUE !
   body idx ROW-BODY !
   tcb [: [: RUN-ROW ;] over TASK:ACTIVATE ;] catch {: code:n :} drop
   code 0<> if tcb TASK:KILL tcb TASK:FREE code throw then
   true idx ROW-LIVE ! ;


: BEGIN-STOP ( -- )
   LOCK TASK:GET
   STOPPING @ if LOCK TASK:RELEASE E-STATE throw then
   true STOPPING !
   LOCK TASK:RELEASE ;

\ While STOPPING is set no START fills, grows or clears a row, so the rows STOP
\ walks are the ones it found.
: ASK-ROW ( [ n -- ] n -- )
   {: q idx:n :}
   idx ROW@ {: tcb:ptr value:n live:bool :}
   live if value q execute then ;

: ASK-ALL ( [ n -- ] -- )
   {: q :}
   ROWS 0 ?do q i ASK-ROW loop ;

: HALT-ROW ( n -- )
   ROW@ {: tcb:ptr value:n live:bool :}
   live if tcb TASK:HALT then ;

\ The join runs outside LOCK: a halted task may still be waiting for it in
\ RUN-ROW.
: END-ROW ( n -- )
   {: idx:n :}
   idx ROW@ {: tcb:ptr value:n live:bool :}
   live if
      tcb GIVE-BACK
      idx CLEAR
   then ;

: END-ALL ( -- )
   ROWS 0 ?do i HALT-ROW loop
   ROWS 0 ?do i END-ROW loop
   LOCK TASK:GET false STOPPING ! LOCK TASK:RELEASE ;

public

\ Starts a task with n bytes of data stack that runs the body with the value.
\ A size below TASK:MIN-STACK is E-TASK-SIZE.
: START ( n n [ n -- ] -- )
   LOCK TASK:GET
   [: PLACE ;] [: LOCK TASK:RELEASE ;] finally ;

\ Runs the quotation on this thread with the value of every task START made
\ and nobody has given back, then halts them all, waits for every one and
\ gives it back. The tasks end even when the quotation throws. Called from a
\ task START made, it waits for that task's own end and never returns.
: STOP ( [ n -- ] -- )
   BEGIN-STOP
   [: ASK-ALL ;] [: END-ALL ;] finally ;

;package
