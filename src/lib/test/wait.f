\ wait.f - a test's own waits; none has a deadline, so a hang shows as the
\ test's WAIT line. EXPIRED names a library call's own timeout no case expects:
\ it throws E-PROC-TIMEOUT, so the test ends as a timeout and not as a FAIL.
\
\ STORAGE CLASS. PROCESS-WIDE: EPOLL-BLOCKED's path and text buffers and its
\ count, so one task at a time counts.

require src/lib/error.f
require src/lib/task.f
HB-TARGET-LINUX? [if]
require src/lib/fs.f
require src/lib/fs-list.f
require src/lib/string-option.f
[then]

package TEST-WAIT

public

\ No assertion counter and no test label is read or changed.
: EXPIRED ( str -- )
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

HB-TARGET-LINUX? [if]
private

\ A thread's syscall file holds "running", or the number of the syscall the
\ thread is blocked in and that call's arguments in hex.
$100 constant SYSCALL-CAP
PATH-CAP BUFFER: TID-DIR-BUF
PATH-CAP BUFFER: SYSCALL-PATH-BUF
SYSCALL-CAP BUFFER: SYSCALL-BUF
variable BLOCKED-N

\ The syscall epoll_wait(3) enters: arm64 has only epoll_pwait.
: EPOLL-NR ( -- n )
   HB-TARGET-LINUX-X86-64? if 232 else 22 then ;

: SYSCALL-PATH ( str -- str ) {: tid:str :}
   s" /proc/self/task" tid TID-DIR-BUF FS:JOIN-PATH {: n:n :}
   TID-DIR-BUF n >str s" syscall" SYSCALL-PATH-BUF FS:JOIN-PATH {: m:n :}
   SYSCALL-PATH-BUF m >str ;

: IN-EPOLL? ( str -- bool )
   STR:SPACE 0 STR:SPLIT-NEXT drop drop
   STR:>NUMBER? MATCH option
      some OF EPOLL-NR = ENDOF
      none OF false ENDOF
   ;MATCH ;

: TID-COUNT ( str -- )
   SYSCALL-PATH SYSCALL-BUF SYSCALL-CAP FS:READ-ALL {: len:n :}
   SYSCALL-BUF len >str IN-EPOLL? if 1 BLOCKED-N +! then ;

\ A thread that ended after the listing has no syscall file to open or read.
: TID-SEEN ( str -- ) {: tid:str :}
   [: tid TID-COUNT ;] catch {: rc:n :}
   rc 0= rc E-FS-OPEN = or rc E-FS-IO = or if exit then
   rc throw ;

public

\ How many of the process's threads are blocked in epoll_wait(3).
: EPOLL-BLOCKED ( -- n )
   0 BLOCKED-N !
   s" /proc/self/task" [: TID-SEEN ;] FS-LIST:EACH
   BLOCKED-N @ ;

\ Waits until at least n threads, one the task's, are blocked in epoll_wait(3).
\ DONE? is read first: a task that ends throws its code here, or E-TASK-NO-RESULT.
: FOR-EPOLL ( n ^n -- )
   {: want:n tcb:^ :}
   begin
      tcb TASK:DONE?
      EPOLL-BLOCKED want < while
      if
         tcb TASK:THROW@ dup 0= if drop E-TASK-NO-RESULT then
         throw
      then
      TASK:PAUSE
   repeat
   drop ;
[then]

;package
