\ fd-lock.f - order descriptor creation against spawn and fork.
\
\ A descriptor is inheritable from its creation until it is marked
\ close-on-exec, so the two happen under FD-LOCK:WITH, which every spawn in
\ src/lib/process.f and its siblings and PROC-FORK:RAW's fork also hold:
\ another task's child inherits the descriptor marked or not at all
\ (src/lib/process-task-test.f PTT-NO-INHERIT). A locked body is a `-HELD` word
\ that takes the lock nowhere inside: a nested scope's release would end the
\ outer one.
\
\ STORAGE CLASS. PROCESS-WIDE. One TASK:FACILITY, made ready while this file
\ loads, before any task is live, and never destroyed.

require src/lib/task.f

package FD-LOCK
TASK:FACILITY LOCK
LOCK TASK:FACILITY-INIT
public
: WITH ( R [ R -- S ] -- S )
   LOCK TASK:GET [: LOCK TASK:RELEASE ;] finally ;
;package
