\ fd-lock.f - order descriptor creation against fork and spawn.
\
\ A forked child that execs keeps every descriptor not yet marked
\ close-on-exec, so creating a descriptor and marking it happen under
\ FD-LOCK:WITH, which PROC-FORK:RAW's fork also holds: another task's fork
\ that execs keeps the descriptor marked or not at all. A spawned child gets
\ only its map (src/lib/process-task-test.f PTT-NO-INHERIT), so PROCESS:SPAWN
\ (src/lib/spawn.f), the spawn every src/lib/process.f RAW word makes, holds
\ the lock for its own descriptors: no fork copies the Linux spawn's error
\ pipe, whose write end in a forked child would keep the spawn waiting for the
\ exec's report. A nested scope's release would end the outer one, so a body
\ run under the lock takes it nowhere inside, and no spawn runs under
\ FD-LOCK:WITH: the RAW words take it through PROCESS:SPAWN.
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
