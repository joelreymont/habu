\ fd-lock.f - order descriptor creation against fork and spawn. A forked child
\ that execs keeps every descriptor not yet close-on-exec, so creating and marking
\ one run under FD-LOCK:WITH, which PROC-FORK:RAW's fork also holds. PROCESS:SPAWN
\ (src/lib/spawn.f) holds it for its own descriptors, so no fork copies the Linux
\ spawn's error pipe. The lock does not nest: no body under WITH takes it or spawns.
\ STORAGE CLASS. PROCESS-WIDE: one TASK:FACILITY, ready at load, never destroyed.

require src/lib/task.f

package FD-LOCK
TASK:FACILITY LOCK
LOCK TASK:FACILITY-INIT
public
: WITH ( R [ R -- S ] -- S )
   LOCK TASK:GET [: LOCK TASK:RELEASE ;] finally ;
;package
