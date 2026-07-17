\ gate-process-child.f - fresh-process telemetry inheritance fixture.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-fork.f
require test/gate-stats.f

package GATE-PROCESS-CHILD
private

: WAIT-CLEAN ( pid -- )
   PROC-WAIT-STATUS 0 <> if E-PROC-WAIT throw then ;

: EXEC ( -- )
   s" /usr/bin/true" >LEN PROC-PATHZ
   s" nested-exec" GS-HELPER-EVENT
   -1 >FD -1 >FD -1 >FD PROC-SPAWN-RAW {: pid:pid :}
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid WAIT-CLEAN ;

: RUN-FORK ( -- )
   s" nested-fork" GATE-PROCESS:OWNER!
   PROC-FORK-RAW {: pid:pid :}
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid PID>N 0= if s" " 0 die then
   pid WAIT-CLEAN ;

: RUN ( -- )
   EXEC
   RUN-FORK ;

RUN

;package
