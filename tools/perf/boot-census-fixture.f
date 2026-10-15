\ boot-census-fixture.f - a process tree of known shape for tools/perf/boot-census.f:
\ one fork child (same image and argv as its parent) and one exec child
\ (/bin/sleep), each alive long enough for the census to list it.
require src/lib/errors.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-fork.f
require src/lib/task.f

package BOOT-CENSUS-FIXTURE
private
500 constant HOLD-MS

: FORK-CHILD ( -- )
   HOLD-MS >MS TASK:SLEEP
   s" " 0 die ;

: SPAWN-SLEEP ( -- )
   PROC-ARGV-RESET
   s" 0.5" >LEN PROC-ARGV+
   s" /bin/sleep" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PROC-WAIT-STATUS drop ;

public
: MAIN ( -- )
   PROC-FORK:CHECKED {: child:pid :}
   child PID>N 0= if FORK-CHILD then
   SPAWN-SLEEP
   child PROC-WAIT-STATUS drop ;
;package

BOOT-CENSUS-FIXTURE:MAIN
