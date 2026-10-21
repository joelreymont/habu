\ boot-census-fixture.f - a process tree of known shape for tools/perf/boot-census.f:
\ one fork child (same image and argv as its parent) and one exec child
\ (/bin/sleep), each alive long enough for the census to list it.
require src/lib/errors.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-fork.f
require src/lib/task.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package BOOT-CENSUS-FIXTURE
private
500 constant HOLD-MS

\ One exec with exactly the same path and argv; only its environment changes.
\ Holding both images gives the census an observation on each side of exec.
: REEXEC ( -- )
   HOLD-MS >MS TASK:SLEEP
   s" BOOT_CENSUS_REEXEC" GETENV nip 0 > if exit then
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/perf/boot-census-fixture.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" reexec" >LEN PROC-ARGV+
   s" BOOT_CENSUS_REEXEC" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE execve
   drop E-PROC-SPAWN throw ;

: FORK-CHILD ( -- )
   HOLD-MS >MS TASK:SLEEP
   s" " 0 die ;

: SPAWN-SLEEP ( -- )
   PROC-ARGV-RESET
   s" 0.5" >LEN PROC-ARGV+
   s" /bin/sleep" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PROC-WAIT-STATUS drop ;

public
: MAIN ( -- )
   SCRIPT-ARGC 0 > if REEXEC exit then
   PROC-FORK:CHECKED {: child:pid :}
   child PID>N 0= if FORK-CHILD then
   SPAWN-SLEEP
   child PROC-WAIT-STATUS drop ;
;package

BOOT-CENSUS-FIXTURE:MAIN
