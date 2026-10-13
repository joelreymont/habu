\ getpid-smoke.f - focused proof of the getpid process-identity primitive.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/memory.f \
\      src/lib/process.f src/lib/process-fork.f test/getpid-smoke.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/process.f
require src/lib/process-fork.f

package GETPID-SMOKE

variable PARENT-PID

: CHILD-DIE ( n -- ) {: rc:n :}
   s" " rc die ;

\ In the forked child getpid must differ from the parent's captured pid, proving
\ getpid is the live per-process syscall and not a baked constant. Exit 0 on a
\ distinct pid, 1 otherwise; the parent reads that status.
: CHILD-RUN ( -- )
   getpid PARENT-PID @ <> if 0 else 1 then CHILD-DIE ;

: FORK-CHILD ( -- pid )
   PROC-FORK:CHECKED dup PID>N 0= if
      drop CHILD-RUN
   then ;

: CHECK-POSITIVE ( -- )
   getpid 0 > TTRUE ;

: CHECK-STABLE ( -- )
   getpid getpid T= ;

: CHECK-CHILD-DISTINCT ( -- )
   getpid PARENT-PID !
   FORK-CHILD PROC-WAIT-STATUS 0 T= ;

: RUN ( -- )
   T-RESET
   CHECK-POSITIVE
   CHECK-STABLE
   CHECK-CHILD-DISTINCT
   T-REPORT
   s" getpid-smoke: ok" type cr ;

RUN

;package
