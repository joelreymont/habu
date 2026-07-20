\ process-fork-test.f - focused proof of the checked fork wrappers.
\
\ A real fork through PROC-FORK:CHECKED returns 0 in the child and the child's
\ positive pid in the parent: the child takes the pid-zero branch and exits
\ cleanly, and the parent sees a positive pid and reaps a clean exit through
\ PROC-WAIT-RC. PROC-FORK:RAW follows the same pid contract without the throw
\ guard. 0 0 PROC-FORK:SET-PGID (run inside a forked child so the test process
\ keeps its own process group) makes that child its own group leader (rc 0),
\ which the parent reads back as a clean child exit.
\ Run: bin/hb --load lib/errors.f lib/prelude.f lib/string.f lib/test.f \
\      lib/memory.f lib/process.f lib/process-fork.f lib/process-fork-test.f

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/process.f
require lib/process-fork.f

package PROC-FORK-TEST

: FORK-EXIT ( n -- )
   s" " rot die ;

\ Reap the forked child by pid: a clean (0) exit lands on the ok arm; anything
\ else is a test failure.
: EXPECT-CLEAN-CHILD ( n -- )
   >PID PROC-WAIT-RC MATCH result
     ok  OF 0 T= ENDOF
     err OF drop -1 0 T= ENDOF
   ;MATCH ;

\ Checked fork: the child sees pid 0 and exits cleanly; the parent sees a
\ positive pid and reaps a clean exit.
: CHECK-CHECKED ( -- )
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if 0 FORK-EXIT then
   pid 0 > TTRUE
   pid EXPECT-CLEAN-CHILD ;

\ Raw fork: same pid contract (0 in child, positive in parent) without the throw.
: CHECK-RAW ( -- )
   PROC-FORK:RAW PID>N {: pid:n :}
   pid 0= if 0 FORK-EXIT then
   pid 0 > TTRUE
   pid EXPECT-CLEAN-CHILD ;

\ 0 0 PROC-FORK:SET-PGID makes the caller its own group leader; done in a child
\ so the test process keeps its group. The child exits 0 iff setpgid returned 0.
: CHECK-SET-PGID ( -- )
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if
      0 >PID 0 >PID PROC-FORK:SET-PGID RC>N 0 = if 0 else 1 then FORK-EXIT
   then
   pid EXPECT-CLEAN-CHILD ;

: RUN ( -- )
   T-RESET
   CHECK-CHECKED
   CHECK-RAW
   CHECK-SET-PGID
   T-REPORT
   s" process-fork-test: ok" type cr ;

RUN

;package
