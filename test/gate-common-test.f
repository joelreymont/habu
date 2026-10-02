\ gate-common-test.f - the rc checks of test/gate-common-lib.f on an entry whose
\ own deadline expired, through the real load path. GE-EXPECT-OK, GE-EXPECT-RC
\ and GE-EXPECT-NONZERO each run in test/gate-common-deadline.f, spawned as a
\ gate row is. Each must print GE-FAIL's capture of the timed-out entry and
\ then leave by an uncaught E-PROC-TIMEOUT, the exit the pool labels
\ TIMEOUT-UNDER-LOAD (test/gate-pool.f GT-POOL-INNER-TIMEOUT?).
\ GE-CAPTURE-ACTION runs here in process: an action writing more than a pipe
\ holds and more than GT-OUT-CAP is refused as truncated instead of blocking,
\ a throwing action keeps its code and output, and neither leaves a file in
\ the gate root.

require lib/errors.f
require lib/string.f
require lib/test.f
require test/gate-common.f
require test/gate-pool.f

package GATE-COMMON-TEST

: RUN-CHECK ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   GE-HB-RESET
   s" --load" GE-ARG+
   s" test/gate-common-deadline.f" GE-ARG+
   s" --" GE-ARG+
   mode modeu GE-ARG+
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV ;

\ The report heads with the check's label and the entry's timeout, and runs
\ through the entry's stderr before the throw ends the child.
: EXPECT-REPORT ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n head:ptr headu:n :}
   mode modeu T-LABEL
   mode modeu RUN-CHECK
   mode modeu GE-RC@ UNCAUGHT-RC T=
   GT-ERR$ GT-POOL-UNCAUGHT-TIMEOUT? TTRUE
   GT-OUT$ head headu STARTS-WITH? TTRUE
   GT-OUT$ S\" \nstderr:\n" CONTAINS? TTRUE ;

\ Past a pipe's 64 KiB and GT-OUT-CAP alike.
70000 constant BIG-BYTES

variable ROOT-FILES

: BIG ( -- )
   BIG-BYTES 0 ?do s" x" type loop ;

: FAILING ( -- )
   s" out" type
   2 s" err" GT-WRITE-FD
   7 throw ;

: ROOT-FILE+ ( ptr u8 n -- )
   2drop ROOT-FILES @ 1+ ROOT-FILES ! ;

: ROOT-FILES# ( -- n )
   0 ROOT-FILES !
   GT-ROOT [: ROOT-FILE+ ;] WALK-FILES
   ROOT-FILES @ ;

: BIG-CAPTURE ( -- )
   [: BIG ;] GE-CAPTURE-ACTION ;

: OVER-CAP ( -- )
   s" capture past GT-OUT-CAP" T-LABEL
   [: BIG-CAPTURE ;] E-PROC-TRUNCATED TTHROWSQ
   s" capture past GT-OUT-CAP leaves no file" T-LABEL
   ROOT-FILES# 0 T= ;

: THROWN ( -- )
   [: FAILING ;] GE-CAPTURE-ACTION
   s" throwing action code" T-LABEL
   GT-RC@ 7 T=
   s" throwing action stdout" T-LABEL
   GT-OUT$ s" out" T$=
   s" throwing action stderr" T-LABEL
   GT-ERR$ s" err" T$=
   s" throwing action leaves no file" T-LABEL
   ROOT-FILES# 0 T= ;

public

: MAIN ( -- )
   T-RESET
   s" hb-gate-common" GT-START
   THROWN
   OVER-CAP
   s" ok" S\" FAIL: deadline under GE-EXPECT-OK\noutcome: timeout" EXPECT-REPORT
   s" rc" S\" FAIL: deadline under GE-EXPECT-RC\noutcome: timeout" EXPECT-REPORT
   s" nonzero" S\" FAIL: deadline under GE-EXPECT-NONZERO\noutcome: timeout" EXPECT-REPORT
   GT-CLEANUP
   T-REPORT ;

;package

GATE-COMMON-TEST:MAIN
