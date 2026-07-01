\ gate-pool-test.f - focused coverage for fork-backed test pool workers.
\ Run: bin/hb --load test/gate-pool-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-fork.f
require lib/test/runner.f
require test/gate-pool.f

$4000 constant GPT-CAP
$1388 constant GPT-TIMEOUT-MS

create GPT-OUT GPT-CAP allot
create GPT-ERR GPT-CAP allot

variable GPT-COW

: GPT-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: GPT-WORKER ( -- )
   7 GPT-COW !
   s" gate-pool fork worker" type cr ;

: GPT-FAIL-WORKER ( -- )
   s" gate-pool failing worker" type cr
   77 throw ;

: GPT-FAIL-CASE ( -- )
   s" gate-pool-test-fail" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" fork failing worker" 1000 [: GPT-FAIL-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN ;

: GPT-FAIL-MODE? ( -- bool )
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ s" fail-case" STR= exit then
   0 0= 0= ;

: GPT-CAPTURE>N ( len len rc -- n n n ) {: outu:len erru:len rc:rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: GPT-FAIL-CAPTURE ( -- n n n )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/gate-pool-test.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" fail-case" >LEN PROC-ARGV+
   GPT-HB$ >LEN GPT-OUT GPT-CAP >LEN GPT-ERR GPT-CAP >LEN
   GPT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   GPT-CAPTURE>N ;

: GPT-EXPECT-FAIL-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" gate-pool failing worker" CONTAINS? TTRUE
   GPT-OUT outu s" outcome kind: 0" CONTAINS? TTRUE
   GPT-OUT outu s" code: 77" CONTAINS? TTRUE
   GPT-OUT outu s" FAIL: fork failing worker" CONTAINS? TTRUE ;

: GPT-EXPECT-FAIL-ERR ( n -- ) {: erru:n :}
   GPT-ERR erru s" test pool phase failed" CONTAINS? TTRUE ;

: GPT-FORK-FAIL-REPORT ( -- )
   GPT-FAIL-CAPTURE 1 T=
   {: outu:n erru:n :}
   outu GPT-EXPECT-FAIL-OUT
   erru GPT-EXPECT-FAIL-ERR ;

: GATE-POOL-TEST-MAIN ( -- )
   GPT-FAIL-MODE? if GPT-FAIL-CASE exit then
   T-RESET
   0 GPT-COW !
   s" gate-pool-test" GT-START
   2 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" fork worker" 1000 [: GPT-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GPT-COW @ 0 T=
   GT-CLEANUP
   GPT-FORK-FAIL-REPORT
   T-REPORT
   s" gate-pool-test: ok" type cr ;

GATE-POOL-TEST-MAIN
