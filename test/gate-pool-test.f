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

variable GPT-COW

: GPT-WORKER ( -- )
   7 GPT-COW !
   s" gate-pool fork worker" type cr ;

: GATE-POOL-TEST-MAIN ( -- )
   T-RESET
   0 GPT-COW !
   s" gate-pool-test" GT-START
   2 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" fork worker" 1000 [: GPT-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GPT-COW @ 0 T=
   GT-CLEANUP
   T-REPORT
   s" gate-pool-test: ok" type cr ;

GATE-POOL-TEST-MAIN
