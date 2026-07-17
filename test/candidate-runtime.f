\ candidate-runtime.f - exact-candidate resident runtime source probes.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/runner.f
require test/gate-common.f
require lib/build.f
require lib/codesign.f
require tools/build-fixpoint.f
require test/gate-pool.f
require test/gate-engine-lib.f

package CANDIDATE-RUNTIME
public

: CONSTRUCT ( -- )
   s" hb-candidate-construct" GT-START
   CONSTRUCT-RUNNER:PARITY!
   GE-CONSTRUCT-EXEC
   GT-CLEANUP
   s" PASS: candidate construct direct/fork parity" type cr ;

;package

' CANDIDATE-RUNTIME:CONSTRUCT catch s" candidate construct runtime" GE-THROW-REPORT
