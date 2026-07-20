\ candidate-runtime.f - exact-candidate resident runtime source probes.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-id.f
require lib/test/runner.f
require test/gate-common.f
require lib/build.f
require lib/codesign.f
require tools/build-fixpoint.f
require test/gate-pool.f
require test/gate-engine-lib.f

package CANDIDATE-RUNTIME
private

64 constant KEY-U
create FILE-KEY KEY-U allot

: VERIFY-EXE ( -- )
   GE-HB$ FILE-KEY SHA256-FILE-HEX 0 <> if
      s" candidate runtime: executable hash failed" GE-IDENTITY-RC die
   then
   FILE-KEY KEY-U ENGINE-ID:KEY$ STR= 0= if
      s" candidate runtime: executable identity mismatch" GE-IDENTITY-RC die
   then
   s" candidate-executable-sha256" FILE-KEY KEY-U GS-EVENT-FIELD ;

: CHECK-DIRECT ( -- )
   RUNTIME-DIRECT:EXEC# {: count:n :}
   count RUNTIME-DIRECT:SUBJECT-LIMIT > if
      s" candidate runtime process-exec=" type count .
      s" max-exec=" type RUNTIME-DIRECT:SUBJECT-LIMIT . cr
      s" candidate runtime process ratchet exceeded" GE-FAIL
   then ;

: RUN-CONSTRUCT ( -- )
   s" hb-candidate-construct" GT-START
   VERIFY-EXE
   RUNTIME-SUBJECT:RESET
   CONSTRUCT-RUNNER:PARITY!
   GE-CONSTRUCT-EXEC
   GT-CLEANUP
   s" PASS: candidate construct direct/subject parity" type cr ;

: RUN-RUNTIME ( bool -- ) {: parity:bool :}
   s" hb-candidate-runtime" GT-START
   VERIFY-EXE
   RUNTIME-DIRECT:RESET
   RUNTIME-SUBJECT:RESET
   parity if RUNTIME-RUNNER:PARITY! else RUNTIME-RUNNER:SUBJECT! then
   parity if CONSTRUCT-RUNNER:PARITY! else CONSTRUCT-RUNNER:SUBJECT! then
   GE-CONSTRUCT-EXEC
   RUNTIME-CHECKS:REST
   parity 0= if CHECK-DIRECT then
   GT-CLEANUP
   s" PASS: candidate runtime source batch" type cr ;

: RUNTIME ( -- )
   0 0= 0= RUN-RUNTIME ;

: RUNTIME-PARITY ( -- )
   0 0= RUN-RUNTIME ;

public

: MAIN ( -- )
   SCRIPT-ARGC 0= if RUNTIME exit then
   SCRIPT-ARGC 1 = if
      0 SCRIPT-ARGV$ s" construct-parity" STR= if RUN-CONSTRUCT exit then
      0 SCRIPT-ARGV$ s" runtime-parity" STR= if RUNTIME-PARITY exit then
   then
   s" usage: test/candidate-runtime.f [construct-parity|runtime-parity]" 64 die ;

;package

' CANDIDATE-RUNTIME:MAIN catch s" candidate runtime" GE-THROW-REPORT
