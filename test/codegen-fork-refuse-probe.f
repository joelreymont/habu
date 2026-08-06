\ codegen-fork-refuse-probe.f - the refusal half of the clang-reference fork
\ contract, in the only process shape whose precondition is real.
\
\ WHY A SEPARATE PROCESS. The refusal under test is "a forked child whose
\ chain never mapped the reference may not load it" - the dyld fork hazard,
\ refused by name in tools/codegen-compare-cabi.f OPEN. That precondition
\ names the PARENT: the forking process must hold no mapping. The
\ codegen-compare gate member cannot be that parent - its sibling files
\ legitimately map the reference in the same process when the host has a C
\ toolchain, and the gate root maps it before forking members - so asserting
\ the refusal inside the member asserts a state the member is not in, and the
\ verdict flips with whichever files shared the process: the intermittent
\ refuse -8264 (dot habu-attr-the-compare-2f98fcfc). This probe is exec'd
\ fresh by test/codegen-fork-reference-test.f: nothing in its chain has
\ mapped, the precondition is ASSERTED rather than assumed, and the refusal
\ is then provable in every configuration the member runs in - including a
\ host with no C toolchain, because the guard fires before any path or build
\ is consulted.
\ Run: bin/hb --load test/codegen-fork-refuse-probe.f

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-env.f
require lib/process-argv.f
require lib/process-fork.f
require lib/test.f
require lib/test/runner.f
require test/gate-stats.f
require test/gate-pool.f
require tools/codegen-compare-cabi.f

package CGFORK-PROBE

\ The child's chain maps nothing, so OPEN must refuse it BY NAME - success or
\ any other error is the stage failure.
: REFUSE-ACT ( -- )
   [: CODEGEN-CABI:OPEN ;] catch {: rc:n :}
   CODEGEN-CC:REMOVE
   rc E-CODEGEN-CLANG-FORK <> if E-CODEGEN-COMPARE-STAGE throw then ;

public

: RUN ( -- )
   T-RESET
   s" refuse-probe: this process maps nothing" T-LABEL
   CODEGEN-CABI:MAPPED? TFALSE
   s" refuse-probe: the unmapped chain's forked child is refused by name" T-LABEL
   GT-POOL-RESET
   s" refuse" 180000 [: REFUSE-ACT ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GT-POOL-RED# 0 T=
   T-REPORT ;

;package

CGFORK-PROBE:RUN
