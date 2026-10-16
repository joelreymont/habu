\ The nested gate gate-images-test.f runs. A compiler bundle whose build ran out
\ of time is a deadline, not a red set: the gate reports and cleans up as
\ usual, then exits PROC-TIMEOUT-RC, so the outer test throws E-PROC-TIMEOUT
\ again instead of asserting an exit code.
require src/lib/errors.f
require src/lib/process.f
require src/lib/test/suite.f
require test/gate-pool.f
require test/gate-images.f
require test/gate-stdlib-lib.f

package GATE-ORCH-REGISTRY
private

\ The compiler build row went red on a deadline: the pool's own timeout, or an
\ inner one it reclassified. A row ended for its CPU budget also reads timed
\ out, but its kind is CPU-BUDGET, which no load explains (GT-POOL-KIND-NAME.),
\ so it keeps the normal teardown.
: COMPILER-TIMED-OUT? ( -- bool )
   GATE-IMAGES:COMPILER-RED {: red:n :}
   red 0 < if false exit then
   red GT-POOL-RED-TIMED-OUT-PTR @ red GT-POOL-RED-OVER? 0= and ;

\ The body's own throw keeps the normal teardown, which rethrows it.
: FINISH ( n -- )
   {: rc:n :}
   rc 0= COMPILER-TIMED-OUT? and if
      E-PROC-TIMEOUT STDLIB-GATE:SUITE-FINISH
      s" gate orchestration: compiler images ran out of time" PROC-TIMEOUT-RC die
   then
   rc STDLIB-GATE:SUITE-FINISH ;

public
: INSTALL ( -- )
   [: FINISH ;] TEST:TEARDOWN! ;
;package

STDLIB-GATE:MAIN
GATE-ORCH-REGISTRY:INSTALL
using TEST
SUITE release test/gate-images-orch-release.f ;SUITE
SUITE consumer test/gate-images-orch-consumer.f ;SUITE
SUITE later test/gate-images-orch-later.f ;SUITE
RUN
;using
