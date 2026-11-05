\ gate-stdlib-lib.f - adapter from the native suite registry to bin/hb.

require src/lib/string.f
require src/lib/test.f
require src/lib/test/runner.f
require src/lib/process-argv.f
require src/lib/process-env.f
require test/gate-pool.f
require test/gate-images.f
require test/image-grant.f
require test/gate-entry-guard.f
require src/lib/engine-id.f                  \ ENGINE-ID:PATH$ - this gate's own binary

package STDLIB-GATE

64 constant SUITE-USAGE-RC
\ A row's argv tokens: their bytes end to end, and each one's length.
DYNAMIC-BUFFER ROW-ARG-BYTES u8
DYNAMIC-BUFFER ROW-ARG-LENS n
variable ROW-ARGS-U
variable ROW-ARG-N
variable ROW-SCAN
variable ROW-SCRIPT?                     \ the row's `--` has passed

: SUITE-USAGE ( -- )
   s" usage: bin/hb --load test/run.f [-- test-name ...]" SUITE-USAGE-RC die ;

: SUITE-CHECK-ARGS ( -- )
   ARGC 3 > if 3 SCRIPT-SEP? 0= if SUITE-USAGE then then
   SCRIPT-ARGC 0 ?do
      i SCRIPT-ARGV$ {: a:ptr u:n :}
      a u TEST:ONLY 0= if
         2 s" test/run.f: no test named " write drop
         2 a u write drop
         2 S\" \n" write drop
         SUITE-USAGE
      then
   loop ;

\ The spawn path is relative, as the gate runs in the checkout; the variable is
\ absolute: ENGINE-CANDIDATE:PATH$ joins a relative one to its reader's own CWD.
: PRODUCT-SPAWN$ ( -- ptr u8 n )
   s" bin/hb" ;

: PRODUCT-ENGINE$ ( -- ptr u8 n )
   ENGINE-ID:PATH$ ;

\ HABU_UNDER_TEST (read first by src/lib/engine-candidate.f) and HABU_FIXPOINT_ENGINE name the
\ product engine to an item's children; set before the inherit, which skips a name already present.
: SUITE-ENGINE-ENV ( -- )
   s" HABU_UNDER_TEST" >LEN PRODUCT-ENGINE$ >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN PRODUCT-ENGINE$ >LEN PROC-ENV+ ;

\ The grant names the images the gate settled for this row (test/image-grant.f); set before the
\ inherit too, so a gate run as a row of another gate grants its own rows, never its parent's.
: SUITE-ENV ( ptr u8 n -- ) {: grant:ptr grantu:n :}
   PROC-ENV-RESET
   SUITE-ENGINE-ENV
   IMAGE-GRANT:NAME$ >LEN grant grantu >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

\ A row's argv waits with it: the readiness probe may start a build row, which
\ stages its own argv in the same process-wide table, so the tokens are kept
\ here and staged only once the row is ready to spawn.
: ROW-BEGIN ( -- )
   0 ROW-ARGS-U !
   0 ROW-ARG-N !
   0 0<> ROW-SCRIPT? !
   GATE-IMAGES:ROW-RESET ;

\ Every argv entry goes through as written, so a row measures what
\ `bin/hb --load <file>` measures; a file whose assertions are tier-1 facts
\ selects the tier itself, after harness and tool requires and before its
\ subject definitions.
: ROW-ARG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   ROW-ARG-N @ 1+ ROW-ARG-LENS-RESERVE
   u ROW-ARG-N @ ROW-ARG-LENS !
   u 0 > if
      ROW-ARGS-U @ u + ROW-ARG-BYTES-RESERVE
      a ROW-ARGS-U @ ROW-ARG-BYTES u BYTE-COPY
   then
   ROW-ARGS-U @ u + ROW-ARGS-U !
   ROW-ARG-N @ 1+ ROW-ARG-N !
   a u s" --" STR= if 0 0= ROW-SCRIPT? ! exit then
   ROW-SCRIPT? @ 0= if a u GATE-IMAGES:ROW-FILE then ;

: ROW-ARGV! ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   0 ROW-SCAN !
   ROW-ARG-N @ 0 ?do
      i ROW-ARG-LENS @ {: u:n :}
      u 0 > if ROW-SCAN @ ROW-ARG-BYTES u else s" " then >LEN PROC-ARGV+
      ROW-SCAN @ u + ROW-SCAN !
   loop ;

\ TRUE once every keyed image the row needs passed, with its argv staged;
\ else the row is started as its own red row naming the failed build row.
\ Asked only once ROW-SETTLED? answered TRUE.
: SUITE-READY? ( ptr u8 n -- bool ) {: label:ptr labelu:n :}
   GATE-IMAGES:ROW-PASSED? if ROW-ARGV! 0 0= exit then
   label labelu GATE-IMAGES:ROW-RED
   0 0<> ;

\ Each runner answers FALSE, starting nothing, while the row's images are not
\ settled: the registry keeps the row pending (src/lib/test/suite.f). Once
\ settled it starts, or its red row does, and answers TRUE.
: SUITE-HB-RUN ( ptr u8 n -- bool ) {: label:ptr labelu:n :}
   GATE-IMAGES:ROW-SETTLED? 0= if false exit then
   label labelu SUITE-READY? 0= if true exit then
   GATE-IMAGES:ROW-GRANT$ SUITE-ENV
   PRODUCT-SPAWN$ label labelu GT-POOL-START
   true ;

: SUITE-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- bool ) {: in:ptr inu:n label:ptr labelu:n :}
   GATE-IMAGES:ROW-SETTLED? 0= if false exit then
   label labelu SUITE-READY? 0= if true exit then
   GATE-IMAGES:ROW-GRANT$ SUITE-ENV
   PRODUCT-SPAWN$ label labelu in inu GT-POOL-START-STDIN
   true ;

\ A keyed image's build row: the product engine, handed its program on stdin
\ with no arguments, granted the family it settles.
: SUITE-BUILD-RUN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: prog:ptr progu:n label:ptr labelu:n grant:ptr grantu:n :}
   PROC-ARGV-RESET
   grant grantu SUITE-ENV
   PRODUCT-SPAWN$ label labelu prog progu GT-POOL-START-STDIN ;

\ The entry guard walks the load graph DERIVE read and refuses the registry
\ before anything starts: this hook runs outside the catch around the rows. Each
\ keyed image settles in the pool through one build row; a row waits until its
\ images settle (test/gate-images.f), so a broken image closure fails its build
\ row and the rows that load it, and every other row runs.
: SUITE-SETUP ( -- )
   SUITE-CHECK-ARGS
   GATE-IMAGES:DERIVE
   ENTRY-GUARD:CHECK
   GT-POOL-CATCH-SIGNALS
   s" habu-native-suite" GT-START
   GT-POOL-RESET
   [: SUITE-BUILD-RUN ;] GATE-IMAGES:START ;

\ Before every drain the registry settles: every build row retires and every
\ waiting row starts, so a sequential group runs alone.
: SUITE-SETTLE ( -- )
   GATE-IMAGES:SETTLE-ALL ;

: SUITE-DRAIN ( -- )
   GT-POOL-DRAIN-SOFT ;

\ The pool drains softly between groups, so every suite runs whatever went red
\ before it; the red set is reported once here, with exit codes, and decides the
\ exit status. Cleanup runs before the red die ends the process, else each red
\ run leaves its pool root in /tmp; every red row's output and capture names are
\ already on stdout. A throwing cleanup is named GT-POOL-CLEANUP.
: SUITE-FINISH ( n -- ) {: rc:n :}
   s" suites: ran " type TEST:ITEMS-RUN GT-U-TYPE
   s"  of " type TEST:ITEMS-REGISTERED GT-U-TYPE cr
   GT-POOL-RED-REPORT
   GT-POOL-CLEANUP
   GT-POOL-SIGNAL-CHECK
   rc 0 <> if exit then                 \ the body threw: the framework rethrows that code
   GT-POOL-RED# 0 > if s" test pool failed" 1 die then ;

: SUITE-INSTALL-HOOKS ( -- )
   [: SUITE-SETUP ;] TEST:SETUP!
   [: SUITE-FINISH ;] TEST:TEARDOWN!
   [: SUITE-SETTLE ;] TEST:SETTLE!
   [: SUITE-DRAIN ;] TEST:DRAIN!
   [: ROW-BEGIN ;] TEST:ARGS-BEGIN!
   [: ROW-ARG+ ;] TEST:ARG+!
   [: SUITE-HB-RUN ;] TEST:RUNNER!
   [: SUITE-HB-RUN-STDIN ;] TEST:STDIN-RUNNER! ;

public

: MAIN ( -- )
   SUITE-INSTALL-HOOKS
   TEST:RESET ;

;package
