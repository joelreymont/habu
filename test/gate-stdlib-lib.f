\ gate-stdlib-lib.f - adapter from the native suite registry to bin/hb.

require src/lib/string.f
require src/lib/test.f
require src/lib/test/runner.f
require src/lib/process-argv.f
require src/lib/process-env.f
require test/gate-pool.f
require test/load-graph.f
require test/gate-entry-guard.f
require src/lib/engine-id.f                  \ ENGINE-ID:PATH$ - this gate's own binary

package STDLIB-GATE

64 constant SUITE-USAGE-RC

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

\ HABU_UNDER_TEST (read first by src/lib/engine-candidate.f) names the product engine to an
\ item's children; set before the inherit, which skips a name already present.
: SUITE-ENV ( -- )
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN PRODUCT-ENGINE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

\ The registry feeds a row's argv right before it offers the row (src/lib/test/suite.f
\ ITEM-RUN-FILE), so the tokens are staged as they come. Every argv entry goes
\ through as written, so a row measures what `bin/hb --load <file>` measures.
: ROW-BEGIN ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+ ;

: ROW-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: SUITE-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   SUITE-ENV
   PRODUCT-SPAWN$ label labelu GT-POOL-START ;

: SUITE-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- ) {: in:ptr inu:n label:ptr labelu:n :}
   SUITE-ENV
   PRODUCT-SPAWN$ label labelu in inu GT-POOL-START-STDIN ;

\ The entry guard walks the load graph LOAD-GRAPH:READ read and refuses the
\ registry before anything starts: this hook runs outside the catch around the
\ rows.
: SUITE-SETUP ( -- )
   SUITE-CHECK-ARGS
   LOAD-GRAPH:READ
   ENTRY-GUARD:CHECK
   GT-POOL-CATCH-SIGNALS
   s" habu-native-suite" GT-START
   GT-POOL-RESET ;

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
