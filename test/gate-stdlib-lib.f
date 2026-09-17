\ gate-stdlib-lib.f - adapter from the native suite registry to bin/hb.

require lib/test.f
require lib/test/runner.f
require test/gate-pool.f
require test/cold-engine.f
require test/whitebox-engine.f

package STDLIB-GATE

360000 constant SUITE-TIMEOUT-MS
64 constant SUITE-USAGE-RC

create WB-BUF FS-PATH-CAP allot
variable WB-U

: SUITE-USAGE ( -- )
   s" usage: bin/hb --load test/run.f" SUITE-USAGE-RC die ;

: SUITE-CHECK-ARGS ( -- )
   ARGC 3 > if 3 SCRIPT-SEP? 0= if SUITE-USAGE then then
   SCRIPT-ARGC 0 <> if SUITE-USAGE then ;

: SUITE-ENV ( -- )
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING ;


: SUITE-RUN-ENV-ASYNC ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n timeout:n label:ptr labelu:n :}
   SUITE-ENV
   path pathu label labelu timeout GT-POOL-START ;



: SUITE-AOT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" test/compiler/codegen-tail-probe.f" STR= if true exit then
   a u s" test/compiler/native-" STARTS-WITH?
   a u s" .f" ENDS-WITH? and ;

: SUITE-ARG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SUITE-AOT? if
      s" test/compiler/aot-mode.f" >LEN PROC-ARGV+
   then
   a u >LEN PROC-ARGV+ ;

: SUITE-HB ( -- )
   PROC-ARGV-RESET
   s" --load" SUITE-ARG+ ;

: SUITE-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" bin/hb" SUITE-TIMEOUT-MS label labelu SUITE-RUN-ENV-ASYNC ;

: WB-PATH$ ( -- ptr u8 n )
   WB-BUF WB-U @ ;

\ The whitebox engine, in the gate's own temp root: a private copy per gate run,
\ so a suite that spawns it cannot reach - or overwrite - the shared keyed
\ artifact, and the tree's bin/hb keeps its seal.
: WB-ENSURE ( -- )
   0 WB-U !
   TEST:WHITEBOX-REGISTERED? 0= if exit then
   s" hb-whitebox" WB-BUF GT-PATH WB-U !
   WB-PATH$ WHITEBOX-ENGINE:PROVIDE ;

: SUITE-WB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   WB-U @ 0 <= if E-FS-PATH throw then
   WB-PATH$ SUITE-TIMEOUT-MS label labelu SUITE-RUN-ENV-ASYNC ;

: SUITE-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- ) {: in:ptr inu:n label:ptr labelu:n :}
   SUITE-ENV
   s" bin/hb" label labelu in inu SUITE-TIMEOUT-MS GT-POOL-START-STDIN ;

\ Emit the shared cold fixture host here, before the first suite forks: the
\ fixtures that need it then copy one keyed artifact instead of each paying the
\ writer's own native build.
: SUITE-SETUP ( -- )
   SUITE-CHECK-ARGS
   s" habu-native-suite" GT-START
   COLD-ENGINE:ENSURE
   WB-ENSURE
   GT-POOL-RESET ;

\ The pool drains softly between groups, so every registered suite runs
\ whatever went red before it; the complete red set is reported here, once,
\ with each suite's exit code, and decides the exit status.
: SUITE-FINISH ( n -- ) {: rc:n :}
   s" suites: ran " type TEST:ITEMS-RUN GT-U-TYPE
   s"  of " type TEST:ITEMS-REGISTERED GT-U-TYPE cr
   GT-POOL-RED-REPORT
   rc 0 <> if exit then                 \ the body threw: the framework rethrows that code
   GT-POOL-RED# 0 > if s" test pool failed" 1 die then
   GT-CLEANUP ;

: SUITE-INSTALL-HOOKS ( -- )
   [: SUITE-SETUP ;] TEST:SETUP!
   [: SUITE-FINISH ;] TEST:TEARDOWN!
   [: GT-POOL-DRAIN-SOFT ;] TEST:DRAIN!
   [: SUITE-HB ;] TEST:ARGS-BEGIN!
   [: SUITE-ARG+ ;] TEST:ARG+!
   [: SUITE-HB-RUN ;] TEST:RUNNER!
   [: SUITE-WB-RUN ;] TEST:WHITEBOX-RUNNER!
   [: SUITE-HB-RUN-STDIN ;] TEST:STDIN-RUNNER! ;

public

: MAIN ( -- )
   SUITE-INSTALL-HOOKS
   TEST:RESET ;

;package
