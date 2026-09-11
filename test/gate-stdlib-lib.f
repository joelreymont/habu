\ gate-stdlib-lib.f - adapter from the native suite registry to bin/hb.

require lib/test.f
require lib/test/runner.f
require test/gate-pool.f

package STDLIB-GATE

360000 constant SUITE-TIMEOUT-MS
64 constant SUITE-USAGE-RC

: SUITE-USAGE ( -- )
   s" usage: bin/hb --load test/run.f" SUITE-USAGE-RC die ;

: SUITE-CHECK-ARGS ( -- )
   ARGC 3 > if 3 SCRIPT-SEP? 0= if SUITE-USAGE then then
   SCRIPT-ARGC 0 <> if SUITE-USAGE then ;

: SUITE-ENV ( -- )
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING ;

: SUITE-RUN-STDIN ( ptr u8 n ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n in:ptr inu:n timeout:n label:ptr labelu:n :}
   SUITE-ENV
   path pathu >LEN PROC-ARGV-CHECK-PATH
   inu 0 < if E-PROC-OUTPUT throw then
   PROC-CAPTURE-RESET
   timeout >MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   PROC-SETUP-STDIN-FDS
   path pathu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE
   in inu >LEN label labelu GT-PROGRESS-STDIN-CAPTURE
   PROC-CLOSE-STDIN-FDS
   PROC-CLOSE-CAPTURE-FDS ;

: SUITE-RUN-ENV-ASYNC ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n timeout:n label:ptr labelu:n :}
   SUITE-ENV
   path pathu label labelu timeout GT-POOL-START ;

: SUITE-FAIL ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" FAIL: " type label labelu type cr
   s" rc: " type GT-RC@ . cr
   GT-OUT$ type
   GT-ERR$ type
   s" native suite failed" 1 die ;

: SUITE-EXPECT-OK ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GT-RC@ 0 <> if label labelu SUITE-FAIL then ;

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

: SUITE-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- ) {: in:ptr inu:n label:ptr labelu:n :}
   GT-POOL-DRAIN
   label labelu GT-PROGRESS-RUN
   s" bin/hb" in inu SUITE-TIMEOUT-MS label labelu SUITE-RUN-STDIN
   label labelu SUITE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: SUITE-SETUP ( -- )
   SUITE-CHECK-ARGS
   s" habu-native-suite" GT-START
   GT-POOL-RESET ;

: SUITE-CLEANUP ( n -- ) {: rc:n :}
   rc 0= GT-POOL-RED# 0= and if GT-CLEANUP then ;

: SUITE-INSTALL-HOOKS ( -- )
   [: SUITE-SETUP ;] TEST:SETUP!
   [: SUITE-CLEANUP ;] TEST:TEARDOWN!
   [: GT-POOL-DRAIN ;] TEST:DRAIN!
   [: SUITE-HB ;] TEST:ARGS-BEGIN!
   [: SUITE-ARG+ ;] TEST:ARG+!
   [: SUITE-HB-RUN ;] TEST:RUNNER!
   [: SUITE-HB-RUN-STDIN ;] TEST:STDIN-RUNNER! ;

public

: MAIN ( -- )
   SUITE-INSTALL-HOOKS
   TEST:RESET ;

;package
