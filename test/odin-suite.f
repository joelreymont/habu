\ odin-suite.f - TEST:* suite adapter for Odin pure-data tests.
\
\ Run from the odin-habu repository root:
\
\   ../habu/bin/hb --load test/odin-suite.f
\   ../habu/bin/hb --load test/odin-suite.f -- --under ../habu/bin/hb

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/runner.f
require test/gate-pool.f

package ODIN-SUITE

120000 constant TIMEOUT-MS
64 constant USAGE-RC

create UNDER-BUF FS-PATH-CAP allot

variable UNDER-U
variable ARG-I

: USAGE ( -- )
   s" usage: test/odin-suite.f [--under PATH] [--pool-slots N]" USAGE-RC die ;

: ARG$ ( -- ptr u8 n )
   ARG-I @ SCRIPT-ARGV$ ;

: ARG-VALUE$ ( -- ptr u8 n )
   ARG-I @ 1+ SCRIPT-ARGC >= if USAGE then
   ARG-I @ 1+ SCRIPT-ARGV$ ;

: ADVANCE ( n -- )
   ARG-I @ + ARG-I ! ;

: POS-NUM ( ptr u8 n -- n )
   STR>NUMBER? 0= if drop USAGE then
   dup 1 < if drop USAGE then ;

: UNDER$ ( -- ptr u8 n )
   UNDER-BUF UNDER-U @ ;

: UNDER! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if USAGE then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a u EXECUTABLE? 0= if E-FS-OPEN throw then
   a UNDER-BUF u BYTE-COPY
   u UNDER-U ! ;

: UNDER-OPT ( -- )
   ARG-VALUE$ UNDER!
   2 ADVANCE ;

: POOL-OPT ( -- )
   ARG-VALUE$ POS-NUM GT-POOL-SLOTS!
   2 ADVANCE ;

: PARSE-ARG ( -- )
   ARG$ s" --under" STR= if UNDER-OPT exit then
   ARG$ s" --pool-slots" STR= if POOL-OPT exit then
   USAGE ;

: CHECK-ARGS ( -- )
   0 UNDER-U !
   0 ARG-I !
   begin ARG-I @ SCRIPT-ARGC < while
      PARSE-ARG
   repeat ;

: HB$ ( -- ptr u8 n )
   UNDER-U @ 0 > if UNDER$ exit then
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop
      s" ../habu/bin/hb" 2dup EXECUTABLE? if exit then
      2drop
      s" bin/hb" 2dup EXECUTABLE? 0= if E-FS-OPEN throw then
      exit
   then
   2dup EXECUTABLE? 0= if E-FS-OPEN throw then ;

: ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: SETUP ( -- )
   CHECK-ARGS
   GT-RESET
   GT-POOL-RESET ;

: TEARDOWN ( -- )
;

: DRAIN ( -- )
   GT-POOL-DRAIN ;

: ARGS-BEGIN ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG+ ;

: SELECT? ( -- bool )
   0 0= ;

: RUNNER ( ptr u8 n -- ) {: label:ptr labelu:n :}
   HB$ label labelu TIMEOUT-MS GT-POOL-START ;

: STDIN-RUNNER ( ptr u8 n ptr u8 n -- )
   2drop 2drop E-FS-OPEN throw ;

: INSTALL ( -- )
   [: SETUP ;] TEST:SETUP!
   [: TEARDOWN ;] TEST:TEARDOWN!
   [: DRAIN ;] TEST:DRAIN!
   [: ARGS-BEGIN ;] TEST:ARGS-BEGIN!
   [: ARG+ ;] TEST:ARG+!
   [: SELECT? ;] TEST:SELECT?!
   [: RUNNER ;] TEST:RUNNER!
   [: STDIN-RUNNER ;] TEST:STDIN-RUNNER! ;

INSTALL
TEST:RESET

TEST:GROUP-PARALLEL odin-pure

TEST:SUITE capture-schema-json
   odin/capture-schema-json-test.f
TEST:END-SUITE

TEST:SUITE capture-schema
   odin/capture-schema-test.f
TEST:END-SUITE

TEST:SUITE config
   odin/config-test.f
TEST:END-SUITE

TEST:SUITE end-to-end
   odin/end-to-end-test.f
TEST:END-SUITE

TEST:SUITE exposure-metrics
   odin/exposure-metrics-test.f
TEST:END-SUITE

TEST:SUITE fps-sweep
   odin/fps-sweep-test.f
TEST:END-SUITE

TEST:SUITE latency-calibration
   odin/latency-calibration-test.f
TEST:END-SUITE

TEST:SUITE latency-xcorr
   odin/latency-xcorr-test.f
TEST:END-SUITE

TEST:SUITE live-records
   odin/live-records-test.f
TEST:END-SUITE

TEST:SUITE low-light-manifest
   odin/low-light-manifest-test.f
TEST:END-SUITE

TEST:SUITE luma-hist
   odin/luma-hist-test.f
TEST:END-SUITE

TEST:SUITE netpbm
   odin/netpbm-test.f
TEST:END-SUITE

TEST:SUITE perception-analyze
   odin/perception-analyze-test.f
TEST:END-SUITE

TEST:SUITE perception-latency-rates
   odin/perception-latency-rates-test.f
TEST:END-SUITE

TEST:SUITE perception-latency
   odin/perception-latency-test.f
TEST:END-SUITE

TEST:SUITE saved-image-scenario
   odin/saved-image-scenario-test.f
TEST:END-SUITE

TEST:SUITE saved-image-analyzers
   odin/saved-image-analyzers-test.f
TEST:END-SUITE

TEST:SUITE capture-backend
   odin/capture-backend-test.f
TEST:END-SUITE

TEST:SUITE cameraone-latency
   odin/cameraone-latency-test.f
TEST:END-SUITE

TEST:SUITE cameraone-latency-scenario
   odin/cameraone-latency-scenario-test.f
TEST:END-SUITE

TEST:SUITE spsc-imu
   odin/spsc-imu-test.f
TEST:END-SUITE

TEST:SUITE spsc-imu-cli
   odin/spsc-imu-cli-test.f
TEST:END-SUITE

TEST:SUITE spsc-motion
   odin/spsc-motion-test.f
TEST:END-SUITE

TEST:SUITE spsc-motion-scenario
   odin/spsc-motion-scenario-test.f
TEST:END-SUITE

TEST:SUITE rig-geometry
   odin/rig-geometry-test.f
TEST:END-SUITE

TEST:SUITE tegrastats
   odin/tegrastats-test.f
TEST:END-SUITE

TEST:SUITE timestamp-metrics
   odin/timestamp-metrics-test.f
TEST:END-SUITE

TEST:SUITE timestamp-phase
   odin/timestamp-phase-test.f
TEST:END-SUITE

TEST:SUITE tracker
   odin/tracker-test.f
TEST:END-SUITE

TEST:SUITE user-assisted-plan
   odin/user-assisted-plan-test.f
TEST:END-SUITE

TEST:SUITE yolo-decode
   odin/yolo-decode-test.f
TEST:END-SUITE

TEST:END-GROUP

TEST:RUN
s" odin test suite: ok" type cr

end-package
