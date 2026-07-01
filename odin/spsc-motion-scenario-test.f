\ spsc-motion-scenario-test.f - dry-run tests for the SPSC scenario runner.

require lib/errors.f
require lib/string.f
require lib/test.f
require odin/spsc-motion-scenario.f

package SPSCEN

: SS-TEST-CONFIG ( -- )
   RESET
   s" bump" SCENARIO!
   s" TESTTAG" TAG!
   s" ../Odin" ODIN-ROOT!
   s" results/imu" OUTPUT-ROOT!
   s" results/imu" BASELINE-ROOT!
   s" static_base" BASELINE-SUFFIX!
   25 SAMPLES!
   3000 TIMEOUT-MS!
   100 WINDOW-MS!
   NO-START!
   DRY-RUN!
   FINALIZE ;

: SS-TEST-PATHS ( -- )
   SS-TEST-CONFIG
   OUTPUT-ID$ s" bump_TESTTAG" T$=
   0 CAPTURE-DIR$ s" ../Odin/results/imu/cam_a0_spsc_bump_TESTTAG" T$=
   MOTION-ROOT$ s" ../Odin/results/imu/spsc_motion_bump_TESTTAG" T$=
   3 MOTION-DIR$ s" ../Odin/results/imu/spsc_motion_bump_TESTTAG/cam_b1" T$=
   2 BASELINE$ s" ../Odin/results/imu/cam_b0_static_base/imu.ndjson" T$= ;

: SS-TEST-SUMMARY ( -- )
   SS-TEST-CONFIG
   SUMMARY s" # SPSC Motion Scenario Runner

- execution mode: dry-run
- scenario: bump_TESTTAG
- Odin root: ../Odin
- Habu engine: ../habu/bin/hb
- output root: results/imu
- baseline root: results/imu
- baseline suffix: static_base
- motion root: ../Odin/results/imu/spsc_motion_bump_TESTTAG
- capture launch: concurrent Habu child captures
- samples per device: 25
- timeout ms: 3000
- window ms: 100
- start timer: no
- readiness mode: characterization
- pre-capture delay s: 0
- pre-capture cue: not supplied
- aggregate summary exit status: 0
- first nonzero exit status: 0

## Capture Status

- cam_a0 (/dev/spsc_bmi0): skipped (../Odin/results/imu/cam_a0_spsc_bump_TESTTAG)
- cam_a1 (/dev/spsc_bmi1): skipped (../Odin/results/imu/cam_a1_spsc_bump_TESTTAG)
- cam_b0 (/dev/spsc_bmi2): skipped (../Odin/results/imu/cam_b0_spsc_bump_TESTTAG)
- cam_b1 (/dev/spsc_bmi3): skipped (../Odin/results/imu/cam_b1_spsc_bump_TESTTAG)

## Motion Analysis Status

- cam_a0: skipped (../Odin/results/imu/spsc_motion_bump_TESTTAG/cam_a0)
- cam_a1: skipped (../Odin/results/imu/spsc_motion_bump_TESTTAG/cam_a1)
- cam_b0: skipped (../Odin/results/imu/spsc_motion_bump_TESTTAG/cam_b0)
- cam_b1: skipped (../Odin/results/imu/spsc_motion_bump_TESTTAG/cam_b1)
" T$= ;

: SS-TEST-RUN ( -- )
   T-RESET
   SS-TEST-PATHS
   SS-TEST-SUMMARY ;

SS-TEST-RUN
T-REPORT

end-package
