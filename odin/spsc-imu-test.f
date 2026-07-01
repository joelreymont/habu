\ spsc-imu-test.f - focused SPSC BMI088 capture backend tests.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/test.f
require odin/spsc-imu.f

package SPSCIMU

4096 constant SI-T-READ-CAP
create SI-T-READ SI-T-READ-CAP allot

: SI-T-READ$ ( ptr u8 n -- ptr u8 n )
   SI-T-READ SI-T-READ-CAP READ-ALL {: u:n :}
   SI-T-READ u ;

: SI-TEST-SETUP ( -- )
   RESET
   s" /dev/spsc_bmi0" DEVICE!
   s" cam_a0" LOGICAL!
   s" /tmp/habu-spsc-imu-test" OUTPUT!
   2 SAMPLES!
   TEST-RESET-STATS
   2 TEST-ACCEL-RANGE!
   1 TEST-GYRO-RANGE!
   1000000000 0 0 100 0 0 10 TEST-ADD-RAW
   1100000000 10 0 100 0 10 0 TEST-ADD-RAW ;

: SI-TEST-ARTIFACTS ( -- )
   s" /tmp/habu-spsc-imu-test" MAKE-DIRS
   s" /tmp/habu-spsc-imu-test/imu.ndjson" TEST-NDJSON$ WRITE-ALL
   s" /tmp/habu-spsc-imu-test/summary.md" TEST-SUMMARY$ WRITE-ALL
   s" /tmp/habu-spsc-imu-test/imu.ndjson" FILE? TTRUE
   s" /tmp/habu-spsc-imu-test/summary.md" FILE? TTRUE
   s" /tmp/habu-spsc-imu-test/summary.md" SI-T-READ$ s" # SPSC BMI088 IMU Capture

- device: /dev/spsc_bmi0
- logical name: cam_a0
- samples requested: 2
- samples read: 2
- timed out: no
- start timer: no
- time domain: host_monotonic_ns
- frame: sensor
- accel range code: 2
- gyro range code: 1
- result: pass

| metric | value |
| --- | ---: |
| first timestamp ns | 1000000000 |
| last timestamp ns | 1100000000 |
| duplicate timestamps | 0 |
| timestamp regressions | 0 |
| period samples | 1 |
| period min ns | 100000000 |
| period max ns | 100000000 |
| period mean ns | 100000000 |
| sample rate mean Hz | 10.000 |
" T$= ;

: SI-TEST-RUN ( -- )
   T-RESET
   SI-TEST-SETUP
   SI-TEST-ARTIFACTS ;

SI-TEST-RUN
T-REPORT

end-package
