\ spsc-motion-test.f - focused SPSC motion analyzer tests.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/test.f
require odin/spsc-motion.f

package SPMOT

12000 constant SMT-ND-CAP
2048 constant SMT-LINE-CAP

create SMT-ND SMT-ND-CAP allot
create SMT-LINE SMT-LINE-CAP allot

variable SMT-ND-U
variable SMT-LINE-U
variable SMT-I

: SMT-C ( n -- )
   SMT-LINE-U @ SMT-LINE-CAP >= if E-STR-CAPACITY throw then
   SMT-LINE SMT-LINE-U @ + c!
   SMT-LINE-U @ 1+ SMT-LINE-U ! ;

: SMT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 SMT-I !
   begin SMT-I @ u < while
      a SMT-I @ + c@ SMT-C
      SMT-I @ 1+ SMT-I !
   repeat ;

: SMT-Q$ ( ptr u8 n -- ) 34 SMT-C SMT+ 34 SMT-C ;
: SMT-KEY ( ptr u8 n -- ) SMT-Q$ 58 SMT-C ;
: SMT-SVAL ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n val:ptr valu:n :}
   key keyu SMT-KEY val valu SMT-Q$ 44 SMT-C ;
: SMT-RVAL ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n val:ptr valu:n :}
   key keyu SMT-KEY val valu SMT+ 44 SMT-C ;
: SMT-J{ ( -- ) 0 SMT-LINE-U ! 123 SMT-C ;
: SMT-J} ( -- ) 125 SMT-LINE SMT-LINE-U @ 1- + c! ;
: SMT-LINE$ ( -- ptr u8 n ) SMT-LINE SMT-LINE-U @ ;

: SMT-ND-RESET ( -- ) 0 SMT-ND-U ! ;
: SMT-ND+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   SMT-ND-U @ u + 1+ SMT-ND-CAP >= if E-STR-CAPACITY throw then
   a SMT-ND SMT-ND-U @ + u BYTE-COPY
   SMT-ND-U @ u + SMT-ND-U !
   10 SMT-ND SMT-ND-U @ + c!
   SMT-ND-U @ 1+ SMT-ND-U ! ;
: SMT-ND$ ( -- ptr u8 n ) SMT-ND SMT-ND-U @ ;

: SMT-COMMON-SCHEMA ( -- )
   s" type" s" schema" SMT-SVAL
   s" schema_version" s" odin.external_imu.v1" SMT-SVAL
   s" source" s" fixture BMI088" SMT-SVAL
   s" device" s" /dev/spsc_bmi0" SMT-SVAL
   s" logical_name" s" cam_a0" SMT-SVAL
   s" time_domain" s" host_monotonic_ns" SMT-SVAL
   s" frame" s" sensor" SMT-SVAL ;

: SMT-SCHEMA-LINE ( -- ptr u8 n )
   SMT-J{ SMT-COMMON-SCHEMA SMT-J} SMT-LINE$ ;

: SMT-SAMPLE-LINE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: idx:ptr idxu:n ts:ptr tsu:n accel:ptr accelu:n gyro:ptr gyrou:n :}
   SMT-J{
   s" type" s" imu_sample" SMT-SVAL
   s" schema_version" s" odin.external_imu.v1" SMT-SVAL
   s" sample_index" idx idxu SMT-RVAL
   s" imu_timestamp_ns" ts tsu SMT-RVAL
   s" time_domain" s" host_monotonic_ns" SMT-SVAL
   s" frame" s" sensor" SMT-SVAL
   s" accel_m_s2" accel accelu SMT-RVAL
   s" gyro_rad_s" gyro gyrou SMT-RVAL
   SMT-J} SMT-LINE$ ;

: SM-TEST-MOTION$ ( -- ptr u8 n )
   SMT-ND-RESET
   SMT-SCHEMA-LINE SMT-ND+
   s" 0" s" 1000000000" s" [0,0,9.8]" s" [0,0,0]" SMT-SAMPLE-LINE SMT-ND+
   s" 1" s" 1100000000" s" [1,0,9.8]" s" [0,0,1]" SMT-SAMPLE-LINE SMT-ND+
   s" 2" s" 1200000000" s" [0,2,9.8]" s" [0,2,0]" SMT-SAMPLE-LINE SMT-ND+
   s" 3" s" 1300000000" s" [0,0,12.8]" s" [3,0,0]" SMT-SAMPLE-LINE SMT-ND+
   SMT-ND$ ;

: SM-TEST-BASELINE$ ( -- ptr u8 n )
   SMT-ND-RESET
   SMT-SCHEMA-LINE SMT-ND+
   s" 0" s" 1000000000" s" [0,0,9.8]" s" [0,0,0.1]" SMT-SAMPLE-LINE SMT-ND+
   s" 1" s" 1100000000" s" [0.1,0,9.8]" s" [0,0,0]" SMT-SAMPLE-LINE SMT-ND+
   s" 2" s" 1200000000" s" [0,0.1,9.8]" s" [0.1,0,0]" SMT-SAMPLE-LINE SMT-ND+
   s" 3" s" 1300000000" s" [0,0,9.9]" s" [0,0.1,0]" SMT-SAMPLE-LINE SMT-ND+
   SMT-ND$ ;

: SM-TEST-WRITE-FIXTURES ( -- )
   s" /tmp/habu-spmot-test" MAKE-DIRS
   s" /tmp/habu-spmot-test/out" MAKE-DIRS
   s" /tmp/habu-spmot-test/motion.ndjson" SM-TEST-MOTION$ WRITE-ALL
   s" /tmp/habu-spmot-test/baseline.ndjson" SM-TEST-BASELINE$ WRITE-ALL ;

: SM-TEST-CONFIGURE ( -- )
   RESET
   s" /tmp/habu-spmot-test/motion.ndjson" INPUT!
   s" /tmp/habu-spmot-test/baseline.ndjson" BASELINE!
   s" /tmp/habu-spmot-test/out" OUTPUT!
   200.0 WINDOW-MS!
   2.0 MAX-ACCEL-DYNAMIC-RMS!
   2.0 MAX-GYRO-DYNAMIC-RMS!
   22.0 MAX-ACCEL-DYNAMIC-RATIO!
   22.0 MAX-GYRO-DYNAMIC-RATIO!
   REQUIRE-READY! ;

: SM-READ$ ( ptr u8 n -- ptr u8 n )
   SM-TEXT-BUF SM-READ-CAP READ-ALL {: u:n :}
   SM-TEXT-BUF u ;

: SM-TEST-SNAPSHOTS ( -- )
   s" /tmp/habu-spmot-test/out/metrics.csv" SM-READ$ s" kind,window_index,start_timestamp_ns,end_timestamp_ns,samples,duration_ms,sample_rate_hz_mean,accel_dynamic_rms_m_s2,accel_dynamic_max_m_s2,accel_norm_mean_m_s2,accel_norm_stddev_m_s2,gyro_dynamic_rms_rad_s,gyro_dynamic_max_rad_s,gyro_norm_mean_rad_s,gyro_norm_stddev_rad_s,accel_dynamic_ratio_to_baseline,gyro_dynamic_ratio_to_baseline
overall,-1,1000000000,1300000000,4,300.000,10.000,1.620185,2.318405,10.613222,1.264721,1.620185,2.318405,1.500000,1.118034,21.602469,21.602469
window,0,1000000000,1200000000,2,100.000,10.000,0.500000,0.500000,9.825444,0.025444,0.500000,0.500000,0.500000,0.500000,6.666667,6.666667
window,1,1200000000,1400000000,2,100.000,10.000,1.802776,1.802776,11.401000,1.399000,1.802776,1.802776,2.500000,0.500000,24.037009,24.037009
" T$=
   s" /tmp/habu-spmot-test/out/readiness.csv" SM-READ$ s" metric,value,threshold,pass
samples,4,>0,yes
timestamp_errors,0,0,yes
accel_dynamic_rms_m_s2,1.620185,2.000000,yes
gyro_dynamic_rms_rad_s,1.620185,2.000000,yes
accel_dynamic_ratio_to_baseline,21.602469,22.000000,yes
gyro_dynamic_ratio_to_baseline,21.602469,22.000000,yes
result,,,pass
" T$=
   s" /tmp/habu-spmot-test/out/summary.md" SM-READ$ s" # SPSC Motion Analysis

- input: /tmp/habu-spmot-test/motion.ndjson
- baseline: /tmp/habu-spmot-test/baseline.ndjson
- source: fixture BMI088
- device: /dev/spsc_bmi0
- logical name: cam_a0
- time domain: host_monotonic_ns
- frame: sensor
- window ms: 200.000
- result: comparison

| metric | value |
| --- | ---: |
| samples | 4 |
| first timestamp ns | 1000000000 |
| last timestamp ns | 1300000000 |
| duplicate timestamps | 0 |
| timestamp regressions | 0 |
| period samples | 3 |
| period min ns | 100000000 |
| period max ns | 100000000 |
| period mean ns | 100000000 |
| sample rate mean Hz | 10.000 |
| duration ms | 300.000 |
| accel mean x/y/z m/s^2 | 0.250000 / 0.500000 / 10.550000 |
| accel dynamic RMS m/s^2 | 1.620185 |
| accel dynamic max m/s^2 | 2.318405 |
| accel norm mean m/s^2 | 10.613222 |
| accel norm stddev m/s^2 | 1.264721 |
| gyro mean x/y/z rad/s | 0.750000 / 0.500000 / 0.250000 |
| gyro dynamic RMS rad/s | 1.620185 |
| gyro dynamic max rad/s | 2.318405 |
| gyro norm mean rad/s | 1.500000 |
| gyro norm stddev rad/s | 1.118034 |
| baseline samples | 4 |
| baseline accel dynamic RMS m/s^2 | 0.075000 |
| baseline gyro dynamic RMS rad/s | 0.075000 |
| accel dynamic RMS ratio to baseline | 21.602469 |
| gyro dynamic RMS ratio to baseline | 21.602469 |

## Peak Windows

| metric | window | value |
| --- | ---: | ---: |
| accel dynamic RMS m/s^2 | 1 | 1.802776 |
| gyro dynamic RMS rad/s | 1 | 1.802776 |

## Windows

| window | samples | duration ms | accel dynamic RMS m/s^2 | gyro dynamic RMS rad/s | accel baseline ratio | gyro baseline ratio |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 0 | 2 | 100.000 | 0.500000 | 0.500000 | 6.666667 | 6.666667 |
| 1 | 2 | 100.000 | 1.802776 | 1.802776 | 24.037009 | 24.037009 |
" T$=
   s" /tmp/habu-spmot-test/out/readiness.md" SM-READ$ s" # SPSC Motion Readiness

- result: pass
- require ready: yes
- motion thresholds configured: 4

| metric | value | threshold | pass |
| --- | ---: | ---: | --- |
| samples | 4 | >0 | yes |
| timestamp errors | 0 | 0 | yes |
| accel dynamic RMS | 1.620185 m/s^2 | 2.000000 m/s^2 | yes |
| gyro dynamic RMS | 1.620185 rad/s | 2.000000 rad/s | yes |
| accel dynamic ratio to baseline | 21.602469 | 22.000000 | yes |
| gyro dynamic ratio to baseline | 21.602469 | 22.000000 | yes |

Readiness passes only when timestamp health is clean and every configured motion threshold passes. Ratio thresholds require a baseline log.
" T$= ;

: SM-TEST-ANALYZE ( -- )
   SM-TEST-WRITE-FIXTURES
   SM-TEST-CONFIGURE
   RUN 0 T=
   s" /tmp/habu-spmot-test/out/metrics.csv" FILE? TTRUE
   s" /tmp/habu-spmot-test/out/readiness.csv" FILE? TTRUE
   s" /tmp/habu-spmot-test/out/summary.md" FILE? TTRUE
   s" /tmp/habu-spmot-test/out/readiness.md" FILE? TTRUE
   SM-TEST-SNAPSHOTS ;

: SM-TEST-SCENARIO ( -- )
   s" demo_20260701" s" results/imu/spsc_motion_demo_20260701" SCENARIO-RESET
   s" cam_a0" s" scenario/cam_a0/readiness.csv" s" metric,value,threshold,pass
timestamp_errors,0,0,yes
accel_dynamic_rms_m_s2,0.031740,0.050000,yes
gyro_dynamic_rms_rad_s,0.005371,0.010000,yes
accel_dynamic_ratio_to_baseline,1.000000,1.200000,yes
gyro_dynamic_ratio_to_baseline,1.000000,1.200000,yes
result,,,pass
" SCENARIO-READINESS+
   SCENARIO-RESULT$ s" pass" T$=
   SCENARIO-CSV$ s" logical_name,result,timestamp_errors,accel_dynamic_rms_m_s2,gyro_dynamic_rms_rad_s,accel_dynamic_ratio_to_baseline,gyro_dynamic_ratio_to_baseline,readiness_csv
cam_a0,pass,0,0.031740,0.005371,1.000000,1.000000,scenario/cam_a0/readiness.csv
" T$=
   SCENARIO-MD$ s" # SPSC Motion Scenario Summary

- scenario: demo_20260701
- input root: results/imu/spsc_motion_demo_20260701
- cameras: 1
- result: pass
- max timestamp errors: 0
- max accel dynamic RMS: 0.031740 m/s^2
- max gyro dynamic RMS: 0.005371 rad/s

| logical name | result | timestamp errors | accel dynamic RMS | gyro dynamic RMS | accel ratio | gyro ratio |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| cam_a0 | pass | 0 | 0.031740 m/s^2 | 0.005371 rad/s | 1.000000 | 1.000000 |
" T$= ;

: SM-TEST-RUN ( -- )
   T-RESET
   SM-TEST-ANALYZE
   SM-TEST-SCENARIO ;

SM-TEST-RUN
T-REPORT

end-package
