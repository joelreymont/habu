\ cameraone-latency-test.f - CameraOne image-time latency pipeline fixture.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/render.f
require lib/test.f
require odin/cameraone-latency.f

package COLAT-TEST
private

$4000 constant CT-TB-CAP
$10000 constant CT-ND-CAP
$4000 constant CT-READ-CAP

create CT-TB CT-TB-CAP allot
create CT-ND CT-ND-CAP allot
create CT-READ-BUF CT-READ-CAP allot

variable CT-TB-N
variable CT-TB-I
variable CT-ND-N
variable CT-ND-I

\ P5 2x2 images with stable mean luma 10, 100, and 20.
create CT-P5-DARK 80 c, 53 c, 10 c, 50 c, 32 c, 50 c, 10 c, 50 c, 53 c, 53 c, 10 c,
   10 c, 10 c, 10 c, 10 c,
here CT-P5-DARK - constant CT-P5-DARK-LEN
create CT-P5-BRIGHT 80 c, 53 c, 10 c, 50 c, 32 c, 50 c, 10 c, 50 c, 53 c, 53 c, 10 c,
   100 c, 100 c, 100 c, 100 c,
here CT-P5-BRIGHT - constant CT-P5-BRIGHT-LEN
create CT-P5-DIM 80 c, 53 c, 10 c, 50 c, 32 c, 50 c, 10 c, 50 c, 53 c, 53 c, 10 c,
   20 c, 20 c, 20 c, 20 c,
here CT-P5-DIM - constant CT-P5-DIM-LEN

: CT-C ( n -- )
   CT-TB CT-TB-N @ + c!
   CT-TB-N @ 1+ CT-TB-N ! ;

: CT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 CT-TB-I !
   begin CT-TB-I @ u < while
      a CT-TB-I @ + c@ CT-C
      CT-TB-I @ 1+ CT-TB-I !
   repeat ;

: CT-Q$ ( ptr u8 n -- )
   34 CT-C CT+ 34 CT-C ;

: CT-KEY ( ptr u8 n -- )
   CT-Q$ 58 CT-C ;

: CT-SVAL ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n val:ptr valu:n :}
   key keyu CT-KEY val valu CT-Q$ 44 CT-C ;

: CT-RVAL ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n val:ptr valu:n :}
   key keyu CT-KEY val valu CT+ 44 CT-C ;

: CT-J{ ( -- )
   0 CT-TB-N !
   123 CT-C ;

: CT-J} ( -- )
   125 CT-TB CT-TB-N @ 1- + c! ;

: CT-J$ ( -- ptr u8 n )
   CT-TB CT-TB-N @ ;

: CT-ND-RESET ( -- )
   0 CT-ND-N ! ;

: CT-ND+LINE ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 CT-ND-I !
   begin CT-ND-I @ u < while
      a CT-ND-I @ + c@ CT-ND CT-ND-N @ + c!
      CT-ND-N @ 1+ CT-ND-N !
      CT-ND-I @ 1+ CT-ND-I !
   repeat
   10 CT-ND CT-ND-N @ + c!
   CT-ND-N @ 1+ CT-ND-N ! ;

: CT-ND$ ( -- ptr u8 n )
   CT-ND CT-ND-N @ ;

: CT-COMMON ( -- )
   s" schema_version" s" odin.capture.v1" CT-SVAL ;

: CT-SCHEMA-LINE ( -- )
   CT-J{
   s" type" s" schema" CT-SVAL
   CT-COMMON
   s" helper_version" s" habu-test" CT-SVAL
   s" sdk_version" s" test" CT-SVAL
   s" host" s" zed-box" CT-SVAL
   s" command" s" capture-save-multi" CT-SVAL
   s" config_path" s" null" CT-RVAL
   CT-J} CT-J$ CT-ND+LINE ;

: CT-FRAME-LINE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: frame:ptr frameu:n ts:ptr tsu:n image:ptr imageu:n :}
   CT-J{
   s" type" s" frame" CT-SVAL
   CT-COMMON
   s" serial" s" SN-CAM" CT-SVAL
   s" logical_name" s" cam_a0" CT-SVAL
   s" frame_index" frame frameu CT-RVAL
   s" sdk_image_timestamp_ns" ts tsu CT-RVAL
   s" host_monotonic_ns" ts tsu CT-RVAL
   s" width" s" 2" CT-RVAL
   s" height" s" 2" CT-RVAL
   s" fps_target" s" 60" CT-RVAL
   s" pixel_format" s" p5" CT-SVAL
   s" exposure_us" s" 4000" CT-RVAL
   s" gain" s" 1000" CT-RVAL
   s" auto_exposure" s" false" CT-RVAL
   s" image_path" image imageu CT-SVAL
   s" dropped" s" false" CT-RVAL
   s" duplicate" s" false" CT-RVAL
   s" timestamp_regressed" s" false" CT-RVAL
   CT-J} CT-J$ CT-ND+LINE ;

: CT-SENSOR-LINE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: frame:ptr frameu:n ts:ptr tsu:n values:ptr valuesu:n :}
   CT-J{
   s" type" s" sensor" CT-SVAL
   CT-COMMON
   s" serial" s" SN-CAM" CT-SVAL
   s" logical_name" s" cam_a0" CT-SVAL
   s" sensor_kind" s" imu" CT-SVAL
   s" time_reference" s" IMAGE" CT-SVAL
   s" sensor_timestamp_ns" ts tsu CT-RVAL
   s" image_frame_index" frame frameu CT-RVAL
   s" sample_rate_hz" s" 60.0" CT-RVAL
   s" values" values valuesu CT-RVAL
   s" units" s" accel_m_s2,gyro_rad_s" CT-SVAL
   s" duplicate" s" false" CT-RVAL
   s" stale" s" false" CT-RVAL
   s" missing" s" false" CT-RVAL
   CT-J} CT-J$ CT-ND+LINE ;

: CT-SUMMARY-LINE ( -- )
   CT-J{
   s" type" s" summary" CT-SVAL
   CT-COMMON
   s" serial" s" SN-CAM" CT-SVAL
   s" frames_seen" s" 3" CT-RVAL
   s" frames_dropped" s" 0" CT-RVAL
   s" duplicates" s" 0" CT-RVAL
   s" timestamp_regressions" s" 0" CT-RVAL
   s" writer_stalls" s" 0" CT-RVAL
   s" result" s" pass" CT-SVAL
   CT-J} CT-J$ CT-ND+LINE ;

: CT-WRITE-IMAGES ( -- )
   s" /tmp/habu-colat-test/capture/images/cam_a0" MAKE-DIRS
   s" /tmp/habu-colat-test/capture/images/cam_a0/000000.pgm" CT-P5-DARK CT-P5-DARK-LEN WRITE-ALL
   s" /tmp/habu-colat-test/capture/images/cam_a0/000001.pgm" CT-P5-BRIGHT CT-P5-BRIGHT-LEN WRITE-ALL
   s" /tmp/habu-colat-test/capture/images/cam_a0/000002.pgm" CT-P5-DIM CT-P5-DIM-LEN WRITE-ALL ;

: CT-WRITE-NDJSON ( -- )
   CT-ND-RESET
   CT-SCHEMA-LINE
   s" 0" s" 1000000000" s" images/cam_a0/000000.pgm" CT-FRAME-LINE
   s" 0" s" 1000000000" s" [0.0,0.0,9.8,0.0,0.0,0.0]" CT-SENSOR-LINE
   s" 1" s" 1010000000" s" images/cam_a0/000001.pgm" CT-FRAME-LINE
   s" 1" s" 1010000000" s" [0.0,0.0,12.0,0.0,0.0,0.0]" CT-SENSOR-LINE
   s" 2" s" 1020000000" s" images/cam_a0/000002.pgm" CT-FRAME-LINE
   s" 2" s" 1020000000" s" [0.0,0.0,9.8,0.0,0.0,0.0]" CT-SENSOR-LINE
   CT-SUMMARY-LINE
   s" /tmp/habu-colat-test/capture/combined.ndjson" CT-ND$ WRITE-ALL ;

: CT-WRITE-FIXTURE ( -- )
   s" /tmp/habu-colat-test" REMOVE-TREE
   CT-WRITE-IMAGES
   s" /tmp/habu-colat-test/out" MAKE-DIRS
   CT-WRITE-NDJSON ;

: CT-READ$ ( ptr u8 n -- ptr u8 n )
   CT-READ-BUF CT-READ-CAP READ-ALL
   CT-READ-BUF swap ;

: CT-EXPECT-LATENCY-METRICS ( -- ptr u8 n )
   RB-RESET
   s" camera_events,imu_events,matched_events,match_mode,camera_duplicate_ids,imu_duplicate_ids,camera_missing_imu,imu_missing_camera,offset_ns,offset_provided,camera_time_domain,imu_time_domain,latency_mean_ns,latency_median_ns,latency_min_ns,latency_max_ns,residual_p95_ns,residual_max_ns,max_jitter_ns,events_outside_jitter,result" RB+ RB-NL
   s" 1,1,1,order,0,0,0,0,0,no,sdk_image_timestamp_ns,sdk_image_timestamp_ns,0,0,0,0,0,0,500000,0,pass" RB+ RB-NL
   RB$ ;

: CT-ASSERT-FILE ( ptr u8 n -- )
   FILE? T-ASSERT ;

: CT-ASSERT-CONTAINS ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n needle:ptr needleu:n :}
   path pathu CT-READ$ needle needleu CONTAINS? TTRUE ;

: CT-ASSERT-LATENCY-METRICS ( -- )
   s" /tmp/habu-colat-test/out/latency/latency_calibration/metrics.csv" CT-READ$
   CT-EXPECT-LATENCY-METRICS STR= T-ASSERT ;

: CT-RUN ( -- )
   T-RESET
   CT-WRITE-FIXTURE
   COLAT:RESET
   s" cam_a0" COLAT:LOGICAL!
   s" /tmp/habu-colat-test/capture/combined.ndjson"
   s" /tmp/habu-colat-test/capture"
   s" /tmp/habu-colat-test/out" COLAT:ANALYZE 0 T=
   s" /tmp/habu-colat-test/out/cameraone_imu/external_imu.ndjson" CT-ASSERT-FILE
   s" /tmp/habu-colat-test/out/cameraone_imu/samples.csv" CT-ASSERT-FILE
   s" /tmp/habu-colat-test/out/latency/camera_events/events.ndjson" CT-ASSERT-FILE
   s" /tmp/habu-colat-test/out/latency/imu_events/events.ndjson" CT-ASSERT-FILE
   s" /tmp/habu-colat-test/out/latency/latency_calibration/events.csv" CT-ASSERT-FILE
   CT-ASSERT-LATENCY-METRICS
   s" /tmp/habu-colat-test/out/cameraone_imu/samples.csv" s" 1,1,1010000000,0.000000,0.000000,12.000000,0.000000,0.000000,0.000000" CT-ASSERT-CONTAINS
   s" /tmp/habu-colat-test/out/latency/camera_events/events.ndjson" s" luminance_delta" CT-ASSERT-CONTAINS
   s" /tmp/habu-colat-test/out/latency/imu_events/events.ndjson" s" sample_index" CT-ASSERT-CONTAINS ;

CT-RUN
T-REPORT

end-package
