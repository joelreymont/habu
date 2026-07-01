\ saved-image-analyzers-test.f - public Habu saved-image analyzer entrypoint tests.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/test.f
require odin/saved-image-analyzers.f

package SIA-TEST
private

$4000 constant TB-CAP
$10000 constant ND-CAP

create TB TB-CAP allot
create ND ND-CAP allot
variable TB-N
variable TB-I
variable ND-N
variable ND-I

\ P5 3x3 image: "P5\n3 3\n255\n" + nine luma bytes.
create P5A 80 c, 53 c, 10 c, 51 c, 32 c, 51 c, 10 c, 50 c, 53 c, 53 c, 10 c,
   10 c, 20 c, 30 c, 40 c, 50 c, 60 c, 70 c, 80 c, 90 c,
here P5A - constant P5A-LEN
create P5B 80 c, 53 c, 10 c, 51 c, 32 c, 51 c, 10 c, 50 c, 53 c, 53 c, 10 c,
   12 c, 22 c, 32 c, 42 c, 52 c, 62 c, 72 c, 82 c, 92 c,
here P5B - constant P5B-LEN

: TB-C ( n -- ) TB TB-N @ + c! TB-N @ 1+ TB-N ! ;
: TB+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 TB-I !
   begin TB-I @ u < while
      a TB-I @ + c@ TB-C
      TB-I @ 1+ TB-I !
   repeat ;
: Q$ ( ptr u8 n -- ) 34 TB-C TB+ 34 TB-C ;
: KEY ( ptr u8 n -- ) Q$ 58 TB-C ;
: SVAL ( ptr u8 n ptr u8 n -- ) {: k:ptr ku:n v:ptr vu:n :}
   k ku KEY v vu Q$ 44 TB-C ;
: RVAL ( ptr u8 n ptr u8 n -- ) {: k:ptr ku:n v:ptr vu:n :}
   k ku KEY v vu TB+ 44 TB-C ;
: J{ ( -- ) 0 TB-N ! 123 TB-C ;
: J} ( -- ) 125 TB TB-N @ 1- + c! ;
: J$ ( -- ptr u8 n ) TB TB-N @ ;

: ND-RESET ( -- ) 0 ND-N ! ;
: ND+LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   0 ND-I !
   begin ND-I @ u < while
      a ND-I @ + c@ ND ND-N @ + c!
      ND-N @ 1+ ND-N !
      ND-I @ 1+ ND-I !
   repeat
   10 ND ND-N @ + c!
   ND-N @ 1+ ND-N ! ;
: ND$ ( -- ptr u8 n ) ND ND-N @ ;

: SCHEMA-LINE ( -- )
   J{
   s" type" s" schema" SVAL
   s" schema_version" s" odin.capture.v1" SVAL
   s" helper_version" s" habu-test" SVAL
   s" sdk_version" s" test" SVAL
   s" host" s" zed-box" SVAL
   s" command" s" capture-save-multi" SVAL
   s" config_path" s" null" RVAL
   J} J$ ND+LINE ;

: FRAME-LINE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ser:ptr seru:n lna:ptr lnau:n fidx:ptr fidxu:n sdk:ptr sdku:n host:ptr hostu:n image:ptr imageu:n :}
   J{
   s" type" s" frame" SVAL
   s" schema_version" s" odin.capture.v1" SVAL
   s" serial" ser seru SVAL
   s" logical_name" lna lnau SVAL
   s" frame_index" fidx fidxu RVAL
   s" sdk_image_timestamp_ns" sdk sdku RVAL
   s" host_monotonic_ns" host hostu RVAL
   s" width" s" 3" RVAL
   s" height" s" 3" RVAL
   s" fps_target" s" 60" RVAL
   s" pixel_format" s" p5" SVAL
   s" exposure_us" s" 4000" RVAL
   s" gain" s" 1000" RVAL
   s" auto_exposure" s" false" RVAL
   s" image_path" image imageu SVAL
   s" dropped" s" false" RVAL
   s" duplicate" s" false" RVAL
   s" timestamp_regressed" s" false" RVAL
   J} J$ ND+LINE ;

: SUMMARY-LINE ( ptr u8 n -- )
   {: ser:ptr seru:n :}
   J{
   s" type" s" summary" SVAL
   s" schema_version" s" odin.capture.v1" SVAL
   s" serial" ser seru SVAL
   s" frames_seen" s" 3" RVAL
   s" frames_dropped" s" 0" RVAL
   s" duplicates" s" 0" RVAL
   s" timestamp_regressions" s" 0" RVAL
   s" writer_stalls" s" 0" RVAL
   s" result" s" pass" SVAL
   J} J$ ND+LINE ;

: WRITE-FIXTURE ( -- )
   s" /tmp/habu-sia-test/capture/images/cam_a0" MAKE-DIRS
   s" /tmp/habu-sia-test/capture/images/cam_a1" MAKE-DIRS
   s" /tmp/habu-sia-test/exposure" MAKE-DIRS
   s" /tmp/habu-sia-test/low_light" MAKE-DIRS
   s" /tmp/habu-sia-test/motion_blur" MAKE-DIRS
   s" /tmp/habu-sia-test/sync" MAKE-DIRS
   s" /tmp/habu-sia-test/capture/images/cam_a0/000000.pgm" P5A P5A-LEN WRITE-ALL
   s" /tmp/habu-sia-test/capture/images/cam_a0/000001.pgm" P5B P5B-LEN WRITE-ALL
   s" /tmp/habu-sia-test/capture/images/cam_a0/000002.pgm" P5A P5A-LEN WRITE-ALL
   s" /tmp/habu-sia-test/capture/images/cam_a1/000000.pgm" P5B P5B-LEN WRITE-ALL
   s" /tmp/habu-sia-test/capture/images/cam_a1/000001.pgm" P5A P5A-LEN WRITE-ALL
   s" /tmp/habu-sia-test/capture/images/cam_a1/000002.pgm" P5B P5B-LEN WRITE-ALL
   ND-RESET
   SCHEMA-LINE
   s" 306885122" s" cam_a0" s" 0" s" 1000000000" s" 2000000000" s" images/cam_a0/000000.pgm" FRAME-LINE
   s" 309091258" s" cam_a1" s" 0" s" 1000100000" s" 2000100000" s" images/cam_a1/000000.pgm" FRAME-LINE
   s" 306885122" s" cam_a0" s" 1" s" 1016666667" s" 2016666667" s" images/cam_a0/000001.pgm" FRAME-LINE
   s" 309091258" s" cam_a1" s" 1" s" 1016766667" s" 2016766667" s" images/cam_a1/000001.pgm" FRAME-LINE
   s" 306885122" s" cam_a0" s" 2" s" 1033333334" s" 2033333334" s" images/cam_a0/000002.pgm" FRAME-LINE
   s" 309091258" s" cam_a1" s" 2" s" 1033433334" s" 2033433334" s" images/cam_a1/000002.pgm" FRAME-LINE
   s" 306885122" SUMMARY-LINE
   s" 309091258" SUMMARY-LINE
   s" /tmp/habu-sia-test/capture/combined.ndjson" ND$ WRITE-ALL ;

: RUN ( -- )
   T-RESET
   WRITE-FIXTURE
   s" /tmp/habu-sia-test/capture/combined.ndjson" s" /tmp/habu-sia-test/capture" s" /tmp/habu-sia-test/exposure" SIA:ANALYZE-EXPOSURE 0 T=
   s" /tmp/habu-sia-test/exposure/metrics.csv" FILE? T-ASSERT
   s" /tmp/habu-sia-test/exposure/summary.md" FILE? T-ASSERT
   s" /tmp/habu-sia-test/capture/combined.ndjson" s" /tmp/habu-sia-test/capture" s" /tmp/habu-sia-test/low_light" SIA:ANALYZE-LOW-LIGHT 0 T=
   s" /tmp/habu-sia-test/low_light/metrics.csv" FILE? T-ASSERT
   s" /tmp/habu-sia-test/capture/combined.ndjson" s" /tmp/habu-sia-test/capture" s" /tmp/habu-sia-test/motion_blur" SIA:ANALYZE-MOTION-BLUR 0 T=
   s" /tmp/habu-sia-test/motion_blur/metrics.csv" FILE? T-ASSERT
   s" /tmp/habu-sia-test/capture/combined.ndjson" s" /tmp/habu-sia-test/sync" 1 1 SIA:ANALYZE-SYNC 0 T=
   s" /tmp/habu-sia-test/sync/frame_sync.csv" FILE? T-ASSERT
   s" /tmp/habu-sia-test/sync/sync_readiness.csv" FILE? T-ASSERT ;

RUN
T-REPORT

end-package
