\ capture-schema-json-test.f - validateObject/validateLine oracle.
\ Builds NDJSON lines programmatically (s" can't hold a literal quote), mirroring
\ habu tools/json-test.f's builder idiom. Signatures use type keywords only.
\ Run: ../habu/bin/hb --load odin/capture-schema-json-test.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/test.f
require tools/json.f
require odin/capture-schema.f
require odin/capture-schema-json.f

\ --- tiny JSON line builder ---
package SCHEMA
private
$1000 constant TB-CAP
create TB TB-CAP allot
variable TB-N  variable TB-CP
: TB+C ( n -- ) TB TB-N @ + c!  TB-N @ 1+ TB-N ! ;
: TB+ ( ptr u8 i64 -- ) {: a:ptr u:i64 :}
   0 TB-CP !
   begin TB-CP @ u < while  a TB-CP @ + c@ TB+C  TB-CP @ 1+ TB-CP !  repeat ;
: Q$ ( ptr u8 i64 -- ) {: a:ptr u:i64 :} J-DQ TB+C a u TB+ J-DQ TB+C ;   \ "string"
: J{ ( -- ) 0 TB-N ! J-LBRACE TB+C ;
: J} ( -- ) J-RBRACE  TB TB-N @ 1- +  c! ;            \ overwrite trailing comma with }
: J$ ( -- ptr u8 i64 ) TB TB-N @ ;
: SVAL ( ptr u8 i64 ptr u8 i64 -- ) {: k:ptr ku:i64 v:ptr vu:i64 :}   \ "key":"val",
   k ku Q$ J-COLON TB+C  v vu Q$ J-COMMA TB+C ;
: RVAL ( ptr u8 i64 ptr u8 i64 -- ) {: k:ptr ku:i64 r:ptr ru:i64 :}   \ "key":raw,  (num/bool/null)
   k ku Q$ J-COLON TB+C  r ru TB+ J-COMMA TB+C ;

\ --- record fixtures (return ptr u8 i64) ---
: F-COMMON ( -- ) s" schema_version" s" odin.capture.v1" SVAL ;
: SCHEMA-BODY ( -- )
   s" helper_version" s" 0.0.0" SVAL  s" sdk_version" s" unknown" SVAL
   s" host" s" zed-box" SVAL  s" command" s" capture" SVAL
   s" config_path" s" null" RVAL ;
: L-SCHEMA ( -- ptr u8 i64 )    J{ s" type" s" schema" SVAL F-COMMON SCHEMA-BODY J} J$ ;
: L-SCHEMA-V2 ( -- ptr u8 i64 ) J{ s" type" s" schema" SVAL
   s" schema_version" s" odin.capture.v2" SVAL SCHEMA-BODY J} J$ ;
: L-SCHEMA-MISS ( -- ptr u8 i64 )   \ host omitted
   J{ s" type" s" schema" SVAL F-COMMON
   s" helper_version" s" 0.0.0" SVAL  s" sdk_version" s" unknown" SVAL
   s" command" s" capture" SVAL  s" config_path" s" null" RVAL J} J$ ;
: L-SCHEMA-BADTYPE ( -- ptr u8 i64 )  \ host is a number, not a string
   J{ s" type" s" schema" SVAL F-COMMON
   s" helper_version" s" 0.0.0" SVAL  s" sdk_version" s" unknown" SVAL
   s" host" s" 123" RVAL  s" command" s" capture" SVAL
   s" config_path" s" null" RVAL J} J$ ;
: L-SCHEMA-TSUNIT ( -- ptr u8 i64 )   \ a wrong-unit key present
   J{ s" foo_timestamp_ms" s" 1" RVAL s" type" s" schema" SVAL F-COMMON SCHEMA-BODY J} J$ ;
: L-BADRTYPE ( -- ptr u8 i64 )  J{ s" type" s" bogus" SVAL J} J$ ;

: ERROR-BODY ( -- )
   s" serial" s" null" RVAL  s" code" s" E1" SVAL  s" message" s" boom" SVAL ;
: L-ERROR ( -- ptr u8 i64 )     J{ s" type" s" error" SVAL F-COMMON ERROR-BODY
   s" fatal" s" true" RVAL J} J$ ;
: L-ERROR-BADBOOL ( -- ptr u8 i64 ) J{ s" type" s" error" SVAL F-COMMON ERROR-BODY
   s" fatal" s" yes" SVAL J} J$ ;     \ fatal is a string, not a bool

: SUMMARY-COUNTS ( -- )
   s" serial" s" null" RVAL  s" frames_dropped" s" 0" RVAL  s" duplicates" s" 0" RVAL
   s" timestamp_regressions" s" 0" RVAL  s" writer_stalls" s" 0" RVAL ;
: L-SUMMARY ( -- ptr u8 i64 )   J{ s" type" s" summary" SVAL F-COMMON
   s" frames_seen" s" 1" RVAL SUMMARY-COUNTS  s" result" s" pass" SVAL J} J$ ;
: L-SUMMARY-BADENUM ( -- ptr u8 i64 ) J{ s" type" s" summary" SVAL F-COMMON
   s" frames_seen" s" 1" RVAL SUMMARY-COUNTS  s" result" s" bogus" SVAL J} J$ ;
: L-SUMMARY-FLOATINT ( -- ptr u8 i64 ) J{ s" type" s" summary" SVAL F-COMMON
   s" frames_seen" s" 1.5" RVAL SUMMARY-COUNTS  s" result" s" pass" SVAL J} J$ ;

: L-FRAME ( -- ptr u8 i64 )     J{ s" type" s" frame" SVAL F-COMMON
   s" serial" s" SN1" SVAL  s" logical_name" s" front" SVAL
   s" frame_index" s" 0" RVAL  s" sdk_image_timestamp_ns" s" 1" RVAL
   s" host_monotonic_ns" s" 2" RVAL  s" width" s" 900" RVAL  s" height" s" 600" RVAL
   s" fps_target" s" 60" RVAL  s" pixel_format" s" NV12" SVAL
   s" exposure_us" s" null" RVAL  s" gain" s" 1.5" RVAL  s" auto_exposure" s" true" RVAL
   s" image_path" s" null" RVAL  s" dropped" s" false" RVAL  s" duplicate" s" false" RVAL
   s" timestamp_regressed" s" false" RVAL J} J$ ;
: L-SENSOR ( -- ptr u8 i64 )    J{ s" type" s" sensor" SVAL F-COMMON
   s" serial" s" SN1" SVAL  s" logical_name" s" front" SVAL  s" sensor_kind" s" imu" SVAL
   s" time_reference" s" IMAGE" SVAL  s" sensor_timestamp_ns" s" 1" RVAL
   s" image_frame_index" s" null" RVAL  s" sample_rate_hz" s" 100.0" RVAL
   s" values" s" [1,2,3]" RVAL  s" units" s" mps2" SVAL  s" duplicate" s" false" RVAL
   s" stale" s" false" RVAL  s" missing" s" false" RVAL J} J$ ;
: L-SENSOR-BADENUM ( -- ptr u8 i64 ) J{ s" type" s" sensor" SVAL F-COMMON
   s" serial" s" SN1" SVAL  s" logical_name" s" front" SVAL  s" sensor_kind" s" imu" SVAL
   s" time_reference" s" NOPE" SVAL  s" sensor_timestamp_ns" s" 1" RVAL
   s" image_frame_index" s" null" RVAL  s" sample_rate_hz" s" 100.0" RVAL
   s" values" s" [1,2,3]" RVAL  s" units" s" mps2" SVAL  s" duplicate" s" false" RVAL
   s" stale" s" false" RVAL  s" missing" s" false" RVAL J} J$ ;

: L-DETECTION ( -- ptr u8 i64 ) J{
   s" schema_version" s" odin.localization_detections.v1" SVAL
   s" camera_serial" s" 12345" SVAL  s" logical_name" s" cam_a0" SVAL
   s" frame_index" s" 42" RVAL  s" sdk_image_timestamp_ns" s" 1700000000123456789" RVAL
   s" target_id" s" yolo-0-cam_a0-42-0" SVAL
   s" pixel_center_x" s" 123.5" RVAL  s" pixel_center_y" s" 456.25" RVAL
   s" detection_source" s" zed_yolo_live_detector" SVAL
   s" confidence" s" 0.9" RVAL  s" latency_ms" s" 38.5" RVAL
   s" queue_depth" s" 0" RVAL  s" decision_timestamp_ns" s" 1700000000161956789" RVAL
   s" tracker_update_index" s" 7" RVAL  s" tracker_timestamp_ns" s" 1700000000162956789" RVAL J} J$ ;
: L-DETECTION-NULL-BBOX ( -- ptr u8 i64 ) J{
   s" schema_version" s" odin.localization_detections.v1" SVAL
   s" camera_serial" s" 12345" SVAL  s" logical_name" s" cam_a0" SVAL
   s" frame_index" s" 42" RVAL  s" sdk_image_timestamp_ns" s" 1700000000123456789" RVAL
   s" target_id" s" t0" SVAL
   s" pixel_center_x" s" 123.5" RVAL  s" pixel_center_y" s" 456.25" RVAL
   s" bbox" s" null" RVAL
   s" detection_source" s" fixture" SVAL  s" confidence" s" 0.9" RVAL
   s" latency_ms" s" 38.5" RVAL J} J$ ;
: L-DETECTION-MISS-LAT ( -- ptr u8 i64 ) J{
   s" schema_version" s" odin.localization_detections.v1" SVAL
   s" camera_serial" s" 12345" SVAL  s" logical_name" s" cam_a0" SVAL
   s" frame_index" s" 42" RVAL  s" sdk_image_timestamp_ns" s" 1700000000123456789" RVAL
   s" target_id" s" t0" SVAL  s" pixel_center_x" s" 123.5" RVAL  s" pixel_center_y" s" 456.25" RVAL
   s" detection_source" s" zed_yolo_live_detector" SVAL  s" confidence" s" 0.9" RVAL J} J$ ;
: L-DETECTION-BAD-Q ( -- ptr u8 i64 ) J{
   s" schema_version" s" odin.localization_detections.v1" SVAL
   s" camera_serial" s" 12345" SVAL  s" logical_name" s" cam_a0" SVAL
   s" frame_index" s" 42" RVAL  s" sdk_image_timestamp_ns" s" 1700000000123456789" RVAL
   s" target_id" s" t0" SVAL  s" pixel_center_x" s" 123.5" RVAL  s" pixel_center_y" s" 456.25" RVAL
   s" detection_source" s" zed_yolo_live_detector" SVAL  s" confidence" s" 0.9" RVAL
   s" latency_ms" s" 38.5" RVAL  s" queue_depth" s" zero" SVAL J} J$ ;

: L-PERCEPTION-TICK ( -- ptr u8 i64 ) J{
   s" schema_version" s" odin.perception_tick.v1" SVAL
   s" camera_serial" s" 12345" SVAL  s" logical_name" s" cam_a0" SVAL
   s" frame_index" s" 42" RVAL  s" sdk_image_timestamp_ns" s" 1700000000123456789" RVAL
   s" tick_source" s" zed_yolo_live_detector" SVAL
   s" inference_index" s" 7" RVAL  s" detections_count" s" 0" RVAL
   s" latency_ms" s" 38.5" RVAL  s" queue_depth" s" 0" RVAL
   s" decision_timestamp_ns" s" 1700000000161956789" RVAL
   s" schedule_lag_ms" s" 0" RVAL  s" tensor_retrieve_ms" s" 38.214" RVAL
   s" detector_run_ms" s" 0" RVAL  s" detector_cycle_ms" s" 38.214" RVAL
   s" mode" s" retrieve-only" SVAL J} J$ ;
: L-PERCEPTION-TICK-BAD-TIMING ( -- ptr u8 i64 ) J{
   s" schema_version" s" odin.perception_tick.v1" SVAL
   s" camera_serial" s" 12345" SVAL  s" logical_name" s" cam_a0" SVAL
   s" frame_index" s" 42" RVAL  s" sdk_image_timestamp_ns" s" 1700000000123456789" RVAL
   s" tick_source" s" zed_yolo_live_detector" SVAL
   s" inference_index" s" 7" RVAL  s" detections_count" s" 0" RVAL
   s" latency_ms" s" 38.5" RVAL  s" detector_run_ms" s" slow" SVAL J} J$ ;

: L-TRACKER-TICK ( -- ptr u8 i64 ) J{
   s" schema_version" s" odin.tracker_tick.v1" SVAL
   s" camera_serial" s" 12345" SVAL  s" logical_name" s" cam_a0" SVAL
   s" frame_index" s" 10" RVAL  s" sdk_image_timestamp_ns" s" 1700000000000000000" RVAL
   s" tracker_source" s" camera_frame_heartbeat" SVAL
   s" tracker_update_index" s" 11" RVAL  s" tracker_timestamp_ns" s" 1700000000028000000" RVAL
   s" latency_ms" s" 28" RVAL  s" queue_depth" s" 0" RVAL  s" tracks_active" s" 0" RVAL J} J$ ;
: L-TRACKER-TICK-MISS-ACTIVE ( -- ptr u8 i64 ) J{
   s" schema_version" s" odin.tracker_tick.v1" SVAL
   s" camera_serial" s" 12345" SVAL  s" logical_name" s" cam_a0" SVAL
   s" frame_index" s" 10" RVAL  s" sdk_image_timestamp_ns" s" 1700000000000000000" RVAL
   s" tracker_source" s" camera_frame_heartbeat" SVAL
   s" tracker_update_index" s" 11" RVAL  s" tracker_timestamp_ns" s" 1700000000028000000" RVAL
   s" latency_ms" s" 28" RVAL J} J$ ;
: L-SPECIAL-TSUNIT ( -- ptr u8 i64 ) J{
   s" schema_version" s" odin.perception_tick.v1" SVAL
   s" decision_timestamp_ms" s" 1" RVAL
   s" camera_serial" s" 12345" SVAL  s" logical_name" s" cam_a0" SVAL
   s" frame_index" s" 42" RVAL  s" sdk_image_timestamp_ns" s" 1700000000123456789" RVAL
   s" tick_source" s" zed_yolo_live_detector" SVAL
   s" inference_index" s" 7" RVAL  s" detections_count" s" 0" RVAL
   s" latency_ms" s" 38.5" RVAL J} J$ ;
: L-SPECIAL-UNSUPPORTED ( -- ptr u8 i64 ) J{
   s" schema_version" s" odin.future_tick.v9" SVAL
   s" camera_serial" s" 12345" SVAL J} J$ ;
: L-SPECIAL-BAD-SCHEMA-KIND ( -- ptr u8 i64 ) J{
   s" schema_version" s" 1" RVAL
   s" camera_serial" s" 12345" SVAL J} J$ ;

\ assert a line validates to ( rtype status )
: V= ( ptr u8 i64 i64 i64 -- ) {: rt:i64 st:i64 :} VALIDATE-LINE {: art:i64 ast:i64 :}
   ast st T=  art rt T= ;
: VST= ( ptr u8 i64 i64 -- ) {: st:i64 :} VALIDATE-LINE nip st T= ;  \ status only

: CSJ-RUN ( -- )
   T-RESET
   L-SCHEMA           SCHEMA V-OK V=
   L-FRAME            FRAME  V-OK V=
   L-SENSOR           SENSOR V-OK V=
   L-ERROR            ERROR  V-OK V=
   L-SUMMARY          SUMMARY V-OK V=
   L-DETECTION        DETECTION V-OK V=
   L-DETECTION-NULL-BBOX DETECTION V-OK V=
   L-PERCEPTION-TICK  PERCEPTION-TICK V-OK V=
   L-TRACKER-TICK     TRACKER-TICK V-OK V=
   L-SCHEMA-V2        UNSUPPORTED-SCHEMA VST=
   L-SCHEMA-MISS      MISSING-FIELD VST=
   L-SCHEMA-BADTYPE   INVALID-FIELD-TYPE VST=
   L-SCHEMA-TSUNIT    INVALID-TS-UNITS VST=
   L-BADRTYPE         UNKNOWN-RTYPE VST=
   L-ERROR-BADBOOL    INVALID-FIELD-TYPE VST=
   L-SUMMARY-BADENUM  UNKNOWN-ENUM VST=
   L-SUMMARY-FLOATINT INVALID-FIELD-TYPE VST=
   L-SENSOR-BADENUM   UNKNOWN-ENUM VST=
   L-DETECTION-MISS-LAT MISSING-FIELD VST=
   L-DETECTION-BAD-Q  INVALID-FIELD-TYPE VST=
   L-PERCEPTION-TICK-BAD-TIMING INVALID-FIELD-TYPE VST=
   L-TRACKER-TICK-MISS-ACTIVE MISSING-FIELD VST=
   L-SPECIAL-TSUNIT   INVALID-TS-UNITS VST=
   L-SPECIAL-UNSUPPORTED UNSUPPORTED-SCHEMA VST=
   L-SPECIAL-BAD-SCHEMA-KIND INVALID-FIELD-TYPE VST=
   s" [1,2,3]"        EXPECTED-OBJECT VST=
   s" {oops"          INVALID-JSON VST= ;

CSJ-RUN
T-REPORT
end-package
