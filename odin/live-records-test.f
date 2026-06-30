\ live-records-test.f - structures are the JSON/analyzer handoff.
\ Run: cat lib/errors.f lib/string.f lib/memory.f lib/float.f lib/sort.f \
\        lib/hashmap.f lib/test.f tools/json.f odin/float-cell.f \
\        odin/capture-schema.f odin/capture-schema-json.f odin/timestamp-metrics.f \
\        odin/live-records.f odin/live-records-test.f | bin/hb

package ODREC
private

$1000 constant TB-CAP
create TB TB-CAP allot
variable TB-N  variable TB-CP
: TB+C ( n -- ) TB TB-N @ + c!  TB-N @ 1+ TB-N ! ;
: TB+ ( ptr u8 i64 -- ) {: a:ptr u:i64 :}
   0 TB-CP !
   begin TB-CP @ u < while
      a TB-CP @ + c@ TB+C
      TB-CP @ 1+ TB-CP !
   repeat ;
: Q$ ( ptr u8 i64 -- ) {: a:ptr u:i64 :} J-DQ TB+C a u TB+ J-DQ TB+C ;
: J{ ( -- ) 0 TB-N ! J-LBRACE TB+C ;
: J} ( -- ) J-RBRACE TB TB-N @ 1- + c! ;
: J$ ( -- ptr u8 i64 ) TB TB-N @ ;
: SVAL ( ptr u8 i64 ptr u8 i64 -- ) {: k:ptr ku:i64 v:ptr vu:i64 :}
   k ku Q$ J-COLON TB+C v vu Q$ J-COMMA TB+C ;
: RVAL ( ptr u8 i64 ptr u8 i64 -- ) {: k:ptr ku:i64 r:ptr ru:i64 :}
   k ku Q$ J-COLON TB+C r ru TB+ J-COMMA TB+C ;

create FR FRAME-REC allot
create DR DETECTION-REC allot
create PR PERCEPTION-TICK-REC allot
create TR TRACKER-TICK-REC allot

: FL-NEAR ( r r -- bool ) f- fabs 0.000001 f< ;
: T-NEAR ( r r -- ) FL-NEAR T-ASSERT ;

: CAP-COMMON ( -- )
   s" type" s" frame" SVAL  s" schema_version" s" odin.capture.v1" SVAL
   s" serial" s" 306885122" SVAL  s" logical_name" s" cam_a0" SVAL
   s" width" s" 1920" RVAL  s" height" s" 1200" RVAL
   s" fps_target" s" 60" RVAL  s" pixel_format" s" null" SVAL
   s" exposure_us" s" -1" RVAL  s" gain" s" -1" RVAL
   s" auto_exposure" s" true" RVAL  s" image_path" s" null" RVAL
   s" dropped" s" false" RVAL  s" duplicate" s" false" RVAL
   s" timestamp_regressed" s" false" RVAL ;

: L-FRAME0 ( -- ptr u8 i64 ) J{ CAP-COMMON
   s" frame_index" s" 0" RVAL  s" sdk_image_timestamp_ns" s" 1000000000" RVAL
   s" host_monotonic_ns" s" 2000000000" RVAL J} J$ ;
: L-FRAME1 ( -- ptr u8 i64 ) J{ CAP-COMMON
   s" frame_index" s" 1" RVAL  s" sdk_image_timestamp_ns" s" 1016666667" RVAL
   s" host_monotonic_ns" s" 2016666667" RVAL J} J$ ;

: L-DETECTION ( -- ptr u8 i64 ) J{
   s" schema_version" s" odin.localization_detections.v1" SVAL
   s" camera_serial" s" 306885122" SVAL  s" logical_name" s" cam_a0" SVAL
   s" frame_index" s" 42" RVAL  s" sdk_image_timestamp_ns" s" 1700000000123456789" RVAL
   s" target_id" s" proxy-1" SVAL
   s" pixel_center_x" s" 123.5" RVAL  s" pixel_center_y" s" 456.25" RVAL
   s" bbox" s" null" RVAL  s" detection_source" s" zed_yolo_live_detector" SVAL
   s" confidence" s" 0.9" RVAL  s" latency_ms" s" 38.5" RVAL
   s" queue_depth" s" 2" RVAL  s" decision_timestamp_ns" s" 1700000000161956789" RVAL
   s" tracker_update_index" s" 7" RVAL  s" tracker_timestamp_ns" s" 1700000000162956789" RVAL J} J$ ;

: L-PERCEPTION-TICK ( -- ptr u8 i64 ) J{
   s" schema_version" s" odin.perception_tick.v1" SVAL
   s" camera_serial" s" 306885122" SVAL  s" logical_name" s" cam_a0" SVAL
   s" frame_index" s" 43" RVAL  s" sdk_image_timestamp_ns" s" 1700000000140123456" RVAL
   s" tick_source" s" zed_yolo_live_detector" SVAL
   s" inference_index" s" 8" RVAL  s" detections_count" s" 0" RVAL
   s" latency_ms" s" 40.5" RVAL  s" queue_depth" s" 1" RVAL
   s" schedule_lag_ms" s" 3.25" RVAL  s" tensor_retrieve_ms" s" 38.214" RVAL
   s" detector_run_ms" s" 7.5" RVAL  s" detector_cycle_ms" s" 45.714" RVAL
   s" mode" s" full" SVAL J} J$ ;

: L-TRACKER-TICK ( -- ptr u8 i64 ) J{
   s" schema_version" s" odin.tracker_tick.v1" SVAL
   s" camera_serial" s" 306885122" SVAL  s" logical_name" s" cam_a0" SVAL
   s" frame_index" s" 44" RVAL  s" sdk_image_timestamp_ns" s" 1700000000156789012" RVAL
   s" tracker_source" s" camera_frame_heartbeat" SVAL
   s" tracker_update_index" s" 9" RVAL  s" tracker_timestamp_ns" s" 1700000000186789012" RVAL
   s" latency_ms" s" 30.0" RVAL  s" queue_depth" s" 0" RVAL
   s" tracks_active" s" 3" RVAL J} J$ ;

: CHECK-LINE ( ptr u8 i64 i64 -- ) {: rt:i64 :}
   SCHEMA:VALIDATE-LINE {: art:i64 ast:i64 :}
   ast SCHEMA:V-OK T=
   art rt T= ;

: FRAME-FEED ( ptr a -- ) {: rec:ptr :}
   rec FRAME.SERIAL-A @ rec FRAME.SERIAL-N @
   rec FRAME.LOGICAL-A @ rec FRAME.LOGICAL-N @
   rec FRAME.FPS-TARGET @ rec FRAME.FRAME-INDEX @ rec FRAME.SDK-TS @ rec FRAME.HOST-NS @
   rec FRAME.DROPPED @ rec FRAME.DUPLICATE @ rec FRAME.REGRESSED @
   CAMSYNC:TM-ADD ;

: LOAD-FRAME ( ptr u8 i64 -- ) {: a:ptr u:i64 :}
   a u SCHEMA:FRAME CHECK-LINE
   a u JSON-PARSE FR FRAME-LOAD ;

: R-TEST-FRAME ( -- )
   CAMSYNC:TM-RESET
   L-FRAME0 LOAD-FRAME
   FR FRAME.SERIAL-A @ FR FRAME.SERIAL-N @ s" 306885122" T$=
   FR FRAME.LOGICAL-A @ FR FRAME.LOGICAL-N @ s" cam_a0" T$=
   FR FRAME-FEED
   L-FRAME1 LOAD-FRAME
   FR FRAME-FEED
   CAMSYNC:TM-FINISH
   CAMSYNC:TM-COUNT 1 T=
   2 0 CAMSYNC:TM-FRAMES@ T=
   1 0 CAMSYNC:TM-PSAMP@ T=
   16666667 0 CAMSYNC:TM-SDK-MEAN@ T= ;

: R-TEST-DETECTION ( -- )
   L-DETECTION 2dup SCHEMA:DETECTION CHECK-LINE
   JSON-PARSE DR DETECTION-LOAD
   DR DET.CAMERA-A @ DR DET.CAMERA-N @ s" 306885122" T$=
   DR DET.TARGET-A @ DR DET.TARGET-N @ s" proxy-1" T$=
   42 DR DET.FRAME-INDEX @ T=
   123.5 DR DET.CENTER-X F@ T-NEAR
   38.5 DR DET.LATENCY F@ T-NEAR
   DR DET.QUEUE-PRESENT @ TTRUE
   2 DR DET.QUEUE-DEPTH @ T=
   7 DR DET.TRACKER-INDEX @ T= ;

: R-TEST-PERCEPTION-TICK ( -- )
   L-PERCEPTION-TICK 2dup SCHEMA:PERCEPTION-TICK CHECK-LINE
   JSON-PARSE PR PERCEPTION-TICK-LOAD
   PR PT.SOURCE-A @ PR PT.SOURCE-N @ s" zed_yolo_live_detector" T$=
   8 PR PT.INFERENCE-INDEX @ T=
   0 PR PT.DETECTIONS-COUNT @ T=
   40.5 PR PT.LATENCY F@ T-NEAR
   PR PT.SCHEDULE-LAG-PRESENT @ TTRUE
   3.25 PR PT.SCHEDULE-LAG F@ T-NEAR
   PR PT.MODE-A @ PR PT.MODE-N @ s" full" T$= ;

: R-TEST-TRACKER-TICK ( -- )
   L-TRACKER-TICK 2dup SCHEMA:TRACKER-TICK CHECK-LINE
   JSON-PARSE TR TRACKER-TICK-LOAD
   TR TT.SOURCE-A @ TR TT.SOURCE-N @ s" camera_frame_heartbeat" T$=
   9 TR TT.UPDATE-INDEX @ T=
   3 TR TT.TRACKS-ACTIVE @ T=
   30.0 TR TT.LATENCY F@ T-NEAR
   TR TT.QUEUE-PRESENT @ TTRUE
   0 TR TT.QUEUE-DEPTH @ T= ;

: R-RUN ( -- )
   T-RESET
   R-TEST-FRAME
   R-TEST-DETECTION
   R-TEST-PERCEPTION-TICK
   R-TEST-TRACKER-TICK ;

R-RUN
T-REPORT
end-package
