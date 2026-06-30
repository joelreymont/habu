\ perception-analyze-test.f - file-level PERCEPTION analyzer regression.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\        lib/fs-mutate.f lib/float.f lib/sort.f lib/prelude.f lib/render.f
\        tools/json.f tools/json-file.f odin/float-cell.f odin/capture-schema.f
\        odin/capture-schema-json.f odin/perception-latency.f
\        odin/perception-render.f odin/live-records.f odin/perception-analyze.f
\        odin/perception-analyze-test.f

package PERCEPTION
private

$500 constant TB-CAP
$3000 constant ND-CAP
$3000 constant EXP-CAP

create TB TB-CAP allot
create ND ND-CAP allot
create EXP EXP-CAP allot
create PAT-ROOT-BUF FS-PATH-CAP allot
create PAT-IN-BUF FS-PATH-CAP allot

variable TB-N
variable TB-I
variable ND-N
variable EXP-N
variable PAT-ROOT-U
variable PAT-IN-U

: PAT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: PAT-ROOT$ ( -- ptr u8 n )
   PAT-ROOT-BUF PAT-ROOT-U @ ;

: PAT-IN$ ( -- ptr u8 n )
   PAT-IN-BUF PAT-IN-U @ ;

: PAT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-perception-analyze" TMPDIR-MKDIR {: a:ptr u:n :}
   a u PAT-ROOT-BUF PAT-ROOT-U PAT-COPY!
   PAT-ROOT$ CLEANUP-TREE+
   PAT-ROOT$ s" perception.jsonl" PAT-IN-BUF JOIN-PATH PAT-IN-U !
   PAT-IN$ CLEANUP+ ;

: TB+C ( n -- )
   TB TB-N @ + c!
   TB-N @ 1+ TB-N ! ;

: TB+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 TB-I !
   begin TB-I @ u < while
      a TB-I @ + c@ TB+C
      TB-I @ 1+ TB-I !
   repeat ;

: Q$ ( ptr u8 n -- )
   J-DQ TB+C TB+ J-DQ TB+C ;

: KEY: ( ptr u8 n -- )
   Q$ J-COLON TB+C ;

: SVAL ( ptr u8 n ptr u8 n -- ) {: k:ptr ku:n v:ptr vu:n :}
   k ku KEY: v vu Q$ J-COMMA TB+C ;

: RVAL ( ptr u8 n ptr u8 n -- ) {: k:ptr ku:n v:ptr vu:n :}
   k ku KEY: v vu TB+ J-COMMA TB+C ;

: J{ ( -- )
   0 TB-N !
   J-LBRACE TB+C ;

: J} ( -- )
   J-RBRACE TB TB-N @ 1- + c! ;

: J$ ( -- ptr u8 n )
   TB TB-N @ ;

: ND+ ( ptr u8 n -- ) {: a:ptr u:n :}
   ND-N @ u + 1+ ND-CAP > if E-PA-FULL throw then
   a ND ND-N @ + u BYTE-COPY
   ND-N @ u + ND-N !
   10 ND ND-N @ + c!
   ND-N @ 1+ ND-N ! ;

: ND-RESET ( -- )
   0 ND-N ! ;

: ND$ ( -- ptr u8 n )
   ND ND-N @ ;

: CAM-A0 ( -- )
   s" camera_serial" s" 306885122" SVAL
   s" logical_name" s" cam_a0" SVAL ;

: CAM-A1 ( -- )
   s" camera_serial" s" 309091258" SVAL
   s" logical_name" s" cam_a1" SVAL ;

: TT-LINE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: camword:ptr camwordu:n frame:ptr frameu:n sdk:ptr sdku:n upd:ptr updu:n lat:ptr latu:n :}
   J{
   s" schema_version" s" odin.tracker_tick.v1" SVAL
   camword camwordu s" a0" STR= if CAM-A0 else CAM-A1 then
   s" frame_index" frame frameu RVAL
   s" sdk_image_timestamp_ns" sdk sdku RVAL
   s" tracker_source" s" camera_frame_heartbeat" SVAL
   s" tracker_update_index" upd updu RVAL
   s" tracker_timestamp_ns" frame frameu s" 10" STR= if s" 2000000000" else s" 2016666667" then RVAL
   s" latency_ms" lat latu RVAL
   s" queue_depth" s" 0" RVAL
   s" tracks_active" s" 0" RVAL
   J} J$ ND+ ;

: PT-LINE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: camword:ptr camwordu:n frame:ptr frameu:n sdk:ptr sdku:n inf:ptr infu:n lat:ptr latu:n q:ptr qu:n sch:ptr schu:n ten:ptr tenu:n run:ptr runu:n cyc:ptr cycu:n :}
   J{
   s" schema_version" s" odin.perception_tick.v1" SVAL
   camword camwordu s" a0" STR= if CAM-A0 else CAM-A1 then
   s" frame_index" frame frameu RVAL
   s" sdk_image_timestamp_ns" sdk sdku RVAL
   s" tick_source" s" detector_fixture" SVAL
   s" inference_index" inf infu RVAL
   s" detections_count" s" 1" RVAL
   s" latency_ms" lat latu RVAL
   s" queue_depth" q qu RVAL
   s" decision_timestamp_ns" s" 0" RVAL
   s" tracker_update_index" frame frameu s" 10" STR= if s" 1" else s" 2" then RVAL
   s" tracker_timestamp_ns" frame frameu s" 10" STR= if s" 2000000000" else s" 2016666667" then RVAL
   s" schedule_lag_ms" sch schu RVAL
   s" tensor_retrieve_ms" ten tenu RVAL
   s" detector_run_ms" run runu RVAL
   s" detector_cycle_ms" cyc cycu RVAL
   J} J$ ND+ ;

: DET-LINE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: camword:ptr camwordu:n frame:ptr frameu:n sdk:ptr sdku:n lat:ptr latu:n q:ptr qu:n conf:ptr confu:n :}
   J{
   s" schema_version" s" odin.localization_detections.v1" SVAL
   camword camwordu s" a0" STR= if CAM-A0 else CAM-A1 then
   s" frame_index" frame frameu RVAL
   s" sdk_image_timestamp_ns" sdk sdku RVAL
   s" target_id" s" proxy-1" SVAL
   s" pixel_center_x" s" 50.0" RVAL
   s" pixel_center_y" s" 50.0" RVAL
   s" bbox" s" null" RVAL
   s" detection_source" s" detector_fixture" SVAL
   s" confidence" conf confu RVAL
   s" latency_ms" lat latu RVAL
   s" queue_depth" q qu RVAL
   s" decision_timestamp_ns" s" 0" RVAL
   s" tracker_update_index" frame frameu s" 10" STR= if s" 1" else s" 2" then RVAL
   s" tracker_timestamp_ns" frame frameu s" 10" STR= if s" 2000000000" else s" 2016666667" then RVAL
   J} J$ ND+ ;

: BUILD-NDJSON ( -- )
   ND-RESET
   s" a0" s" 10" s" 1000000000" s" 1" s" 4.0" TT-LINE
   s" a1" s" 10" s" 1000000000" s" 1" s" 5.0" TT-LINE
   s" a0" s" 10" s" 1000000000" s" 1" s" 12.0" s" 0" s" 1.0" s" 20.0" s" 30.0" s" 50.0" PT-LINE
   s" a1" s" 10" s" 1000000000" s" 2" s" 18.0" s" 1" s" 2.0" s" 21.0" s" 31.0" s" 52.0" PT-LINE
   s" a0" s" 10" s" 1000000000" s" 12.0" s" 0" s" 0.95" DET-LINE
   s" a1" s" 10" s" 1000000000" s" 18.0" s" 1" s" 0.94" DET-LINE
   s" a0" s" 11" s" 1016666667" s" 2" s" 6.0" TT-LINE
   s" a1" s" 11" s" 1016666667" s" 2" s" 7.0" TT-LINE
   s" a0" s" 11" s" 1016666667" s" 3" s" 24.0" s" 2" s" 3.0" s" 22.0" s" 32.0" s" 54.0" PT-LINE
   s" a1" s" 11" s" 1016666667" s" 4" s" 41.0" s" 2" s" 4.0" s" 23.0" s" 33.0" s" 56.0" PT-LINE
   s" a0" s" 11" s" 1016666667" s" 24.0" s" 2" s" 0.93" DET-LINE
   s" a1" s" 11" s" 1016666667" s" 41.0" s" 2" s" 0.92" DET-LINE ;

: EX+ ( ptr u8 n -- ) {: a:ptr u:n :}
   EXP-N @ u + EXP-CAP > if E-PA-FULL throw then
   a EXP EXP-N @ + u BYTE-COPY
   EXP-N @ u + EXP-N ! ;

: EX-L ( ptr u8 n -- )
   EX+
   EXP-N @ 1+ EXP-CAP > if E-PA-FULL throw then
   10 EXP EXP-N @ + c!
   EXP-N @ 1+ EXP-N ! ;

: EXP$ ( -- ptr u8 n )
   EXP EXP-N @ ;

: EXPECT-METRICS ( -- ptr u8 n )
   0 EXP-N !
   s" metric,value,threshold,pass" EX-L
   s" records,12,," EX-L
   s" detection_records,4,," EX-L
   s" inference_ticks,4,," EX-L
   s" tracker_ticks,4,," EX-L
   s" cameras,2,," EX-L
   s" targets,1,," EX-L
   s" latency_samples,4,," EX-L
   s" latency_ms_p50,18.000,," EX-L
   s" latency_ms_p95,41.000,50.000,yes" EX-L
   s" latency_ms_p99,41.000,80.000,yes" EX-L
   s" latency_ms_max,41.000,," EX-L
   s" detector_rate_min_hz,60.000,50.000,yes" EX-L
   s" detector_rate_mean_hz,60.000,," EX-L
   s" detector_rate_max_hz,60.000,," EX-L
   s" inference_rate_min_hz,60.000,50.000,yes" EX-L
   s" inference_rate_mean_hz,60.000,," EX-L
   s" inference_rate_max_hz,60.000,," EX-L
   s" queue_depth_samples,4,," EX-L
   s" queue_depth_max,2,4,yes" EX-L
   s" queue_depth_mean,1.250,," EX-L
   s" tracker_updates,4,," EX-L
   s" tracker_rate_hz,120.000,," EX-L
   s" tracker_rate_min_hz,60.000,," EX-L
   s" tracker_rate_mean_hz,60.000,," EX-L
   s" tracker_rate_max_hz,60.000,," EX-L
   s" tracker_latency_samples,4,," EX-L
   s" tracker_latency_ms_p50,5.000,," EX-L
   s" tracker_latency_ms_p95,7.000,," EX-L
   s" tracker_latency_ms_p99,7.000,," EX-L
   s" tracker_latency_ms_max,7.000,," EX-L
   s" schedule_lag_samples,4,," EX-L
   s" schedule_lag_ms_p50,2.000,," EX-L
   s" schedule_lag_ms_p95,4.000,," EX-L
   s" schedule_lag_ms_p99,4.000,," EX-L
   s" schedule_lag_ms_max,4.000,," EX-L
   s" tensor_retrieve_samples,4,," EX-L
   s" tensor_retrieve_ms_p50,21.000,," EX-L
   s" tensor_retrieve_ms_p95,23.000,," EX-L
   s" tensor_retrieve_ms_p99,23.000,," EX-L
   s" tensor_retrieve_ms_max,23.000,," EX-L
   s" detector_run_samples,4,," EX-L
   s" detector_run_ms_p50,31.000,," EX-L
   s" detector_run_ms_p95,33.000,," EX-L
   s" detector_run_ms_p99,33.000,," EX-L
   s" detector_run_ms_max,33.000,," EX-L
   s" detector_cycle_samples,4,," EX-L
   s" detector_cycle_ms_p50,52.000,," EX-L
   s" detector_cycle_ms_p95,56.000,," EX-L
   s" detector_cycle_ms_p99,56.000,," EX-L
   s" detector_cycle_ms_max,56.000,," EX-L
   s" result,,,pass" EX-L
   EXP$ ;

: EXPECT-CAMERAS ( -- ptr u8 n )
   0 EXP-N !
   s" camera_serial,logical_name,detections,unique_sdk_frames,first_sdk_image_timestamp_ns,last_sdk_image_timestamp_ns,detector_output_rate_hz,inference_ticks,unique_inference_sdk_frames,first_inference_sdk_image_timestamp_ns,last_inference_sdk_image_timestamp_ns,inference_rate_hz,tracker_ticks,unique_tracker_sdk_frames,first_tracker_timestamp_ns,last_tracker_timestamp_ns,tracker_rate_hz" EX-L
   s" 306885122,cam_a0,2,2,1000000000,1016666667,60.000,2,2,1000000000,1016666667,60.000,2,2,2000000000,2016666667,60.000" EX-L
   s" 309091258,cam_a1,2,2,1000000000,1016666667,60.000,2,2,1000000000,1016666667,60.000,2,2,2000000000,2016666667,60.000" EX-L
   EXP$ ;

: PA-ANALYZE-TEST ( -- )
   T-RESET
   PAT-PREPARE
   BUILD-NDJSON
   PAT-IN$ ND$ WRITE-ALL
   PAT-IN$ PA-ANALYZE-FILE
   50.0 80.0 4 50.0 PA-SET-READINESS
   PL-RENDER EXPECT-METRICS T$=
   PA-CAMERA-METRICS-CSV EXPECT-CAMERAS T$=
   CLEANUP-RUN
   PAT-ROOT$ EXISTS? TFALSE
   T-REPORT ;

PA-ANALYZE-TEST
end-package
