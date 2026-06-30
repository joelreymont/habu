\ live-records.f - checked structures for Odin capture/live detector NDJSON rows.
\
\ These structures are the typed handoff between SCHEMA's JSON validation and
\ analyzer kernels. Load after lib/string.f lib/float.f tools/json.f.

package ODREC
public

BEGIN-STRUCTURE FRAME-REC
   CELL +FIELD FRAME._SERIAL-A
   CELL +FIELD FRAME._SERIAL-N
   CELL +FIELD FRAME._LOGICAL-A
   CELL +FIELD FRAME._LOGICAL-N
   CELL +FIELD FRAME._FPS-TARGET
   CELL +FIELD FRAME._FRAME-INDEX
   CELL +FIELD FRAME._SDK-TS
   CELL +FIELD FRAME._HOST-NS
   CELL +FIELD FRAME._DROPPED
   CELL +FIELD FRAME._DUPLICATE
   CELL +FIELD FRAME._REGRESSED
END-STRUCTURE

BEGIN-STRUCTURE DETECTION-REC
   CELL +FIELD DET._CAMERA-A
   CELL +FIELD DET._CAMERA-N
   CELL +FIELD DET._LOGICAL-A
   CELL +FIELD DET._LOGICAL-N
   CELL +FIELD DET._FRAME-INDEX
   CELL +FIELD DET._SDK-TS
   CELL +FIELD DET._TARGET-A
   CELL +FIELD DET._TARGET-N
   CELL +FIELD DET._CENTER-X
   CELL +FIELD DET._CENTER-Y
   CELL +FIELD DET._SOURCE-A
   CELL +FIELD DET._SOURCE-N
   CELL +FIELD DET._CONFIDENCE
   CELL +FIELD DET._LATENCY
   CELL +FIELD DET._QUEUE-PRESENT
   CELL +FIELD DET._QUEUE-DEPTH
   CELL +FIELD DET._DECISION-PRESENT
   CELL +FIELD DET._DECISION-TS
   CELL +FIELD DET._TRACKER-INDEX-PRESENT
   CELL +FIELD DET._TRACKER-INDEX
   CELL +FIELD DET._TRACKER-TS-PRESENT
   CELL +FIELD DET._TRACKER-TS
END-STRUCTURE

BEGIN-STRUCTURE PERCEPTION-TICK-REC
   CELL +FIELD PT._CAMERA-A
   CELL +FIELD PT._CAMERA-N
   CELL +FIELD PT._LOGICAL-A
   CELL +FIELD PT._LOGICAL-N
   CELL +FIELD PT._FRAME-INDEX
   CELL +FIELD PT._SDK-TS
   CELL +FIELD PT._SOURCE-A
   CELL +FIELD PT._SOURCE-N
   CELL +FIELD PT._INFERENCE-INDEX
   CELL +FIELD PT._DETECTIONS-COUNT
   CELL +FIELD PT._LATENCY
   CELL +FIELD PT._QUEUE-PRESENT
   CELL +FIELD PT._QUEUE-DEPTH
   CELL +FIELD PT._DECISION-PRESENT
   CELL +FIELD PT._DECISION-TS
   CELL +FIELD PT._TRACKER-INDEX-PRESENT
   CELL +FIELD PT._TRACKER-INDEX
   CELL +FIELD PT._TRACKER-TS-PRESENT
   CELL +FIELD PT._TRACKER-TS
   CELL +FIELD PT._SCHEDULE-LAG-PRESENT
   CELL +FIELD PT._SCHEDULE-LAG
   CELL +FIELD PT._TENSOR-RETRIEVE-PRESENT
   CELL +FIELD PT._TENSOR-RETRIEVE
   CELL +FIELD PT._DETECTOR-RUN-PRESENT
   CELL +FIELD PT._DETECTOR-RUN
   CELL +FIELD PT._DETECTOR-CYCLE-PRESENT
   CELL +FIELD PT._DETECTOR-CYCLE
   CELL +FIELD PT._MODE-A
   CELL +FIELD PT._MODE-N
END-STRUCTURE

BEGIN-STRUCTURE TRACKER-TICK-REC
   CELL +FIELD TT._CAMERA-A
   CELL +FIELD TT._CAMERA-N
   CELL +FIELD TT._LOGICAL-A
   CELL +FIELD TT._LOGICAL-N
   CELL +FIELD TT._FRAME-INDEX
   CELL +FIELD TT._SDK-TS
   CELL +FIELD TT._SOURCE-A
   CELL +FIELD TT._SOURCE-N
   CELL +FIELD TT._UPDATE-INDEX
   CELL +FIELD TT._TRACKER-TS
   CELL +FIELD TT._LATENCY
   CELL +FIELD TT._QUEUE-PRESENT
   CELL +FIELD TT._QUEUE-DEPTH
   CELL +FIELD TT._TRACKS-ACTIVE
END-STRUCTURE

private

: CELL-FIELD ( ptr a n -- ptr a ) cells + ;

public

: FRAME.SERIAL-A ( ptr a -- ptr ptr u8 ) 0 ptr-field ;
: FRAME.SERIAL-N ( ptr a -- ptr a ) 1 CELL-FIELD ;
: FRAME.LOGICAL-A ( ptr a -- ptr ptr u8 ) 2 ptr-field ;
: FRAME.LOGICAL-N ( ptr a -- ptr a ) 3 CELL-FIELD ;
: FRAME.FPS-TARGET ( ptr a -- ptr a ) 4 CELL-FIELD ;
: FRAME.FRAME-INDEX ( ptr a -- ptr a ) 5 CELL-FIELD ;
: FRAME.SDK-TS ( ptr a -- ptr a ) 6 CELL-FIELD ;
: FRAME.HOST-NS ( ptr a -- ptr a ) 7 CELL-FIELD ;
: FRAME.DROPPED ( ptr a -- ptr a ) 8 CELL-FIELD ;
: FRAME.DUPLICATE ( ptr a -- ptr a ) 9 CELL-FIELD ;
: FRAME.REGRESSED ( ptr a -- ptr a ) 10 CELL-FIELD ;

: DET.CAMERA-A ( ptr a -- ptr ptr u8 ) 0 ptr-field ;
: DET.CAMERA-N ( ptr a -- ptr a ) 1 CELL-FIELD ;
: DET.LOGICAL-A ( ptr a -- ptr ptr u8 ) 2 ptr-field ;
: DET.LOGICAL-N ( ptr a -- ptr a ) 3 CELL-FIELD ;
: DET.FRAME-INDEX ( ptr a -- ptr a ) 4 CELL-FIELD ;
: DET.SDK-TS ( ptr a -- ptr a ) 5 CELL-FIELD ;
: DET.TARGET-A ( ptr a -- ptr ptr u8 ) 6 ptr-field ;
: DET.TARGET-N ( ptr a -- ptr a ) 7 CELL-FIELD ;
: DET.CENTER-X ( ptr a -- ptr a ) 8 CELL-FIELD ;
: DET.CENTER-Y ( ptr a -- ptr a ) 9 CELL-FIELD ;
: DET.SOURCE-A ( ptr a -- ptr ptr u8 ) 10 ptr-field ;
: DET.SOURCE-N ( ptr a -- ptr a ) 11 CELL-FIELD ;
: DET.CONFIDENCE ( ptr a -- ptr a ) 12 CELL-FIELD ;
: DET.LATENCY ( ptr a -- ptr a ) 13 CELL-FIELD ;
: DET.QUEUE-PRESENT ( ptr a -- ptr a ) 14 CELL-FIELD ;
: DET.QUEUE-DEPTH ( ptr a -- ptr a ) 15 CELL-FIELD ;
: DET.DECISION-PRESENT ( ptr a -- ptr a ) 16 CELL-FIELD ;
: DET.DECISION-TS ( ptr a -- ptr a ) 17 CELL-FIELD ;
: DET.TRACKER-INDEX-PRESENT ( ptr a -- ptr a ) 18 CELL-FIELD ;
: DET.TRACKER-INDEX ( ptr a -- ptr a ) 19 CELL-FIELD ;
: DET.TRACKER-TS-PRESENT ( ptr a -- ptr a ) 20 CELL-FIELD ;
: DET.TRACKER-TS ( ptr a -- ptr a ) 21 CELL-FIELD ;

: PT.CAMERA-A ( ptr a -- ptr ptr u8 ) 0 ptr-field ;
: PT.CAMERA-N ( ptr a -- ptr a ) 1 CELL-FIELD ;
: PT.LOGICAL-A ( ptr a -- ptr ptr u8 ) 2 ptr-field ;
: PT.LOGICAL-N ( ptr a -- ptr a ) 3 CELL-FIELD ;
: PT.FRAME-INDEX ( ptr a -- ptr a ) 4 CELL-FIELD ;
: PT.SDK-TS ( ptr a -- ptr a ) 5 CELL-FIELD ;
: PT.SOURCE-A ( ptr a -- ptr ptr u8 ) 6 ptr-field ;
: PT.SOURCE-N ( ptr a -- ptr a ) 7 CELL-FIELD ;
: PT.INFERENCE-INDEX ( ptr a -- ptr a ) 8 CELL-FIELD ;
: PT.DETECTIONS-COUNT ( ptr a -- ptr a ) 9 CELL-FIELD ;
: PT.LATENCY ( ptr a -- ptr a ) 10 CELL-FIELD ;
: PT.QUEUE-PRESENT ( ptr a -- ptr a ) 11 CELL-FIELD ;
: PT.QUEUE-DEPTH ( ptr a -- ptr a ) 12 CELL-FIELD ;
: PT.DECISION-PRESENT ( ptr a -- ptr a ) 13 CELL-FIELD ;
: PT.DECISION-TS ( ptr a -- ptr a ) 14 CELL-FIELD ;
: PT.TRACKER-INDEX-PRESENT ( ptr a -- ptr a ) 15 CELL-FIELD ;
: PT.TRACKER-INDEX ( ptr a -- ptr a ) 16 CELL-FIELD ;
: PT.TRACKER-TS-PRESENT ( ptr a -- ptr a ) 17 CELL-FIELD ;
: PT.TRACKER-TS ( ptr a -- ptr a ) 18 CELL-FIELD ;
: PT.SCHEDULE-LAG-PRESENT ( ptr a -- ptr a ) 19 CELL-FIELD ;
: PT.SCHEDULE-LAG ( ptr a -- ptr a ) 20 CELL-FIELD ;
: PT.TENSOR-RETRIEVE-PRESENT ( ptr a -- ptr a ) 21 CELL-FIELD ;
: PT.TENSOR-RETRIEVE ( ptr a -- ptr a ) 22 CELL-FIELD ;
: PT.DETECTOR-RUN-PRESENT ( ptr a -- ptr a ) 23 CELL-FIELD ;
: PT.DETECTOR-RUN ( ptr a -- ptr a ) 24 CELL-FIELD ;
: PT.DETECTOR-CYCLE-PRESENT ( ptr a -- ptr a ) 25 CELL-FIELD ;
: PT.DETECTOR-CYCLE ( ptr a -- ptr a ) 26 CELL-FIELD ;
: PT.MODE-A ( ptr a -- ptr ptr u8 ) 27 ptr-field ;
: PT.MODE-N ( ptr a -- ptr a ) 28 CELL-FIELD ;

: TT.CAMERA-A ( ptr a -- ptr ptr u8 ) 0 ptr-field ;
: TT.CAMERA-N ( ptr a -- ptr a ) 1 CELL-FIELD ;
: TT.LOGICAL-A ( ptr a -- ptr ptr u8 ) 2 ptr-field ;
: TT.LOGICAL-N ( ptr a -- ptr a ) 3 CELL-FIELD ;
: TT.FRAME-INDEX ( ptr a -- ptr a ) 4 CELL-FIELD ;
: TT.SDK-TS ( ptr a -- ptr a ) 5 CELL-FIELD ;
: TT.SOURCE-A ( ptr a -- ptr ptr u8 ) 6 ptr-field ;
: TT.SOURCE-N ( ptr a -- ptr a ) 7 CELL-FIELD ;
: TT.UPDATE-INDEX ( ptr a -- ptr a ) 8 CELL-FIELD ;
: TT.TRACKER-TS ( ptr a -- ptr a ) 9 CELL-FIELD ;
: TT.LATENCY ( ptr a -- ptr a ) 10 CELL-FIELD ;
: TT.QUEUE-PRESENT ( ptr a -- ptr a ) 11 CELL-FIELD ;
: TT.QUEUE-DEPTH ( ptr a -- ptr a ) 12 CELL-FIELD ;
: TT.TRACKS-ACTIVE ( ptr a -- ptr a ) 13 CELL-FIELD ;

private

: JNODE ( i64 ptr u8 n -- i64 ) {: root:i64 key:ptr ku:n :}
   root key ku JSON-GET ;

: JSTR$ ( i64 ptr u8 n -- ptr u8 n )
   JNODE JSON-STRING$ ;

: JINT ( i64 ptr u8 n -- n )
   JNODE JSON-NUMBER$ STR>NUMBER? drop ;

: JFLOAT ( i64 ptr u8 n -- r )
   JNODE JSON-NUMBER$ STR>FLOAT drop ;

: JBOOL ( i64 ptr u8 n -- n )
   JNODE JSON-BOOL@ if 1 else 0 then ;

: OPT-INT! ( i64 ptr u8 n ptr a ptr a -- ) {: root:i64 key:ptr ku:n pp:ptr vp:ptr :}
   root key ku JSON-GET {: v:i64 :}
   v -1 = if 0 pp ! 0 vp ! exit then
   -1 pp !
   v JSON-NUMBER$ STR>NUMBER? drop vp ! ;

: OPT-FLOAT! ( i64 ptr u8 n ptr a ptr a -- ) {: root:i64 key:ptr ku:n pp:ptr vp:ptr :}
   root key ku JSON-GET {: v:i64 :}
   v -1 = if 0 pp ! 0.0 vp F! exit then
   -1 pp !
   v JSON-NUMBER$ STR>FLOAT drop vp F! ;

: FRAME-SERIAL! ( ptr u8 n ptr a -- ) {: a u rec :} \ typed-local-lint: allow-bare-local
   a rec FRAME.SERIAL-A !  u rec FRAME.SERIAL-N ! ;

: FRAME-LOGICAL! ( ptr u8 n ptr a -- ) {: a u rec :} \ typed-local-lint: allow-bare-local
   a rec FRAME.LOGICAL-A !  u rec FRAME.LOGICAL-N ! ;

: DET-CAMERA! ( ptr u8 n ptr a -- ) {: a u rec :} \ typed-local-lint: allow-bare-local
   a rec DET.CAMERA-A !  u rec DET.CAMERA-N ! ;

: DET-LOGICAL! ( ptr u8 n ptr a -- ) {: a u rec :} \ typed-local-lint: allow-bare-local
   a rec DET.LOGICAL-A !  u rec DET.LOGICAL-N ! ;

: DET-TARGET! ( ptr u8 n ptr a -- ) {: a u rec :} \ typed-local-lint: allow-bare-local
   a rec DET.TARGET-A !  u rec DET.TARGET-N ! ;

: DET-SOURCE! ( ptr u8 n ptr a -- ) {: a u rec :} \ typed-local-lint: allow-bare-local
   a rec DET.SOURCE-A !  u rec DET.SOURCE-N ! ;

: PT-CAMERA! ( ptr u8 n ptr a -- ) {: a u rec :} \ typed-local-lint: allow-bare-local
   a rec PT.CAMERA-A !  u rec PT.CAMERA-N ! ;

: PT-LOGICAL! ( ptr u8 n ptr a -- ) {: a u rec :} \ typed-local-lint: allow-bare-local
   a rec PT.LOGICAL-A !  u rec PT.LOGICAL-N ! ;

: PT-SOURCE! ( ptr u8 n ptr a -- ) {: a u rec :} \ typed-local-lint: allow-bare-local
   a rec PT.SOURCE-A !  u rec PT.SOURCE-N ! ;

: PT-MODE! ( i64 ptr a -- ) {: root:i64 rec:ptr :}
   root s" mode" JSON-GET {: v:i64 :}
   v -1 = if 0 rec PT.MODE-N ! exit then
   v JSON-STRING$ {: a u :} \ typed-local-lint: allow-bare-local
   a rec PT.MODE-A !  u rec PT.MODE-N ! ;

: TT-CAMERA! ( ptr u8 n ptr a -- ) {: a u rec :} \ typed-local-lint: allow-bare-local
   a rec TT.CAMERA-A !  u rec TT.CAMERA-N ! ;

: TT-LOGICAL! ( ptr u8 n ptr a -- ) {: a u rec :} \ typed-local-lint: allow-bare-local
   a rec TT.LOGICAL-A !  u rec TT.LOGICAL-N ! ;

: TT-SOURCE! ( ptr u8 n ptr a -- ) {: a u rec :} \ typed-local-lint: allow-bare-local
   a rec TT.SOURCE-A !  u rec TT.SOURCE-N ! ;

public

: FRAME-LOAD ( i64 ptr a -- ) {: root:i64 rec:ptr :}
   root s" serial"                 JSTR$ rec FRAME-SERIAL!
   root s" logical_name"           JSTR$ rec FRAME-LOGICAL!
   root s" fps_target"             JINT rec FRAME.FPS-TARGET !
   root s" frame_index"            JINT rec FRAME.FRAME-INDEX !
   root s" sdk_image_timestamp_ns" JINT rec FRAME.SDK-TS !
   root s" host_monotonic_ns"      JINT rec FRAME.HOST-NS !
   root s" dropped"                JBOOL rec FRAME.DROPPED !
   root s" duplicate"              JBOOL rec FRAME.DUPLICATE !
   root s" timestamp_regressed"    JBOOL rec FRAME.REGRESSED ! ;

: DETECTION-LOAD ( i64 ptr a -- ) {: root:i64 rec:ptr :}
   root s" camera_serial"          JSTR$ rec DET-CAMERA!
   root s" logical_name"           JSTR$ rec DET-LOGICAL!
   root s" frame_index"            JINT rec DET.FRAME-INDEX !
   root s" sdk_image_timestamp_ns" JINT rec DET.SDK-TS !
   root s" target_id"              JSTR$ rec DET-TARGET!
   root s" pixel_center_x"         JFLOAT rec DET.CENTER-X F!
   root s" pixel_center_y"         JFLOAT rec DET.CENTER-Y F!
   root s" detection_source"       JSTR$ rec DET-SOURCE!
   root s" confidence"             JFLOAT rec DET.CONFIDENCE F!
   root s" latency_ms"             JFLOAT rec DET.LATENCY F!
   root s" queue_depth"            rec DET.QUEUE-PRESENT rec DET.QUEUE-DEPTH OPT-INT!
   root s" decision_timestamp_ns"  rec DET.DECISION-PRESENT rec DET.DECISION-TS OPT-INT!
   root s" tracker_update_index"   rec DET.TRACKER-INDEX-PRESENT rec DET.TRACKER-INDEX OPT-INT!
   root s" tracker_timestamp_ns"   rec DET.TRACKER-TS-PRESENT rec DET.TRACKER-TS OPT-INT! ;

: PERCEPTION-TICK-LOAD ( i64 ptr a -- ) {: root:i64 rec:ptr :}
   root s" camera_serial"          JSTR$ rec PT-CAMERA!
   root s" logical_name"           JSTR$ rec PT-LOGICAL!
   root s" frame_index"            JINT rec PT.FRAME-INDEX !
   root s" sdk_image_timestamp_ns" JINT rec PT.SDK-TS !
   root s" tick_source"            JSTR$ rec PT-SOURCE!
   root s" inference_index"        JINT rec PT.INFERENCE-INDEX !
   root s" detections_count"       JINT rec PT.DETECTIONS-COUNT !
   root s" latency_ms"             JFLOAT rec PT.LATENCY F!
   root s" queue_depth"            rec PT.QUEUE-PRESENT rec PT.QUEUE-DEPTH OPT-INT!
   root s" decision_timestamp_ns"  rec PT.DECISION-PRESENT rec PT.DECISION-TS OPT-INT!
   root s" tracker_update_index"   rec PT.TRACKER-INDEX-PRESENT rec PT.TRACKER-INDEX OPT-INT!
   root s" tracker_timestamp_ns"   rec PT.TRACKER-TS-PRESENT rec PT.TRACKER-TS OPT-INT!
   root s" schedule_lag_ms"        rec PT.SCHEDULE-LAG-PRESENT rec PT.SCHEDULE-LAG OPT-FLOAT!
   root s" tensor_retrieve_ms"     rec PT.TENSOR-RETRIEVE-PRESENT rec PT.TENSOR-RETRIEVE OPT-FLOAT!
   root s" detector_run_ms"        rec PT.DETECTOR-RUN-PRESENT rec PT.DETECTOR-RUN OPT-FLOAT!
   root s" detector_cycle_ms"      rec PT.DETECTOR-CYCLE-PRESENT rec PT.DETECTOR-CYCLE OPT-FLOAT!
   root rec PT-MODE! ;

: TRACKER-TICK-LOAD ( i64 ptr a -- ) {: root:i64 rec:ptr :}
   root s" camera_serial"          JSTR$ rec TT-CAMERA!
   root s" logical_name"           JSTR$ rec TT-LOGICAL!
   root s" frame_index"            JINT rec TT.FRAME-INDEX !
   root s" sdk_image_timestamp_ns" JINT rec TT.SDK-TS !
   root s" tracker_source"         JSTR$ rec TT-SOURCE!
   root s" tracker_update_index"   JINT rec TT.UPDATE-INDEX !
   root s" tracker_timestamp_ns"   JINT rec TT.TRACKER-TS !
   root s" latency_ms"             JFLOAT rec TT.LATENCY F!
   root s" queue_depth"            rec TT.QUEUE-PRESENT rec TT.QUEUE-DEPTH OPT-INT!
   root s" tracks_active"          JINT rec TT.TRACKS-ACTIVE ! ;

end-package
