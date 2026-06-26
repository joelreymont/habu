\ timestamp-render.f - byte-exact renderers for the timestamp-sync report, ported
\ from renderFrameSyncCsv/Json + renderCrossCameraSyncCsv/Json in
\ src/timestamp_metrics.zig. Reads the timestamp-metrics.f kernel (TM-*/TX-*) and
\ appends into the lib/render.f buffer (RB-*). The Zig prints with std.json at
\ 2-space indent and emits the i128 host_minus_sdk_* fields as JSON strings; both
\ are reproduced here exactly. Cameras render in first-seen order (the Zig does not
\ sort them). Depends on lib/errors.f lib/string.f lib/render.f odin/timestamp-metrics.f.

\ CSV/JSON formatting helpers (QT/CM/QSTR/QK/SPACES/RB-BOOL/KVN/KVS/KVB) live in
\ lib/render.f, shared across all analyzer renderers.

\ ===========================================================================
\ renderFrameSyncCsv
\ ===========================================================================
\ renderFrameSyncCsv: 28 columns declared once, row = camera index (the cell
\ quotations take the row index and read the TM-* accessors).
package CAMSYNC
private
: TS-CSV-COLS ( -- ) TBL-RESET
   s" serial" AL-L [: TM-SER@ RB+ ;] COL+
   s" logical_name" AL-L [: TM-LNA@ RB+ ;] COL+
   s" frames" AL-L [: TM-FRAMES@ RB# ;] COL+
   s" period_samples" AL-L [: TM-PSAMP@ RB# ;] COL+
   s" fps_target" AL-L [: TM-FPS@ RB# ;] COL+
   s" target_period_ns" AL-L [: TM-TGTP@ RB# ;] COL+
   s" sdk_period_mean_ns" AL-L [: TM-SDK-MEAN@ RB# ;] COL+
   s" sdk_period_min_ns" AL-L [: TM-SDK-MIN@ RB# ;] COL+
   s" sdk_period_max_ns" AL-L [: TM-SDK-MAX@ RB# ;] COL+
   s" sdk_jitter_max_abs_ns" AL-L [: TM-SDK-JIT@ RB# ;] COL+
   s" host_period_mean_ns" AL-L [: TM-HST-MEAN@ RB# ;] COL+
   s" host_period_min_ns" AL-L [: TM-HST-MIN@ RB# ;] COL+
   s" host_period_max_ns" AL-L [: TM-HST-MAX@ RB# ;] COL+
   s" host_jitter_max_abs_ns" AL-L [: TM-HST-JIT@ RB# ;] COL+
   s" dropped_event_flags" AL-L [: TM-DROP@ RB# ;] COL+
   s" frames_dropped" AL-L [: TM-FDROP@ RB# ;] COL+
   s" duplicate_flags" AL-L [: TM-DUP@ RB# ;] COL+
   s" timestamp_regression_flags" AL-L [: TM-REGR@ RB# ;] COL+
   s" index_gap_drops" AL-L [: TM-GAP@ RB# ;] COL+
   s" max_index_gap_drop" AL-L [: TM-MAXGAP@ RB# ;] COL+
   s" first_frame_index" AL-L [: TM-FIDX0@ RB# ;] COL+
   s" last_frame_index" AL-L [: TM-LIDX@ RB# ;] COL+
   s" first_sdk_timestamp_ns" AL-L [: TM-FSDK@ RB# ;] COL+
   s" last_sdk_timestamp_ns" AL-L [: TM-LSDK@ RB# ;] COL+
   s" first_host_monotonic_ns" AL-L [: TM-FHST@ RB# ;] COL+
   s" last_host_monotonic_ns" AL-L [: TM-LHST@ RB# ;] COL+
   s" host_minus_sdk_first_ns" AL-L [: TM-HMSDK-F@ RB# ;] COL+
   s" host_minus_sdk_last_ns" AL-L [: TM-HMSDK-L@ RB# ;] COL+ ;
public
: TS-CSV ( -- ptr u8 n ) TS-CSV-COLS  RB-RESET  TM-COUNT TBL-CSV  RB$ ;

\ ===========================================================================
\ renderCrossCameraSyncCsv
\ ===========================================================================
\ renderCrossCameraSyncCsv: 19 columns, single row (cell quotations drop the row index)
private
: TX-CSV-COLS ( -- ) TBL-RESET
   s" common_lifecycle" AL-L [: drop TX-COMMON@ RB-BOOL ;] COL+
   s" multi_schema_records" AL-L [: drop TX-MULTI@ RB# ;] COL+
   s" non_multi_schema_records" AL-L [: drop TX-NONMULTI@ RB# ;] COL+
   s" camera_count" AL-L [: drop TX-CAMCOUNT@ RB# ;] COL+
   s" frame_index_sets" AL-L [: drop TX-FSETS@ RB# ;] COL+
   s" complete_frame_index_sets" AL-L [: drop TX-COMPLETE@ RB# ;] COL+
   s" incomplete_frame_index_sets" AL-L [: drop TX-INCOMPLETE@ RB# ;] COL+
   s" min_cameras_per_set" AL-L [: drop TX-MINCAM@ RB# ;] COL+
   s" max_cameras_per_set" AL-L [: drop TX-MAXCAM@ RB# ;] COL+
   s" sdk_skew_mean_ns" AL-L [: drop TX-SMEAN@ RB# ;] COL+
   s" sdk_skew_min_ns" AL-L [: drop TX-SMIN@ RB# ;] COL+
   s" sdk_skew_p95_ns" AL-L [: drop TX-SP95@ RB# ;] COL+
   s" sdk_skew_p99_ns" AL-L [: drop TX-SP99@ RB# ;] COL+
   s" sdk_skew_max_ns" AL-L [: drop TX-SMAX@ RB# ;] COL+
   s" host_skew_mean_ns" AL-L [: drop TX-HMEAN@ RB# ;] COL+
   s" host_skew_min_ns" AL-L [: drop TX-HMIN@ RB# ;] COL+
   s" host_skew_p95_ns" AL-L [: drop TX-HP95@ RB# ;] COL+
   s" host_skew_p99_ns" AL-L [: drop TX-HP99@ RB# ;] COL+
   s" host_skew_max_ns" AL-L [: drop TX-HMAX@ RB# ;] COL+ ;
public
: TX-CSV ( -- ptr u8 n ) TX-CSV-COLS  RB-RESET  1 TBL-CSV  RB$ ;

\ ===========================================================================
\ cross_camera JSON object body (19 fields at indent 4), shared by both JSON renderers
\ ===========================================================================
private
: TX-OBJ ( -- )
   TX-COMMON@ 4 s" common_lifecycle" KVB CM RB-NL
   TX-MULTI@ 4 s" multi_schema_records" KVN CM RB-NL
   TX-NONMULTI@ 4 s" non_multi_schema_records" KVN CM RB-NL
   TX-CAMCOUNT@ 4 s" camera_count" KVN CM RB-NL
   TX-FSETS@ 4 s" frame_index_sets" KVN CM RB-NL
   TX-COMPLETE@ 4 s" complete_frame_index_sets" KVN CM RB-NL
   TX-INCOMPLETE@ 4 s" incomplete_frame_index_sets" KVN CM RB-NL
   TX-MINCAM@ 4 s" min_cameras_per_set" KVN CM RB-NL
   TX-MAXCAM@ 4 s" max_cameras_per_set" KVN CM RB-NL
   TX-SMEAN@ 4 s" sdk_skew_mean_ns" KVN CM RB-NL
   TX-SMIN@ 4 s" sdk_skew_min_ns" KVN CM RB-NL
   TX-SP95@ 4 s" sdk_skew_p95_ns" KVN CM RB-NL
   TX-SP99@ 4 s" sdk_skew_p99_ns" KVN CM RB-NL
   TX-SMAX@ 4 s" sdk_skew_max_ns" KVN CM RB-NL
   TX-HMEAN@ 4 s" host_skew_mean_ns" KVN CM RB-NL
   TX-HMIN@ 4 s" host_skew_min_ns" KVN CM RB-NL
   TX-HP95@ 4 s" host_skew_p95_ns" KVN CM RB-NL
   TX-HP99@ 4 s" host_skew_p99_ns" KVN CM RB-NL
   TX-HMAX@ 4 s" host_skew_max_ns" KVN RB-NL ;

\ ===========================================================================
\ renderFrameSyncJson - one camera object (indent 4 braces, indent 6 fields)
\ ===========================================================================
: TS-CAM ( n bool -- ) {: ix:n last:bool :}
   4 SPACES 123 RB-C RB-NL
   ix TM-SER@ 6 s" serial" KVS CM RB-NL
   ix TM-LNA@ 6 s" logical_name" KVS CM RB-NL
   ix TM-FRAMES@ 6 s" frames" KVN CM RB-NL
   ix TM-PSAMP@ 6 s" period_samples" KVN CM RB-NL
   ix TM-FPS@ 6 s" fps_target" KVN CM RB-NL
   ix TM-TGTP@ 6 s" target_period_ns" KVN CM RB-NL
   ix TM-SDK-MEAN@ 6 s" sdk_period_mean_ns" KVN CM RB-NL
   ix TM-SDK-MIN@ 6 s" sdk_period_min_ns" KVN CM RB-NL
   ix TM-SDK-MAX@ 6 s" sdk_period_max_ns" KVN CM RB-NL
   ix TM-SDK-JIT@ 6 s" sdk_jitter_max_abs_ns" KVN CM RB-NL
   ix TM-HST-MEAN@ 6 s" host_period_mean_ns" KVN CM RB-NL
   ix TM-HST-MIN@ 6 s" host_period_min_ns" KVN CM RB-NL
   ix TM-HST-MAX@ 6 s" host_period_max_ns" KVN CM RB-NL
   ix TM-HST-JIT@ 6 s" host_jitter_max_abs_ns" KVN CM RB-NL
   ix TM-DROP@ 6 s" dropped_event_flags" KVN CM RB-NL
   ix TM-FDROP@ 6 s" frames_dropped" KVN CM RB-NL
   ix TM-DUP@ 6 s" duplicate_flags" KVN CM RB-NL
   ix TM-REGR@ 6 s" timestamp_regression_flags" KVN CM RB-NL
   ix TM-GAP@ 6 s" index_gap_drops" KVN CM RB-NL
   ix TM-MAXGAP@ 6 s" max_index_gap_drop" KVN CM RB-NL
   ix TM-FIDX0@ 6 s" first_frame_index" KVN CM RB-NL
   ix TM-LIDX@ 6 s" last_frame_index" KVN CM RB-NL
   ix TM-FSDK@ 6 s" first_sdk_timestamp_ns" KVN CM RB-NL
   ix TM-LSDK@ 6 s" last_sdk_timestamp_ns" KVN CM RB-NL
   ix TM-FHST@ 6 s" first_host_monotonic_ns" KVN CM RB-NL
   ix TM-LHST@ 6 s" last_host_monotonic_ns" KVN CM RB-NL
   ix TM-HMSDK-F@ 6 s" host_minus_sdk_first_ns" KVN CM RB-NL
   ix TM-HMSDK-L@ 6 s" host_minus_sdk_last_ns" KVN RB-NL
   4 SPACES 125 RB-C  last if RB-NL else CM RB-NL then ;

public
: TS-JSON ( -- ptr u8 n )
   RB-RESET
   123 RB-C RB-NL
   s" odin.capture.v1" 2 s" schema_version" KVS CM RB-NL
   TM-FRECS@ 2 s" frame_records" KVN CM RB-NL
   2 SPACES s" cameras" QK 91 RB-C RB-NL
   0 TM-SI ! begin TM-SI @ TM-COUNT < while
      TM-SI @  TM-SI @ 1+ TM-COUNT =  TS-CAM
      TM-SI @ 1+ TM-SI !
   repeat
   2 SPACES 93 RB-C CM RB-NL
   2 SPACES s" cross_camera" QK 123 RB-C RB-NL
   TX-OBJ
   2 SPACES 125 RB-C RB-NL
   125 RB-C RB-NL
   RB$ ;

\ ===========================================================================
\ renderCrossCameraSyncJson
\ ===========================================================================
: TX-JSON ( -- ptr u8 n )
   RB-RESET
   123 RB-C RB-NL
   s" odin.capture.v1" 2 s" schema_version" KVS CM RB-NL
   2 SPACES s" cross_camera" QK 123 RB-C RB-NL
   TX-OBJ
   2 SPACES 125 RB-C RB-NL
   125 RB-C RB-NL
   RB$ ;
end-package
