\ timestamp-metrics-test.f - per-camera kernel oracle, from src/timestamp_metrics.zig
\ test "timestamp metrics collate frame periods and gaps by camera" (sample_ndjson).
\ Frames are fed as the exact field values from that fixture.
\ Run: ../habu/bin/hb --load odin/timestamp-metrics-test.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/float.f
require lib/sort.f
require lib/hashmap.f
require lib/test.f
require odin/timestamp-metrics.f

\ ( fps frame_index sdk_ts host_ts dropped dup regressed ) for serial A / B.
\ Each frame folds into both the per-camera kernel and the frame-index groups.
package CAMSYNC
private
: A+ ( n n n n n n n -- ) {: fps:n fidx:n sdk:n host:n drp:n dupf:n regr:n :}
   s" 306885122" s" cam_a0" fps fidx sdk host drp dupf regr TM-ADD
   fidx sdk host TG-ADD ;
: B+ ( n n n n n n n -- ) {: fps:n fidx:n sdk:n host:n drp:n dupf:n regr:n :}
   s" 309091258" s" cam_a1" fps fidx sdk host drp dupf regr TM-ADD
   fidx sdk host TG-ADD ;

: TM-RUN ( -- )
   T-RESET
   TM-RESET  TX-RESET
   s" capture-null-multi" TX-SCHEMA          \ the one schema record (multi command)
   \ cam_a0 (306885122): three monotonic 60fps frames at ~16.6667ms period
   60 0 1000000000 2000000000 0 0 0 A+
   60 1 1016666667 2016666667 0 0 0 A+
   60 2 1033333334 2033333334 0 0 0 A+
   \ cam_a1 (309091258): frame 0 then frame 2 (index gap of 1), frame 2 dropped
   60 0 3000000000 4000000000 0 0 0 B+
   60 2 3033333334 4033333334 1 0 0 B+
   \ summary for cam_a1: frames_dropped = 4
   s" 309091258" 4 TM-SUMMARY
   TM-FINISH
   2 TX-BUILD                     \ cross-camera skew over 2 cameras

   2 TM-COUNT T=                  \ two cameras
   3 0 TM-FRAMES@ T=              \ cam0 frames
   2 0 TM-PSAMP@ T=               \ cam0 period samples
   16666667 0 TM-SDK-MEAN@ T=     \ cam0 sdk period mean
   16666667 0 TM-HST-MEAN@ T=     \ cam0 host period mean
   16666667 0 TM-SDK-MIN@ T=      \ both deltas equal -> min == mean
   16666667 0 TM-SDK-MAX@ T=
   1 0 TM-SDK-JIT@ T=             \ |16666667 - 16666666(target)| = 1
   2 1 TM-FRAMES@ T=              \ cam1 frames
   1 1 TM-GAP@ T=                 \ cam1 index_gap_drops
   1 1 TM-MAXGAP@ T=              \ cam1 max_index_gap_drop
   1 1 TM-DROP@ T=                \ cam1 dropped_event_flags
   4 1 TM-FDROP@ T=               \ cam1 frames_dropped (from summary)
   \ cross-camera skew (index sets {0,1,2}; 0 and 2 complete, 1 incomplete)
   3 TX-FSETS@ T=
   2 TX-COMPLETE@ T=
   1 TX-INCOMPLETE@ T=
   TX-COMMON@ TTRUE
   2000000000 TX-SMAX@ T=         \ |3e9 - 1e9| at the complete index sets
   2000000000 TX-SMIN@ T=
   2000000000 TX-SMEAN@ T=
   2000000000 TX-SP95@ T=         \ skews [2e9,2e9] -> nearest-rank p95/p99 = 2e9
   2000000000 TX-SP99@ T= ;

\ sequential helper streams (two non-multi schema records) -> no common lifecycle,
\ so cross skew is suppressed even though the index-0 set is complete (test 1688).
: TM-RUN2 ( -- )
   TM-RESET  TX-RESET
   s" capture-null" TX-SCHEMA
   60 0 1000000000 2000000000 0 0 0 A+
   s" capture-null" TX-SCHEMA
   60 0 3000000000 4000000000 0 0 0 B+
   TM-FINISH
   2 TX-BUILD
   TX-COMMON@ TFALSE
   0 TX-MULTI@ T=
   2 TX-NONMULTI@ T=
   1 TX-COMPLETE@ T=
   0 TX-SMAX@ T=
   0 TX-HMAX@ T= ;

TM-RUN
TM-RUN2
T-REPORT
end-package
