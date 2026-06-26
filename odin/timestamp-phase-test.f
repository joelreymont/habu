\ timestamp-phase-test.f - oracle from src/timestamp_metrics.zig test 1700
\ "snap: sync phase offsets identify one-frame camera phase" (phase_offset_sample_ndjson).
\ Expected per-camera phase rows (from the CSV oracle):
\   cam_a0 (self): offset 0, matched 3, skews 0, same_index_p99 0
\   cam_a1:        offset -1, matched 2, mean/p95/p99/max 333, same_index_p99 16667000
\ Run: cat lib/errors.f lib/string.f lib/hashmap.f lib/float.f lib/sort.f lib/test.f \
\        odin/timestamp-metrics.f odin/timestamp-phase.f odin/timestamp-phase-test.f | bin/hb

package CAMSYNC
private
: PA+ ( n n n n -- ) {: fps:n fidx:n sdk:n host:n :}        \ cam_a0 -> camera index 0
   s" 306885122" s" cam_a0" fps fidx sdk host 0 0 0 TM-ADD
   0 fidx sdk FS-ADD ;
: PB+ ( n n n n -- ) {: fps:n fidx:n sdk:n host:n :}        \ cam_a1 -> camera index 1
   s" 309091258" s" cam_a1" fps fidx sdk host 0 0 0 TM-ADD
   1 fidx sdk FS-ADD ;

: PHO-RUN ( -- )
   T-RESET  TM-RESET  FS-RESET
   60 0 1000000000 2000000000 PA+
   60 1 1016666667 2016666667 PA+
   60 2 1033333334 2033333334 PA+
   60 0 1016667000 2016667000 PB+
   60 1 1033333667 2033333667 PB+
   60 2 1050000334 2050000334 PB+
   PHO-BUILD
   \ cam_a0 vs itself: zero skew at offset 0
   0 0 PHO-OFF@ T=   3 0 PHO-MATCH@ T=
   0 0 PHO-MEAN@ T=  0 0 PHO-P95@ T=  0 0 PHO-P99@ T=  0 0 PHO-MAX@ T=  0 0 PHO-SAME99@ T=
   \ cam_a1 is one frame ahead: best offset -1, tiny 333ns skew; same-index p99 ~16.67ms
   -1 1 PHO-OFF@ T=  2 1 PHO-MATCH@ T=
   333 1 PHO-MEAN@ T=  333 1 PHO-P95@ T=  333 1 PHO-P99@ T=  333 1 PHO-MAX@ T=
   16667000 1 PHO-SAME99@ T= ;

\ timestamp pairing oracle (test 1717, timestamp_pairing_sample_ndjson):
\   cam_a0 self: ref 3, matched 3, dup 0, skews 0
\   cam_a0 vs cam_b0: ref 3, matched 3, dup 0, mean/p95/p99/max 333
: QB+ ( n n n n -- ) {: fps:n fidx:n sdk:n host:n :}        \ cam_b0 -> camera index 1
   s" 302484649" s" cam_b0" fps fidx sdk host 0 0 0 TM-ADD
   1 fidx sdk FS-ADD ;
: TP-RUN ( -- )
   T-RESET  TM-RESET  FS-RESET
   60 0 1000000000 2000000000 PA+
   60 1 1016666667 2016666667 PA+
   60 2 1033333334 2033333334 PA+
   60 1 1000000333 2000000333 QB+
   60 2 1016667000 2016667000 QB+
   60 3 1033333667 2033333667 QB+
   TP-BUILD
   3 0 TP-PREF@ T=  3 0 TP-PMATCH@ T=  0 0 TP-PDUP@ T=
   0 0 TP-PMEAN@ T=  0 0 TP-PP95@ T=  0 0 TP-PP99@ T=  0 0 TP-PMAX@ T=
   3 1 TP-PREF@ T=  3 1 TP-PMATCH@ T=  0 1 TP-PDUP@ T=
   333 1 TP-PMEAN@ T=  333 1 TP-PP95@ T=  333 1 TP-PP99@ T=  333 1 TP-PMAX@ T= ;

\ timestamp frame pairs oracle (test 1734): 6 rows (cam_a0 self x3, cam_a0->cam_b0 x3)
: FP-RUN ( -- )
   T-RESET  TM-RESET  FS-RESET
   60 0 1000000000 2000000000 PA+
   60 1 1016666667 2016666667 PA+
   60 2 1033333334 2033333334 PA+
   60 1 1000000333 2000000333 QB+
   60 2 1016667000 2016667000 QB+
   60 3 1033333667 2033333667 QB+
   FP-BUILD
   6 FP-N@ T=
   \ row 0: cam_a0 f0 -> cam_a0 f0, skew 0
   0 0 FP-REFFIDX@ T=  1000000000 0 FP-REFSDK@ T=  0 0 FP-CAMIDX@ T=
   0 0 FP-MFIDX@ T=  1000000000 0 FP-MSDK@ T=  0 0 FP-SKEW@ T=  0 0 FP-DUP@ T=
   \ row 3: cam_a0 f0 -> cam_b0 f1, skew 333
   0 3 FP-REFFIDX@ T=  1 3 FP-CAMIDX@ T=  1 3 FP-MFIDX@ T=
   1000000333 3 FP-MSDK@ T=  333 3 FP-SKEW@ T=  0 3 FP-DUP@ T=
   \ row 5: cam_a0 f2 -> cam_b0 f3, skew 333
   2 5 FP-REFFIDX@ T=  1 5 FP-CAMIDX@ T=  3 5 FP-MFIDX@ T=
   1033333667 5 FP-MSDK@ T=  333 5 FP-SKEW@ T= ;

\ readiness gates (tests 1928 sync, 1755 pairing)
: SA+ ( n n n n n -- ) {: fps:n fidx:n sdk:n host:n drp:n :}
   s" 306885122" s" cam_a0" fps fidx sdk host drp 0 0 TM-ADD  fidx sdk host TG-ADD ;
: SB+ ( n n n n n -- ) {: fps:n fidx:n sdk:n host:n drp:n :}
   s" 309091258" s" cam_a1" fps fidx sdk host drp 0 0 TM-ADD  fidx sdk host TG-ADD ;
: SR-PASS ( -- )   \ clean multi capture -> pass
   TM-RESET TG-RESET TX-RESET  s" capture-null-multi" TX-SCHEMA
   60 0 1000000000 2000000000 0 SA+  60 1 1016666667 2016666667 0 SA+  60 2 1033333334 2033333334 0 SA+
   60 0 1000000100 2000000100 0 SB+  60 1 1016666767 2016666767 0 SB+  60 2 1033333434 2033333434 0 SB+
   TM-FINISH 2 TX-BUILD SR-BUILD ;
: SR-FAIL ( -- )   \ sample_ndjson: incomplete sets, 2e9 skew, 4 drops -> fail
   TM-RESET TG-RESET TX-RESET  s" capture-null-multi" TX-SCHEMA
   60 0 1000000000 2000000000 0 SA+  60 1 1016666667 2016666667 0 SA+  60 2 1033333334 2033333334 0 SA+
   60 0 3000000000 4000000000 0 SB+  60 2 3033333334 4033333334 1 SB+
   s" 309091258" 4 TM-SUMMARY  TM-FINISH 2 TX-BUILD SR-BUILD ;
: TA+ ( n n n n -- ) {: fps:n fidx:n sdk:n host:n :}
   s" 306885122" s" cam_a0" fps fidx sdk host 0 0 0 TM-ADD  0 fidx sdk FS-ADD ;
: TB+ ( n n n n -- ) {: fps:n fidx:n sdk:n host:n :}
   s" 302484649" s" cam_b0" fps fidx sdk host 0 0 0 TM-ADD  1 fidx sdk FS-ADD ;
: TPR-PASS ( -- )  \ pairing fixture: all matched, tiny skew -> pass
   TM-RESET TG-RESET TX-RESET FS-RESET  s" capture-save-multi" TX-SCHEMA
   60 0 1000000000 2000000000 TA+  60 1 1016666667 2016666667 TA+  60 2 1033333334 2033333334 TA+
   60 1 1000000333 2000000333 TB+  60 2 1016667000 2016667000 TB+  60 3 1033333667 2033333667 TB+
   TM-FINISH 2 TX-BUILD TP-BUILD TPR-BUILD ;
: RD-RUN ( -- )
   T-RESET
   SR-PASS  SR-RESULT@ TTRUE
   SR-FAIL  SR-RESULT@ TFALSE
   TPR-PASS TPR-RESULT@ TTRUE ;

PHO-RUN
TP-RUN
FP-RUN
RD-RUN
T-REPORT
end-package
