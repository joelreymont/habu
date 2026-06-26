\ tracker-render.f - tracking summary renderers, ported from renderSummaryMarkdown
\ + renderMetricsCsv + renderCameraCsv in src/tracker.zig. Reads the tracker.f
\ kernel aggregates (after TK-LEN-STATS) and emits into the lib/render.f buffer.
\ Float metrics use {d:.4} (RB-RATIO4 from integer sum/count, round-half-up); the
\ test values are exact. records = TK-RECORDS@, frames = TK-FRAMES@, cameras = 1
\ for a single-camera stream (this kernel processes one camera; the Zig groups by
\ camera). result() is "ready" under default options (no thresholds set), matching
\ ready(). Depends on lib/errors.f lib/string.f lib/float.f lib/sort.f
\ odin/float-cell.f lib/render.f odin/tracker.f.

package TRACKER
private
: TR-CAMS ( -- n ) TK-RECORDS@ 0 > if 1 else 0 then ;       \ single-camera stream

\ renderSummaryMarkdown (test 712) via the MD-* bullet DSL
public
: TR-MD ( -- ptr u8 n )
   RB-RESET
   s" # Tracking Association Summary" RB+ RB-NL  RB-NL
   s" records" TK-RECORDS@ MD-N
   s" cameras" TR-CAMS MD-N
   s" frames" TK-FRAMES@ MD-N
   s" tracks created" TK-CREATED@ MD-N
   s" tracks confirmed" TK-CONFCNT@ MD-N
   s" association rate" TK-MATCHED@  TK-MATCHED@ TK-NEW@ +  MD-R
   s" mean track length" TK-LSUM@ TK-LCNT@ MD-R
   s" median track length" TK-LMED@ 1 MD-R
   s" max track length" TK-LMAX@ MD-N
   s" mean hits per track" TK-THITS@ TK-LCNT@ MD-R
   s" result" s" ready" MD-S
   RB$ ;

\ renderMetricsCsv (metric,value rows) via the CV* vertical-CSV DSL
: TR-MCSV ( -- ptr u8 n )
   RB-RESET
   s" metric,value" RB+ RB-NL
   s" records" TK-RECORDS@ CVN
   s" cameras" TR-CAMS CVN
   s" frames" TK-FRAMES@ CVN
   s" tracks_created" TK-CREATED@ CVN
   s" tracks_confirmed" TK-CONFCNT@ CVN
   s" detections_matched" TK-MATCHED@ CVN
   s" detections_new" TK-NEW@ CVN
   s" association_rate" TK-MATCHED@  TK-MATCHED@ TK-NEW@ +  CVR
   s" mean_track_length" TK-LSUM@ TK-LCNT@ CVR
   s" median_track_length" TK-LMED@ 1 CVR
   s" max_track_length" TK-LMAX@ CVN
   s" mean_hits_per_track" TK-THITS@ TK-LCNT@ CVR
   RB$ ;

\ renderCameraCsv via the report engine; camera identity (kernel is geometry-only)
\ is held in vars so the cell quotations can read it.
variable TC-SER-A  variable TC-SER-N  variable TC-LNA-A  variable TC-LNA-N
private
: TR-CC-COLS ( -- ) TBL-RESET
   s" logical_name" AL-L [: drop TC-LNA-A @ TC-LNA-N @ RB+ ;] COL+
   s" serial" AL-L [: drop TC-SER-A @ TC-SER-N @ RB+ ;] COL+
   s" detections" AL-L [: drop TK-RECORDS@ RB# ;] COL+
   s" frames" AL-L [: drop TK-FRAMES@ RB# ;] COL+
   s" tracks_created" AL-L [: drop TK-CREATED@ RB# ;] COL+
   s" tracks_confirmed" AL-L [: drop TK-CONFCNT@ RB# ;] COL+
   s" detections_matched" AL-L [: drop TK-MATCHED@ RB# ;] COL+
   s" mean_track_length" AL-L [: drop TK-LSUM@ TK-LCNT@ RB-RATIO4 ;] COL+
   s" max_track_length" AL-L [: drop TK-LMAX@ RB# ;] COL+ ;
public
: TR-CCSV ( ptr u8 n ptr u8 n -- ptr u8 n ) {: sa:ptr sn:n la:ptr ln:n :}
   sa TC-SER-A !  sn TC-SER-N !  la TC-LNA-A !  ln TC-LNA-N !
   TR-CC-COLS  RB-RESET  1 TBL-CSV  RB$ ;
end-package
