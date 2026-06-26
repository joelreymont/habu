\ tracker-render-emit.f - emits the tracking summary markdown + metrics/camera CSVs
\ to stdout for verification against src/tracker.zig test-712 (markdown ohsnap) and
\ the renderMetricsCsv/renderCameraCsv formulas. Feeds the exact sample_ndjson
\ fixture (same four detections as tracker-test.f).

package TRACKER
private
: FEED ( -- )
   TK-RESET
   64.0 0.0 3 5 TK-SETUP
   100.0 100.0  90.0 90.0 20.0 20.0 1 FD-ADD   0 TK-FRAME
   105.0 102.0  95.0 92.0 20.0 20.0 1 FD-ADD   1 TK-FRAME
   110.0 104.0  100.0 94.0 20.0 20.0 1 FD-ADD
   400.0 400.0  390.0 390.0 20.0 20.0 1 FD-ADD  2 TK-FRAME
   TK-LEN-STATS ;

: MARK ( ptr u8 n -- ) type 10 emit ;

FEED
s" <<<MD>>>"   MARK   TR-MD  type
s" <<<MCSV>>>" MARK   TR-MCSV type
s" <<<CCSV>>>" MARK   s" 1" s" cam_a0" TR-CCSV type
s" <<<END>>>"  MARK
end-package
