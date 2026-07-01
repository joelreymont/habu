\ tracker-test.f - oracle from src/tracker.zig "iou and distance".
\ Run: ../habu/bin/hb --load odin/tracker-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/float.f
require lib/sort.f
require odin/float-cell.f
require odin/tracker.f

package TRACKER
private
: FL-NEAR ( r r -- bool ) f- fabs 0.000000001 f< ;
: T-NEAR ( r r -- ) FL-NEAR T-ASSERT ;

: TRK-RUN ( -- )
   T-RESET
   \ boxes (0,0,10,10) and (5,0,10,10): intersection 5x10=50, union 150 -> 1/3
   0.0 0.0 10.0 10.0  5.0 0.0 10.0 10.0 TRK-IOU   1.0 3.0 f/ T-NEAR
   \ disjoint boxes -> 0
   0.0 0.0 10.0 10.0  100.0 100.0 10.0 10.0 TRK-IOU  0.0 T-NEAR
   \ distance (0,0)-(3,4) = 5
   0.0 0.0 3.0 4.0 TRK-DIST  5.0 T-NEAR ;

\ "single moving target keeps one stable confirmed track" (confirm_hits=3, dist=64):
\ 3 close detections are one track (confirmed at hit 3); the far one spawns a second.
: TKA-RUN ( -- )
   TK-RESET
   64.0 0.0 3 5 TK-SETUP
   \ frame 0: det center (100,100)
   100.0 100.0  90.0 90.0 20.0 20.0 1 FD-ADD   0 TK-FRAME
   \ frame 1: det center (105,102) -> matches track
   105.0 102.0  95.0 92.0 20.0 20.0 1 FD-ADD   1 TK-FRAME
   \ frame 2: det (110,104) matches + far det (400,400) spawns a new track
   110.0 104.0  100.0 94.0 20.0 20.0 1 FD-ADD
   400.0 400.0  390.0 390.0 20.0 20.0 1 FD-ADD  2 TK-FRAME
   4 TK-RECORDS@ T=
   2 TK-CREATED@ T=
   1 TK-CONFCNT@ T=
   2 TK-MATCHED@ T=
   2 TK-NEW@ T=
   \ track length stats (test 712): lengths [3,1] -> sum 4, count 2 (mean 2.0),
   \ median sorted[1]=3, max 3; total hits 4 (mean 2.0 per track)
   TK-LEN-STATS
   4 TK-LSUM@ T=  2 TK-LCNT@ T=  3 TK-LMAX@ T=  3 TK-LMED@ T=  4 TK-THITS@ T= ;

TRK-RUN
TKA-RUN
T-REPORT
end-package
