\ latency-calibration-test.f - oracle checks for the latency_calibration core.
\ src/latency_calibration.zig has no inline tests; these use exact hand-computed
\ values.
\ Run: ../habu/bin/hb --load odin/latency-calibration-test.f

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/sort.f
require lib/test.f
require odin/float-cell.f
require odin/latency-calibration.f

package LATCAL
private
create A3 3 cells allot   create A4 4 cells allot   create A5 5 cells allot   create LAT 3 cells allot
: SETA 1 A3 0 cells + ! 2 A3 1 cells + ! 3 A3 2 cells + !
       1 A4 0 cells + ! 2 A4 1 cells + ! 3 A4 2 cells + ! 4 A4 3 cells + !
       10 A5 0 cells + ! 20 A5 1 cells + ! 30 A5 2 cells + ! 40 A5 3 cells + ! 50 A5 4 cells + !
       100 LAT 0 cells + ! 200 LAT 1 cells + ! 300 LAT 2 cells + ! ;
: RUN ( -- )
   T-RESET  SETA
   2 A3 3 MEDIAN-I64 T=                 \ odd median
   2 A4 4 MEDIAN-I64 T=                 \ even median = divTrunc(2+3,2)=2
   50 A5 5 95 PCTL-U64 T=               \ p95 -> sorted[4]=50
   30 A5 5 50 PCTL-U64 T=               \ p50 -> sorted[2]=30
   500000 0.5 MAXJITTER T=
   100 -100 ABS T=  100 100 ABS T=
   LAT 3 50 LATSTATS                    \ latencies [100,200,300], jitter 50ns
   200 MEAN@ T=  200 LC-MED@ T=  100 MIN@ T=  300 LC-MAX@ T=
   100 RP95@ T=  100 LC-RMAX@ T=  2 LC-OUT@ T= ;
RUN
T-REPORT
end-package
