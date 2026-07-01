\ perception-latency-rates-test.f - oracle tests for the rate/queue/timing
\ reductions added to perception-latency.f, ported from analyze() in
\ src/perception_latency.zig. The .zig carries no inline test, so the expected
\ values were produced by running the exact .zig functions (QueueStats,
\ rateFromWindow, percentileF64/summarizeTiming) under zig 0.16:
\   queue[2,5,3,5,1]            -> samples=5 sum=16 max=5 mean=3.2
\   rate(61,0,1e9)=60.0  rate(3,0,2e9)=1.0  rate(3,1e9,1033333334)=59.9999988
\   rate(count<2)=null   rate(last<=first)=null
\   timing[10..1]               -> samples=10 p50=5 p95=10 p99=10 max=10
\   ratesum[60,30,90]           -> min=30 max=90 sum=180 mean=60 (exact reduction)
\ Run: ../habu/bin/hb --load odin/perception-latency-rates-test.f

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/sort.f
require lib/prelude.f
require lib/test.f
require odin/float-cell.f
require odin/perception-latency.f

package PERCEPTION
private
: FL-NEAR ( r r -- bool ) f- fabs 0.000001 f< ;
: T-NEAR ( r r -- ) FL-NEAR T-ASSERT ;

create TVALS 10 cells allot
: SET-TVALS ( -- )           \ [10,9,8,7,6,5,4,3,2,1] (unsorted on purpose)
   10.0 TVALS 0 cells + F!  9.0 TVALS 1 cells + F!  8.0 TVALS 2 cells + F!
   7.0  TVALS 3 cells + F!  6.0 TVALS 4 cells + F!  5.0 TVALS 5 cells + F!
   4.0  TVALS 6 cells + F!  3.0 TVALS 7 cells + F!  2.0 TVALS 8 cells + F!
   1.0  TVALS 9 cells + F! ;

create RVALS 3 cells allot
: SET-RVALS ( -- ) 60.0 RVALS 0 cells + F!  30.0 RVALS 1 cells + F!  90.0 RVALS 2 cells + F! ;

: PLR-RUN ( -- )
   T-RESET
   \ QueueStats
   QS-RESET  2 QS-OBSERVE 5 QS-OBSERVE 3 QS-OBSERVE 5 QS-OBSERVE 1 QS-OBSERVE
   5 QS-SAMPLES T=  16 QS-SUM@ T=  5 QS-MAX@ T=
   QS-MEAN T-ASSERT  3.2 T-NEAR
   \ rateFromWindow
   61 0 1000000000 RATE-WINDOW T-ASSERT  60.0 T-NEAR
   3 0 2000000000 RATE-WINDOW T-ASSERT  1.0 T-NEAR
   3 1000000000 1033333334 RATE-WINDOW T-ASSERT  59.9999988 T-NEAR
   1 0 1 RATE-WINDOW 0= T-ASSERT fdrop                 \ count<2 -> null
   2 5 5 RATE-WINDOW 0= T-ASSERT fdrop                 \ last<=first -> null
   \ summarizeTiming
   SET-TVALS  TVALS 10 TSUM
   10 TS-SAMPLES T=
   5.0 TS-P50@ T-NEAR  10.0 TS-P95@ T-NEAR  10.0 TS-P99@ T-NEAR  10.0 TS-MAX@ T-NEAR
   \ computeRateSummaries
   SET-RVALS  RVALS 3 RSUM
   3 RS-COUNT T=
   30.0 RS-MIN@ T-NEAR  90.0 RS-MAX@ T-NEAR  180.0 RS-SUM@ T-NEAR
   RS-MEAN T-ASSERT  60.0 T-NEAR ;

PLR-RUN
T-REPORT
end-package
