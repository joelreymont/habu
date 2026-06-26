\ perception-latency-test.f - latency percentile oracle. Expected values produced
\ by running src/perception_latency.zig's percentileF64 directly (zig 0.16):
\   [10,20,30,40,50] -> p50=30 p95=50 p99=50 max=50
\   [1..8]           -> p50=4  p95=8  p99=8  max=8
\ Run: cat lib/errors.f lib/string.f lib/float.f lib/sort.f lib/prelude.f lib/test.f \
\        odin/float-cell.f odin/perception-latency.f odin/perception-latency-test.f | bin/hb

package PERCEPTION
private
: FL-NEAR ( r r -- bool ) f- fabs 0.000001 f< ;
: T-NEAR ( r r -- ) FL-NEAR T-ASSERT ;

: PL-RUN ( -- )
   T-RESET
   PL-RESET
   30.0 PL-ADD  10.0 PL-ADD  50.0 PL-ADD  20.0 PL-ADD  40.0 PL-ADD
   PL-FINISH
   5 PL-SAMPLES@ T=
   30.0 PL-P50@ T-NEAR  50.0 PL-P95@ T-NEAR  50.0 PL-P99@ T-NEAR  50.0 PL-MAX@ T-NEAR
   PL-RESET
   8.0 PL-ADD 1.0 PL-ADD 7.0 PL-ADD 2.0 PL-ADD 6.0 PL-ADD 3.0 PL-ADD 5.0 PL-ADD 4.0 PL-ADD
   PL-FINISH
   8 PL-SAMPLES@ T=
   4.0 PL-P50@ T-NEAR  8.0 PL-P95@ T-NEAR  8.0 PL-P99@ T-NEAR  8.0 PL-MAX@ T-NEAR ;

PL-RUN
T-REPORT
end-package
