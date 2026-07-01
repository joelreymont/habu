\ latency-xcorr-test.f - oracle checks for the latency_xcorr core. src/latency_xcorr.zig
\ has no inline tests; these use exact hand-computable values: norm(3,4,0)=5,
\ ms->ns truncation, abs, and Pearson r=1.0 over a perfectly-correlated set
\ (x=[10,20,30], y=[1,2,3] -> r=1, mean_x=20, mean_y=2).
\ Run: ../habu/bin/hb --load odin/latency-xcorr-test.f

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/test.f
require odin/float-cell.f
require odin/latency-xcorr.f

package XCORR
private
: FL-NEAR ( r r -- bool ) f- fabs 0.0000001 f< ;
: T-NEAR ( r r -- ) FL-NEAR T-ASSERT ;
create XS 3 cells allot  create YS 3 cells allot
: SETXY 10.0 XS 0 cells + F! 20.0 XS 1 cells + F! 30.0 XS 2 cells + F!
        1.0 YS 0 cells + F! 2.0 YS 1 cells + F! 3.0 YS 2 cells + F! ;
: XC-RUN ( -- )
   T-RESET
   3.0 4.0 0.0 NORM 5.0 T-NEAR
   1.0 MS>NS 1000000 T=
   -200.0 MS>NS -200000000 T=
   -100 ABS-I64 100 T=
   0 ABS-I64 0 T=
   SETXY  XS YS 3 PEARSON  2.0 T-NEAR 20.0 T-NEAR 1.0 T-NEAR ;
XC-RUN
T-REPORT
end-package
