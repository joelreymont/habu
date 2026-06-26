\ luma-hist-test.f - histogram luminance stats, hand-computed oracle.
\ Run: cat lib/errors.f lib/test.f odin/luma-hist.f odin/luma-hist-test.f | bin/hb

package LUMA
private
create H BINS cells allot
create LUMA-A 10 c, 20 c, 30 c, 40 c,            \ four distinct luma values
create LUMA-B 0 c, 0 c, 255 c, 255 c,            \ two extremes

: RUN ( -- )
   T-RESET
   \ {10,20,30,40}: total 4, mean 25, p25=10, p50=20, p100=40
   H RESET  H LUMA-A 4 ADD
   H TOTAL  4 T=
   H MEAN   25 T=
   H 25 100 PCT  10 T=
   H 50 100 PCT  20 T=
   H 100 100 PCT 40 T=
   \ {0,0,255,255}: total 4, mean 127, p50=0 (lower median), p100=255
   H RESET  H LUMA-B 4 ADD
   H TOTAL  4 T=
   H MEAN   127 T=
   H 50 100 PCT  0 T=
   H 100 100 PCT 255 T=
   \ empty histogram: mean 0, percentile 0
   H RESET
   H TOTAL 0 T=
   H MEAN  0 T=
   H 50 100 PCT 0 T= ;

RUN
T-REPORT
end-package
