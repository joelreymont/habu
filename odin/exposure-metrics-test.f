\ exposure-metrics-test.f - oracle checks for the exposure_metrics core (no inline
\ tests in src/exposure_metrics.zig; exact hand-computed values).
\ Run: cat lib/errors.f lib/string.f lib/float.f lib/test.f odin/float-cell.f \
\        lib/render.f odin/exposure-metrics.f odin/exposure-metrics-test.f | bin/hb

package EXPOSURE
private
: FL-NEAR ( r r -- bool ) f- fabs 0.001 f< ;
: T-NEAR ( r r -- ) FL-NEAR T-ASSERT ;
create HIST 256 cells allot  variable HZ
: ZHIST  0 HZ ! begin HZ @ 256 < while 0 HIST HZ @ cells + ! HZ @ 1+ HZ ! repeat ;
: EX-RUN ( -- )
   T-RESET
   255 255 255 LUMA-RGB 255 T=
   0 0 0 LUMA-RGB 0 T=
   255 0 0 LUMA-RGB 54 T=
   100 3 INT-MEAN 33 T=   102 3 INT-MEAN 34 T=   5 0 INT-MEAN 0 T=
   65280 256 MEAN-LUM 255 T=   65279 256 MEAN-LUM 254 T=
   100.0 3 FLT-MEAN 33.3333 T-NEAR
   ZHIST  4 HIST 100 cells + !
   HIST 4 50 100 PCTL 100 T=
   HIST 4 5 100 PCTL 100 T=
   HIST 4 95 100 PCTL 100 T=
   HIST 4 CONTRAST 0 T=
   RB-RESET 1 2 WRITE-RATIO RB$ s" 50.000" STR= T-ASSERT
   RB-RESET 1 3 WRITE-RATIO RB$ s" 33.333" STR= T-ASSERT
   RB-RESET 1 6 WRITE-RATIO RB$ s" 16.666" STR= T-ASSERT
   RB-RESET 0 0 WRITE-RATIO RB$ s" 0.000" STR= T-ASSERT ;
EX-RUN
T-REPORT
end-package
