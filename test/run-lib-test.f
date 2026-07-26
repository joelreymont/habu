\ run-lib-test.f - focused coverage for the DGX Spark host profile and the
\ startup calibration reference it selects.
\
\ Manual-standalone gate (not a TEST:SUITE member): run directly from the repo
\ root with
\   printf '' | bin/hb --load lib/prelude.f lib/string.f lib/fmt.f lib/test.f \
\          test/run-lib.f test/run-lib-test.f
\
\ The profile-mapping asserts are host-independent (they drive TEST-RUN:PROFILE!
\ directly); the detection asserts are valid on a DGX Spark (GB10) host, where
\ this suite is run.
\
\ The fresh-child post-run calibration probe that used to be covered here went
\ with the whole-gate performance verdict: nothing times the gate as a whole any
\ more, so nothing needs a second spin reading after it.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require test/run-lib.f

package RUN-LIB-TEST

: SPARK ( -- )                               \ pin the runner to the spark profile
   TEST-RUN:PROFILE-DGX-SPARK-10X2 TEST-RUN:PROFILE! ;

: RUN ( -- )
   \ ---- host-independent profile mapping ----------------------------------
   TEST-RUN:PROFILE-DGX-SPARK-10X2 4 T=
   TEST-RUN:CAL-REF-SPARK-MS 87 T=
   s" dgx-spark-10x2" TEST-RUN:PROFILE-ID? TEST-RUN:PROFILE-DGX-SPARK-10X2 T=
   SPARK TEST-RUN:CAL-REF-MS 87 T=
   SPARK TEST-RUN:PROFILE$ s" dgx-spark-10x2" STR= TTRUE

   \ ---- detection on this DGX Spark host ----------------------------------
   TEST-RUN:SPARK-MODEL? TTRUE
   TEST-RUN:DETECT-PROFILE TEST-RUN:PROFILE-DGX-SPARK-10X2 T=

   T-REPORT
   s" run-lib-test: ok" type cr ;

RUN

;package
