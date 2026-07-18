\ run-lib-test.f - focused coverage for the DGX Spark host profile and the
\ fresh-child post-run calibration probe (both landed with the GB10 calibration
\ root-cause fix).
\
\ Manual-standalone gate (not a TEST:SUITE member): run directly from the repo
\ root with
\   printf '' | bin/hb --load lib/prelude.f lib/string.f lib/fmt.f lib/test.f \
\          test/run-lib.f test/run-lib-test.f
\
\ The profile-mapping asserts are host-independent (they drive TR-PROFILE-ID
\ directly); the detection and calibration-child asserts are valid on a DGX Spark
\ (GB10) host, where this suite is run.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require test/run-lib.f

package RUN-LIB-TEST

: SPARK ( -- )                               \ pin the runner to the spark profile
   TR-PROFILE-DGX-SPARK-10X2 TR-PROFILE-ID ! ;

: SANE-MS? ( n -- bool )                     \ a plausible single-spin wall time
   dup 1 >= swap 10000 < and ;

: RUN ( -- )
   \ ---- host-independent profile mapping ----------------------------------
   TR-PROFILE-DGX-SPARK-10X2 4 T=
   TR-CAL-REF-SPARK-MS 87 T=
   s" dgx-spark-10x2" TR-PROFILE-ID? TR-PROFILE-DGX-SPARK-10X2 T=
   SPARK TR-CAL-REF-MS 87 T=
   SPARK TR-PROFILE$ s" dgx-spark-10x2" STR= TTRUE
   SPARK TR-VERDICT-CONTROL? TTRUE

   \ ---- detection on this DGX Spark host ----------------------------------
   TR-SPARK-MODEL? TTRUE
   TR-DETECT-PROFILE TR-PROFILE-DGX-SPARK-10X2 T=

   \ ---- the fresh-child calibration probe returns a real measurement ------
   TR-CAL-CHILD-MS? SANE-MS? TTRUE

   T-REPORT
   s" run-lib-test: ok" type cr ;

RUN

;package
