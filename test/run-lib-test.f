\ run-lib-test.f - focused coverage for the DGX Spark host profile and the
\ startup calibration reference it selects.
\
\ Manual-standalone gate (not a TEST:SUITE member): run directly from the repo
\ root with
\   printf '' | bin/hb --load lib/prelude.f lib/string.f lib/fmt.f lib/test.f \
\          test/run-lib.f test/run-lib-test.f
\
\ The profile-mapping asserts are host-independent (they drive PROFILE!
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

package TEST

private

: SPARK ( -- )                               \ pin the runner to the spark profile
   PROFILE-DGX-SPARK-10X2 PROFILE! ;

: RUN-LIB-TEST ( -- )
   \ ---- host-independent profile mapping ----------------------------------
   PROFILE-DGX-SPARK-10X2 4 T=
   CAL-REF-SPARK-MS 87 T=
   s" dgx-spark-10x2" PROFILE-ID? PROFILE-DGX-SPARK-10X2 T=
   SPARK CAL-REF-MS 87 T=
   SPARK PROFILE$ s" dgx-spark-10x2" STR= TTRUE

   \ ---- detection on this DGX Spark host ----------------------------------
   SPARK-MODEL? TTRUE
   DETECT-PROFILE PROFILE-DGX-SPARK-10X2 T=

   \ ---- PROFILE! is a behaviour with a domain, not a store ----------------
   \ It is the runner's only public mutator, so it has to refuse an id the
   \ runner has no profile for instead of storing it and letting the first
   \ lookup fall through a `case` with nothing to match.
   s" every known profile id is accepted" T-LABEL
   [: TR-PROFILE-MACOS-ARM64-10X2 PROFILE! ;] catch 0 T=
   [: PROFILE-DGX-SPARK-10X2 PROFILE! ;] catch 0 T=
   s" an id past the known profiles is refused" T-LABEL
   [: PROFILE-DGX-SPARK-10X2 1+ PROFILE! ;] E-TR-PROFILE TTHROWSQ
   s" a zero or negative id is refused" T-LABEL
   [: 0 PROFILE! ;] E-TR-PROFILE TTHROWSQ
   [: -1 PROFILE! ;] E-TR-PROFILE TTHROWSQ
   s" the refusal left the profile as it was" T-LABEL
   SPARK PROFILE PROFILE-DGX-SPARK-10X2 T=

   T-REPORT
   s" run-lib-test: ok" type cr ;

RUN-LIB-TEST

;package
