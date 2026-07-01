\ user-assisted-plan-test.f - pure helpers for the user-assisted planner.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/test.f
require odin/user-assisted-plan.f

package UAP

: UAP-TEST-BATCHES ( -- )
   s" latency_vibration" UAP-BATCH-ID UAP-BATCH-LATENCY T=
   s" sky_low_light" UAP-BATCH-ID UAP-BATCH-SKY T=
   s" motion" UAP-BATCH-ID UAP-BATCH-MOTION T=
   s" geometry_proxy" UAP-BATCH-ID UAP-BATCH-GEOMETRY T=
   s" other" UAP-BATCH-ID UAP-BATCH-NONE T= ;

: UAP-TEST-NAMES ( -- )
   s" cam_a0" UAP-NAME? TTRUE
   s" cam-a.0" UAP-NAME? TTRUE
   s" _cam" UAP-NAME? TFALSE
   s" cam/a0" UAP-NAME? TFALSE
   s" " UAP-NAME? TFALSE ;

: UAP-TEST-PATHS ( -- )
   UAP-DEFAULTS
   s" configs/cameras.json" UAP-ODIN-PATH$ s" ../Odin/configs/cameras.json" T$=
   s" cam_a0" UAP-BASELINE$ s" ../Odin/results/imu/cam_a0_spsc_static_20260624_0502_CEST/imu.ndjson" T$=
   s" /tmp/odin-root" UAP-ROOT!
   s" imu-root" UAP-BASELINE-ROOT!
   s" static_1" UAP-BASELINE-SUFFIX!
   s" cam_b1" UAP-BASELINE$ s" /tmp/odin-root/imu-root/cam_b1_static_1/imu.ndjson" T$= ;

: UAP-TEST-RUN ( -- )
   T-RESET
   UAP-TEST-BATCHES
   UAP-TEST-NAMES
   UAP-TEST-PATHS ;

UAP-TEST-RUN
T-REPORT

end-package
