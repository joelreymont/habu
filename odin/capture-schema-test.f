\ capture-schema-test.f - record-type + schema-version oracle (src/capture_schema.zig).
\ Run: ../habu/bin/hb --load odin/capture-schema-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require odin/capture-schema.f

package SCHEMA
private
: RUN ( -- )
   T-RESET
   \ every record name maps to its type code (test "schema exposes all record types")
   s" schema"   RTYPE SCHEMA   T=
   s" frame"    RTYPE FRAME    T=
   s" sensor"   RTYPE SENSOR   T=
   s" resource" RTYPE RESOURCE T=
   s" error"    RTYPE ERROR    T=
   s" summary"  RTYPE SUMMARY  T=
   s" bogus"    RTYPE UNKNOWN  T=
   \ schema version is exact (test "schema version is exact")
   s" odin.capture.v1" VERSION-OK? TTRUE
   s" odin.localization_detections.v1" VERSION-OK? TTRUE
   s" odin.perception_tick.v1" VERSION-OK? TTRUE
   s" odin.tracker_tick.v1" VERSION-OK? TTRUE
   s" odin.capture.v2" VERSION-OK? TFALSE
   s" odin.localization_detections.v1" VERSION-RTYPE DETECTION T=
   s" odin.perception_tick.v1" VERSION-RTYPE PERCEPTION-TICK T=
   s" odin.tracker_tick.v1" VERSION-RTYPE TRACKER-TICK T=
   s" odin.capture.v1" VERSION-RTYPE UNKNOWN T= ;

RUN
T-REPORT
end-package
