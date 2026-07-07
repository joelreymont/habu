\ run-budget-cal-test.f - focused tests for calibration-derived gate budgets.
\ Run: bin/hb --load lib/test.f test/run-lib.f test/run-budget-cal-test.f

require lib/test.f
require test/run-lib.f

package TRBC

: PROBE-MEASURES ( -- )
   TR-CALIBRATE
   TR-CAL-MEASURED-MS @ 0 > TTRUE ;

: MACOS! ( n -- )
   TR-PROFILE-MACOS-ARM64-10X2 TR-PROFILE-ID !
   TR-CAL-MEASURED-MS ! ;

: NOMINAL-KEEPS-BUDGET ( -- )
   TR-CAL-REF-MACOS-MS MACOS!
   TR-CAL-PCT 100 T=
   40000 TR-CAL-SCALED 40000 T= ;

: SLOW-HOST-SCALES ( -- )
   TR-CAL-REF-MACOS-MS 2 * MACOS!
   TR-CAL-PCT 200 T=
   40000 TR-CAL-SCALED 80000 T=
   45000 TR-CAL-SCALED 90000 T= ;

: FAST-HOST-CLAMPS-LOW ( -- )
   TR-CAL-REF-MACOS-MS 2 / MACOS!
   TR-CAL-PCT 100 T=
   40000 TR-CAL-SCALED 40000 T= ;

: THRASHING-CLAMPS-HIGH ( -- )
   TR-CAL-REF-MACOS-MS 10 * MACOS!
   TR-CAL-PCT 300 T=
   40000 TR-CAL-SCALED 120000 T= ;

: UNCALIBRATED-PROFILE-KEEPS-TABLE ( -- )
   TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 TR-PROFILE-ID !
   TR-CAL-REF-MACOS-MS 10 * TR-CAL-MEASURED-MS !
   TR-CAL-PCT 100 T=
   100000 TR-CAL-SCALED 100000 T= ;

: UNMEASURED-KEEPS-TABLE ( -- )
   0 MACOS!
   TR-CAL-PCT 100 T= ;

\ HB_LOAD_PCT export text: the clamp guarantees 100..300, so the exported env
\ value is always exactly three digits - the worker-side parser relies on it.
: PCT-TEXT-NOMINAL ( -- )
   TR-CAL-REF-MACOS-MS MACOS!
   TR-CAL-PCT TR-PCT$ s" 100" T$= ;

: PCT-TEXT-SCALED ( -- )
   TR-CAL-REF-MACOS-MS 2 * MACOS!
   TR-CAL-PCT TR-PCT$ s" 200" T$= ;

: PCT-TEXT-CLAMPED ( -- )
   TR-CAL-REF-MACOS-MS 10 * MACOS!
   TR-CAL-PCT TR-PCT$ s" 300" T$= ;

\ The export floors the calibration at the gate's own pool pressure
\ (nested x 100): an idle-box calibration still budgets for in-gate
\ self-contention, while a heavy calibration wins over a light floor.
: EXPORT-FLOORS-AT-PRESSURE ( -- )
   TR-CAL-REF-MACOS-MS MACOS!
   2 TR-NESTED-POOL !
   TR-LOAD-PCT-EXPORT 200 T= ;

: EXPORT-KEEPS-HEAVY-CAL ( -- )
   TR-CAL-REF-MACOS-MS 3 * MACOS!
   2 TR-NESTED-POOL !
   TR-LOAD-PCT-EXPORT 300 T= ;

: EXPORT-NO-NEST-NO-FLOOR ( -- )
   TR-CAL-REF-MACOS-MS MACOS!
   1 TR-NESTED-POOL !
   TR-LOAD-PCT-EXPORT 100 T= ;

: MAIN ( -- )
   T-RESET
   PROBE-MEASURES
   NOMINAL-KEEPS-BUDGET
   SLOW-HOST-SCALES
   FAST-HOST-CLAMPS-LOW
   THRASHING-CLAMPS-HIGH
   UNCALIBRATED-PROFILE-KEEPS-TABLE
   UNMEASURED-KEEPS-TABLE
   PCT-TEXT-NOMINAL
   PCT-TEXT-SCALED
   PCT-TEXT-CLAMPED
   EXPORT-FLOORS-AT-PRESSURE
   EXPORT-KEEPS-HEAVY-CAL
   EXPORT-NO-NEST-NO-FLOOR
   T-REPORT ;

MAIN

end-package
