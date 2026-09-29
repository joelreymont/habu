\ The active allocation follows stack switches and nonlocal frame restoration.
require test/engine-stack-lifecycle-lib.f

package STACK-LIFECYCLE-TEST
public

: RUN ( -- )
   T-RESET IN-PROCESS UNGUARDED-UNCAUGHT PRIMITIVE-BOUNDARIES T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN
