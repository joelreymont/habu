\ include-events-test.f - checked fixtures for the source-composition event log.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f tools/include-events-test.f
\
\ The discovery flow covers include/require ordering and token spans. These
\ cases cover repeated provided events and disabling event capture.

require lib/errors.f
require lib/string.f
require lib/test.f

package IE-TEST

variable IE-SAVE-N

: IE-BEGIN ( -- )
   REQUIRE-N @ IE-SAVE-N !
   EVENTS-RESET  EVENT-ON  DISCOVERY-ON ;

: IE-END ( -- )
   DISCOVERY-OFF  EVENT-OFF
   IE-SAVE-N @ REQUIRE-N ! ;

: IE-TEST-PROVIDED-DEDUP ( -- )
   IE-BEGIN
   s" tfam5-ie-c.f" provided
   s" tfam5-ie-c.f" provided
   EVENT-COUNT 2 T=
   0 EVENT-KIND@ EV-PROVIDED T=
   0 EVENT-STATE@ EV-STATE-FRESH T=
   1 EVENT-STATE@ EV-STATE-KNOWN T=
   IE-END ;

: IE-TEST-DISABLED-NO-RECORD ( -- )
   REQUIRE-N @ IE-SAVE-N !
   EVENTS-RESET  EVENT-OFF  DISCOVERY-ON
   s" tfam5-ie-f.f" required
   EVENT-COUNT 0 T=
   DISCOVERY-OFF
   IE-SAVE-N @ REQUIRE-N ! ;

: IE-MAIN ( -- )
   T-RESET
   IE-TEST-PROVIDED-DEDUP
   IE-TEST-DISABLED-NO-RECORD
   EVENT-OFF DISCOVERY-OFF EVENTS-RESET
   T-REPORT
   s" include-events-test: ok" type cr ;

IE-MAIN

;package
