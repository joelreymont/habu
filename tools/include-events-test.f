\ include-events-test.f - checked fixtures for the source-composition event log.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f tools/include-events-test.f
\
\ Drives the core loader words (included/required/provided) in discovery mode
\ (record-only, no file load) and asserts the ordered event log records include
\ multiplicity, require/provided exact-string registry state, loader-token span,
\ and exact path bytes.

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

: IE-TEST-MULTIPLICITY ( -- )
   IE-BEGIN
   s" tfam5-ie-a.f" included
   s" tfam5-ie-a.f" included
   EVENT-COUNT 2 T=
   0 EVENT-KIND@ EV-INCLUDED T=
   1 EVENT-KIND@ EV-INCLUDED T=
   0 EVENT-STATE@ EV-STATE-FRESH T=
   1 EVENT-STATE@ EV-STATE-FRESH T=
   0 EVENT-PATH@ s" tfam5-ie-a.f" T$=
   IE-END ;

: IE-TEST-REQUIRE-DEDUP ( -- )
   IE-BEGIN
   s" tfam5-ie-b.f" required
   s" tfam5-ie-b.f" required
   EVENT-COUNT 2 T=
   0 EVENT-KIND@ EV-REQUIRED T=
   1 EVENT-KIND@ EV-REQUIRED T=
   0 EVENT-STATE@ EV-STATE-FRESH T=
   1 EVENT-STATE@ EV-STATE-KNOWN T=
   IE-END ;

: IE-TEST-PROVIDED-DEDUP ( -- )
   IE-BEGIN
   s" tfam5-ie-c.f" provided
   s" tfam5-ie-c.f" provided
   EVENT-COUNT 2 T=
   0 EVENT-KIND@ EV-PROVIDED T=
   0 EVENT-STATE@ EV-STATE-FRESH T=
   1 EVENT-STATE@ EV-STATE-KNOWN T=
   IE-END ;

: IE-TEST-EXACT-SPELLING ( -- )
   IE-BEGIN
   s" ./tfam5-ie-d.f" required
   s" tfam5-ie-d.f" required
   EVENT-COUNT 2 T=
   0 EVENT-STATE@ EV-STATE-FRESH T=
   1 EVENT-STATE@ EV-STATE-FRESH T=
   IE-END ;

: IE-TEST-SPAN ( -- )
   IE-BEGIN
   $DEAD 7 DISC-TOK!
   s" tfam5-ie-e.f" included
   0 EVENT-TOK@ {: toka:n toku:n :}
   toku 7 T=
   toka $DEAD T=
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
   IE-TEST-MULTIPLICITY
   IE-TEST-REQUIRE-DEDUP
   IE-TEST-PROVIDED-DEDUP
   IE-TEST-EXACT-SPELLING
   IE-TEST-SPAN
   IE-TEST-DISABLED-NO-RECORD
   EVENT-OFF DISCOVERY-OFF EVENTS-RESET
   T-REPORT
   s" include-events-test: ok" type cr ;

IE-MAIN

;package
