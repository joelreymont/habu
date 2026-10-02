\ include-events-test.f - checked fixtures for the source-composition event log.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f tools/include-events-test.f
\
\ The discovery flow covers include/require ordering and token spans. These
\ cases cover repeated provided events and disabling event capture.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/memory.f
require lib/process.f
require lib/test/subject.f

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

\ EVENT-COPY-PATH is an engine word any source can call, so it measures a path
\ against the room left in the pool: a length near the maximum cell would wrap
\ the sum back under EVENT-POOL-CAP, and a negative one would slip under it. The
\ refusal ends the process, so the refused cases run in a forked child.
$1000 constant IE-CHUNK
$400 constant IE-CAPTURE-CAP
10000 constant IE-TIMEOUT-MS
create IE-SRC IE-CHUNK allot
create IE-OUT IE-CAPTURE-CAP allot
create IE-ERR IE-CAPTURE-CAP allot

: IE-POOL-FILL ( -- )
   EVENTS-RESET
   EVENT-POOL-CAP IE-CHUNK / 0 ?do IE-SRC IE-CHUNK EVENT-COPY-PATH 2drop loop
   IE-SRC EVENT-POOL-CAP IE-CHUNK mod EVENT-COPY-PATH 2drop ;
: IE-POOL-OVER ( -- ) IE-POOL-FILL IE-SRC 1 EVENT-COPY-PATH 2drop ;
: IE-POOL-NEG ( -- )
   EVENTS-RESET IE-SRC 1 EVENT-COPY-PATH 2drop IE-SRC -1 EVENT-COPY-PATH 2drop ;
: IE-POOL-MAX ( -- )
   EVENTS-RESET IE-SRC 1 EVENT-COPY-PATH 2drop
   IE-SRC -1 1 rshift EVENT-COPY-PATH 2drop ;

: IE-DIES ( ptr u8 n -- ) {: source:ptr sourceu:n :}
   source sourceu IE-OUT IE-CAPTURE-CAP >LEN IE-ERR IE-CAPTURE-CAP >LEN
   IE-TIMEOUT-MS >MS SUBJECT:RUN {: outu:len erru:len oc :}
   source sourceu IE-OUT outu LEN>N IE-ERR erru LEN>N oc INCLUDE-EVENT-RC
   T-OUTCOME-EXITED=
   outu LEN>N 0 T=
   IE-ERR erru LEN>N S\" events: pool overflow\n" T$= ;

: IE-TEST-POOL-ROOM ( -- )
   s" a path that fills the event pool exactly" T-LABEL
   IE-POOL-FILL IE-SRC 0 EVENT-COPY-PATH drop EVENT-POOL-CAP T=
   s" a path one past the pool, -1 and the maximum cell are refused" T-LABEL
   s" IE-POOL-OVER" IE-DIES
   s" IE-POOL-NEG" IE-DIES
   s" IE-POOL-MAX" IE-DIES
   EVENTS-RESET ;

: IE-MAIN ( -- )
   T-RESET
   IE-TEST-PROVIDED-DEDUP
   IE-TEST-DISABLED-NO-RECORD
   IE-TEST-POOL-ROOM
   EVENT-OFF DISCOVERY-OFF EVENTS-RESET
   T-REPORT
   s" include-events-test: ok" type cr ;

IE-MAIN

;package
