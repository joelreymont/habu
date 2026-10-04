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

\ The pool grows with the paths it holds: a megabyte of them keeps each at the
\ offset it was given. EVENT-COPY-PATH is an engine word any source can call,
\ so a negative length, or one so long that its sum with the fill wraps, is
\ refused; the refusal ends the process, so those cases run in a forked child.
$1000 constant IE-CHUNK
$100 constant IE-CHUNKS                  \ a megabyte of paths
$400 constant IE-CAPTURE-CAP
10000 constant IE-TIMEOUT-MS
create IE-SRC IE-CHUNK allot
create IE-OUT IE-CAPTURE-CAP allot
create IE-ERR IE-CAPTURE-CAP allot

\ Path k of the fill is IE-CHUNK copies of byte k.
: IE-CHUNK-FILL ( n -- )
   {: k:n :}
   IE-CHUNK 0 ?do k IE-SRC i + c! loop ;

: IE-POOL-FILL ( -- )
   EVENTS-RESET
   IE-CHUNKS 0 ?do
      i IE-CHUNK-FILL
      IE-SRC IE-CHUNK EVENT-COPY-PATH IE-CHUNK T= i IE-CHUNK * T=
   loop ;

\ The first and last byte of each path that are not its own.
: IE-POOL-MISSES ( -- n )
   0
   IE-CHUNKS 0 ?do
      i IE-CHUNK * EVENT-POOL-AT c@ i $ff and <> if 1 + then
      i 1 + IE-CHUNK * 1 - EVENT-POOL-AT c@ i $ff and <> if 1 + then
   loop ;

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
   s" a megabyte of paths keeps each at its offset" T-LABEL
   IE-POOL-FILL
   IE-POOL-MISSES 0 T=
   s" -1 and the maximum cell are refused" T-LABEL
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
