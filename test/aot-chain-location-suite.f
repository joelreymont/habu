\ aot-chain-location-suite.f - the chain producer refuses declared rows that
\ do not match the live window's rows, through the real producer.
\
\ A private source-built host captures the real compiler with a copy of
\ tools/aot-chain-capture.f; each case drops a row, duplicates one or moves a
\ row's location, and the producer must refuse by its own code. The rows whose
\ target is wrong are test/aot-chain-target-suite.f and the accepting controls
\ test/aot-chain-producer-suite.f, gate rows of their own: each case captures
\ the whole compiler in its own child.
\
\ Registered as `TEST:SUITE aot-chain-location`. Run standalone:
\   bin/hb --load test/aot-chain-location-suite.f

require lib/test.f
require test/aot-chain-producer-lib.f

package AOT-CHAIN-SUITE

: PROBE-LOCATION-REFUSALS ( -- )
   s" missing" REFUSE-RC PRODUCER-CASE
   s" duplicate" REFUSE-RC PRODUCER-CASE
   s" location" REFUSE-RC PRODUCER-CASE ;

\ Public so the driver below runs it with the package closed.
public

: LOCATION-RUN ( -- )
   [: SETUP PREPARE-PRODUCER PROBE-LOCATION-REFUSALS ;] RUN-PROBES
   s" aot-chain-location: ok" type cr ;

;package

AOT-CHAIN-SUITE:LOCATION-RUN
