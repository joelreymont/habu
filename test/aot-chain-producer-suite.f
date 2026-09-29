\ aot-chain-producer-suite.f - the chain producer accepts its live rows, through
\ the real producer (dot habu-retire-the-s-4fbc244f).
\
\ A private source-built host captures the real compiler with a copy of
\ tools/aot-chain-capture.f and runs the producer's live-row checks over the
\ unaltered rows, with two rows swapped (the order-independent control) and
\ over an index past the former fixed row limit. Each case captures the whole
\ compiler in its own child, so the refusals are rows of their own:
\ test/aot-chain-location-suite.f and test/aot-chain-target-suite.f. The
\ artifact format and the capture tool's refusal are
\ test/aot-chain-capture-suite.f.
\
\ Registered as `TEST:SUITE aot-chain-producer`. Run standalone:
\   bin/hb --load test/aot-chain-producer-suite.f

require lib/test.f
require test/aot-chain-producer-lib.f

package AOT-CHAIN-SUITE

: PROBE-PRODUCER-ACCEPTS ( -- )
   s" valid" 0 PRODUCER-CASE
   s" chain-closure: portable" SAID?
   s" reorder" 0 PRODUCER-CASE
   s" index-scale" 0 PRODUCER-CASE ;

\ Public so the driver below runs it with the package closed.
public

: PRODUCER-RUN ( -- )
   [: SETUP PREPARE-PRODUCER PROBE-PRODUCER-ACCEPTS ;] RUN-PROBES
   s" aot-chain-producer: ok" type cr ;

;package

AOT-CHAIN-SUITE:PRODUCER-RUN
