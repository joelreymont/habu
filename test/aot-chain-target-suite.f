\ aot-chain-target-suite.f - the chain producer refuses declared rows whose
\ target does not match the live window's, through the real producer.
\
\ A private source-built host captures the real compiler with a copy of
\ tools/aot-chain-capture.f; each case flips a row's target kind, moves its
\ target or nulls it, and the producer must refuse by its own code. The row
\ set and location refusals are test/aot-chain-location-suite.f and the
\ accepting controls test/aot-chain-producer-suite.f, gate rows of their own:
\ each case captures the whole compiler in its own child.
\
\ Registered as `TEST:SUITE aot-chain-target`. Run standalone:
\   bin/hb --load test/aot-chain-target-suite.f

require lib/test.f
require test/aot-chain-producer-lib.f

package AOT-CHAIN-SUITE

: PROBE-TARGET-REFUSALS ( -- )
   s" kind" REFUSE-RC PRODUCER-CASE
   s" target" REFUSE-RC PRODUCER-CASE
   s" null-target" REFUSE-RC PRODUCER-CASE ;

\ Public so the driver below runs it with the package closed.
public

: TARGET-RUN ( -- )
   [: SETUP PREPARE-PRODUCER PROBE-TARGET-REFUSALS ;] RUN-PROBES
   s" aot-chain-target: ok" type cr ;

;package

AOT-CHAIN-SUITE:TARGET-RUN
