\ maki/test-eval-emit.f - a parallel slice of the maki checked suite.
\ Split from the monolithic maki/test.f (dot habu-split-monolithic-maki-fccca4ea):
\ the two heavy eval device-emit codegen suites (emit-device, emit), split out
\ so the eval harness slice stays light.
\ Measured self-time on GB10 (idle, 2026-07-19): ~6877 ms across 2 suites.
\ Run standalone: bin/hb --load maki/test-eval-emit.f

require maki/test-harness.f

using TEST

RESET

GROUP SEQ maki-eval-emit
SUITE maki/eval/emit-test.f
;SUITE
SUITE maki/eval/emit-device-test.f
;SUITE
SUITE maki/eval/emit-device-leak-test.f
;SUITE
;GROUP

RUN

;using
