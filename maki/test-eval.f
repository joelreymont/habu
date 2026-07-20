\ maki/test-eval.f - a parallel slice of the maki checked suite.
\ Split from the monolithic maki/test.f (dot habu-split-monolithic-maki-fccca4ea):
\ the eval harness: authoring, repair, matrix, pass@k, transcript, live and
\ device-fault checks (the two heavy device-emit suites are in test-eval-emit.f).
\ Measured self-time on GB10 (idle, 2026-07-19): ~5385 ms across 14 suites.
\ Run standalone: bin/hb --load maki/test-eval.f

require maki/test-harness.f

using TEST

RESET

GROUP SEQ maki-eval
SUITE maki/eval/eval-test.f
;SUITE
SUITE maki/eval/fixture.f
;SUITE
SUITE maki/eval/repair.f
;SUITE
SUITE maki/eval/repair-ab-test.f
;SUITE
SUITE maki/eval/repair-mech-test.f
;SUITE
SUITE maki/eval/passk-test.f
;SUITE
SUITE maki/eval/transcript-test.f
;SUITE
SUITE maki/eval/matrix-test.f
;SUITE
SUITE maki/eval/matrix-main.f
;SUITE
SUITE maki/eval/live-test.f
;SUITE
SUITE maki/eval/tokest-test.f
;SUITE
SUITE maki/eval/live-author-test.f
;SUITE
SUITE maki/eval/device-fault-test.f
;SUITE
SUITE maki/eval/train.f
;SUITE
;GROUP

RUN

;using
