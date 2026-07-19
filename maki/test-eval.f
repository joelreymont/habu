\ maki/test-eval.f - a parallel slice of the maki checked suite.
\ Split from the monolithic maki/test.f (dot habu-split-monolithic-maki-fccca4ea):
\ the eval harness: authoring, repair, matrix, pass@k, transcript, live and
\ device-fault checks (the two heavy device-emit suites are in test-eval-emit.f).
\ Measured self-time on GB10 (idle, 2026-07-19): ~5385 ms across 14 suites.
\ Run standalone: bin/hb --load maki/test-eval.f

require maki/test-harness.f

TEST:RESET

TEST:GROUP SEQ maki-eval
TEST:SUITE maki/eval/eval-test.f
TEST:;SUITE
TEST:SUITE maki/eval/fixture.f
TEST:;SUITE
TEST:SUITE maki/eval/repair.f
TEST:;SUITE
TEST:SUITE maki/eval/repair-ab-test.f
TEST:;SUITE
TEST:SUITE maki/eval/repair-mech-test.f
TEST:;SUITE
TEST:SUITE maki/eval/passk-test.f
TEST:;SUITE
TEST:SUITE maki/eval/transcript-test.f
TEST:;SUITE
TEST:SUITE maki/eval/matrix-test.f
TEST:;SUITE
TEST:SUITE maki/eval/matrix-main.f
TEST:;SUITE
TEST:SUITE maki/eval/live-test.f
TEST:;SUITE
TEST:SUITE maki/eval/tokest-test.f
TEST:;SUITE
TEST:SUITE maki/eval/live-author-test.f
TEST:;SUITE
TEST:SUITE maki/eval/device-fault-test.f
TEST:;SUITE
TEST:SUITE maki/eval/train.f
TEST:;SUITE
TEST:;GROUP

TEST:RUN
