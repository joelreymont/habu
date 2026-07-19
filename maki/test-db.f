\ maki/test-db.f - a parallel slice of the maki checked suite.
\ Split from the monolithic maki/test.f (dot habu-split-monolithic-maki-fccca4ea):
\ the provenance database: commit-store, diff-runner/diff-suite, keywire and
\ audit-log cross-process suites, promotion, obligation and evidence-db checks.
\ Measured self-time on GB10 (idle, 2026-07-19): ~9293 ms across 27 suites.
\ Run standalone: bin/hb --load maki/test-db.f

require maki/test-harness.f

TEST:RESET

TEST:GROUP SEQ maki-db
TEST:SUITE maki/db/artifact-test.f
TEST:;SUITE
TEST:SUITE maki/db/transaction-test.f
TEST:;SUITE
TEST:SUITE maki/db/commit-store-test.f
TEST:;SUITE
TEST:SUITE maki/db/commit-store-crash-test.f
TEST:;SUITE
TEST:SUITE maki/db/diagnostic-test.f
TEST:;SUITE
TEST:SUITE maki/db/obligation-test.f
TEST:;SUITE
TEST:SUITE maki/db/evidence-test.f
TEST:;SUITE
TEST:SUITE maki/db/evidence-applicability-test.f
TEST:;SUITE
TEST:SUITE maki/db/promotion-policy-test.f
TEST:;SUITE
TEST:SUITE maki/db/promotion-authority-test.f
TEST:;SUITE
TEST:SUITE maki/db/promotion-test.f
TEST:;SUITE
TEST:SUITE maki/db/action-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-suite-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-suite-id-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-runner-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-runner-tensor-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-runner-spawn-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-runner-inject-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-case-store-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-case-store-xproc-test.f
TEST:;SUITE
TEST:SUITE maki/db/capbud-test.f
TEST:;SUITE
TEST:SUITE maki/db/agent-loop-test.f
TEST:;SUITE
TEST:SUITE maki/db/audit-log-test.f
TEST:;SUITE
TEST:SUITE maki/db/commit-store-discharge-test.f
TEST:;SUITE
TEST:SUITE maki/db/keywire-xproc-test.f
TEST:;SUITE
TEST:SUITE maki/db/keywire-xproc-env-test.f
TEST:;SUITE
TEST:SUITE maki/db/audit-log-xproc-test.f
TEST:;SUITE
TEST:;GROUP

TEST:RUN
