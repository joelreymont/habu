\ maki/test-db.f - a parallel slice of the maki checked suite.
\ Split from the monolithic maki/test.f (dot habu-split-monolithic-maki-fccca4ea):
\ the provenance database: commit-store, diff-runner/diff-suite, keywire and
\ audit-log cross-process suites, promotion, obligation and evidence-db checks.
\ Measured self-time on GB10 (idle, 2026-07-19): ~9293 ms across 27 suites.
\ Run standalone: bin/hb --load maki/test-db.f

require maki/test-harness.f

using TEST

RESET

GROUP SEQ maki-db
SUITE maki/db/artifact-test.f
;SUITE
SUITE maki/db/transaction-test.f
;SUITE
SUITE maki/db/commit-store-test.f
;SUITE
SUITE maki/db/commit-store-crash-test.f
;SUITE
SUITE maki/db/diagnostic-test.f
;SUITE
SUITE maki/db/obligation-test.f
;SUITE
SUITE maki/db/evidence-test.f
;SUITE
SUITE maki/db/evidence-applicability-test.f
;SUITE
SUITE maki/db/promotion-policy-test.f
;SUITE
SUITE maki/db/promotion-authority-test.f
;SUITE
SUITE maki/db/promotion-test.f
;SUITE
SUITE maki/db/action-test.f
;SUITE
SUITE maki/db/diff-suite-test.f
;SUITE
SUITE maki/db/diff-suite-id-test.f
;SUITE
SUITE maki/db/diff-runner-test.f
;SUITE
SUITE maki/db/diff-runner-tensor-test.f
;SUITE
SUITE maki/db/diff-runner-spawn-test.f
;SUITE
SUITE maki/db/diff-runner-inject-test.f
;SUITE
SUITE maki/db/diff-case-store-test.f
;SUITE
SUITE maki/db/diff-case-store-xproc-test.f
;SUITE
SUITE maki/db/capbud-test.f
;SUITE
SUITE maki/db/agent-loop-test.f
;SUITE
SUITE maki/db/audit-log-test.f
;SUITE
SUITE maki/db/commit-store-discharge-test.f
;SUITE
SUITE maki/db/keywire-xproc-test.f
;SUITE
SUITE maki/db/keywire-xproc-env-test.f
;SUITE
SUITE maki/db/audit-log-xproc-test.f
;SUITE
;GROUP

RUN

;using
