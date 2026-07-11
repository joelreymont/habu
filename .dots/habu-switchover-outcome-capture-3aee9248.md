---
title: "Switchover: -OUTCOME capture API (kind code) pair -> outcome sum"
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T21:56:14.161145+02:00"
---

Follow-up to the wave-C outcome-sum slice (lib/process.f SUMTYPE outcome: exited n | signaled n | timeout). The wide capture API still speaks the legacy (kind code) int pair through PROC-OUTCOME-EXIT/SIGNAL/TIMEOUT + PROC-OUTCOME-KIND/CODE vars: PROC-CAPTURE-OUTCOME@, PROC-CAPTURE-FINISH-OUTCOME, RUN-CAPTURE-OUTCOME (lib/process.f), RUN-ARGV-CAPTURE-OUTCOME, RUN-ARGV-STDIN-CAPTURE-OUTCOME (lib/process-argv.f), RUN-ARGV-ENV-*-OUTCOME (lib/process-env.f), PROC-CMD-RUN-OUTCOME/PROC-CMD-OUTCOME@ (lib/process-command.f), plus consumer-side pair stores (lib/test/runner.f GT-STORE-RUN/GT-RC@/GT-TIMEOUT, test/gate-pool.f pool kind/code arrays, test/seal.f SLV-KIND). ~38 consumer files total (rg RUN-ARGV-CAPTURE-OUTCOME / PROC-OUTCOME-EXIT). PROC-OUTCOME-PAIR ( outcome -- n n ) in lib/process.f is the ONE documented sum->pair boundary to retire; migrate the API to return the sum, rewrite consumers to MATCH (storage sites keep W=1 derivable state: status + timed-out flag, per the slice-4 design note), then delete PROC-OUTCOME-PAIR, the three kind constants, and PROC-OUTCOME-KIND/CODE. Needs its own slice plan: several consumer files belong to concurrent workers (tools lint-test cluster, repair/diag fixtures).

## INTERNAL STORAGE PORTION — DONE (capture-state redesign slice)

lib/process.f stores NO pair state: PROC-OUTCOME-KIND/CODE deleted; the machine
keeps raw PROC-STATUS + a PROC-TIMED-OUT flag (both W=1). PROC-CAPTURE-OUTCOME
( -- outcome ) derives the sum on demand; PROC-CAPTURE-OUTCOME@ flattens it
through PROC-OUTCOME-PAIR (public sig unchanged). GE-EVAL-STORE-RC
(test/gate-common-lib.f) stores its synthesized exited-rc quad straight into GT
runner state — also closing a latent always-pass hole (GE-EXPECT-OK reads
GT-RC@, which never saw the old process-var forge at GDB-JITDUMP / GDX-SARIF /
GDX-TRUST-LINT-STALE). TR-PHASE-OK? (test/run-lib.f) reads GT-RC@.

REMAINING (the sweep): migrate the wide -OUTCOME API returns to the sum;
rewrite ~38 consumer files to MATCH; consumer-side stores (runner GT vars,
gate-pool arrays, seal SLV-KIND, process-command mirror — NOTE process-command
cannot derive from rc: pair->rc is lossy for exit codes >= 128, so it must
snapshot status+flag or wait for W=2 storage); then delete PROC-OUTCOME-PAIR,
PROC-PAIR>RC, and the three kind constants. Several consumer files belong to
concurrent workers (tools lint-test cluster, repair/diag fixtures) — needs a
slice plan.
