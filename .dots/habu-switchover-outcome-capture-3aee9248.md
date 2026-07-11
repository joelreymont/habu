---
title: "Switchover: -OUTCOME capture API (kind code) pair -> outcome sum"
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T21:56:14.161145+02:00"
---

Follow-up to the wave-C outcome-sum slice (lib/process.f SUMTYPE outcome: exited n | signaled n | timeout). The wide capture API still speaks the legacy (kind code) int pair through PROC-OUTCOME-EXIT/SIGNAL/TIMEOUT + PROC-OUTCOME-KIND/CODE vars: PROC-CAPTURE-OUTCOME@, PROC-CAPTURE-FINISH-OUTCOME, RUN-CAPTURE-OUTCOME (lib/process.f), RUN-ARGV-CAPTURE-OUTCOME, RUN-ARGV-STDIN-CAPTURE-OUTCOME (lib/process-argv.f), RUN-ARGV-ENV-*-OUTCOME (lib/process-env.f), PROC-CMD-RUN-OUTCOME/PROC-CMD-OUTCOME@ (lib/process-command.f), plus consumer-side pair stores (lib/test/runner.f GT-STORE-RUN/GT-RC@/GT-TIMEOUT, test/gate-pool.f pool kind/code arrays, test/seal.f SLV-KIND). ~38 consumer files total (rg RUN-ARGV-CAPTURE-OUTCOME / PROC-OUTCOME-EXIT). PROC-OUTCOME-PAIR ( outcome -- n n ) in lib/process.f is the ONE documented sum->pair boundary to retire; migrate the API to return the sum, rewrite consumers to MATCH (storage sites keep W=1 derivable state: status + timed-out flag, per the slice-4 design note), then delete PROC-OUTCOME-PAIR, the three kind constants, and PROC-OUTCOME-KIND/CODE. Needs its own slice plan: several consumer files belong to concurrent workers (tools lint-test cluster, repair/diag fixtures).
