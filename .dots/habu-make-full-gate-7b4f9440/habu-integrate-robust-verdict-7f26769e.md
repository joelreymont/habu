---
title: Integrate robust verdict into full native gate
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T16:45:56.635130+02:00"
blocks:
  - habu-define-robust-gate-2f4b3e7b
---

Problem: the full native runner currently emits one calibrated wall-clock
verdict and cannot execute the frozen marginal retry rule without risking cache
reuse, recursive retries, SHA drift, or missing evidence. Fix: integrate the
landed `PERF-VERDICT` policy at the top/under-test phase boundary; bracket each
attempt with calibration, allocate distinct fresh roots and build cache, prove
no prior-attempt artifact is visible, validate the expected within-attempt cache
counter contract, pin one exact-tree under-test SHA, and render deterministic
attempt plus final verdict rows. Initial pass runs
once; initial marginal runs exactly two additional fresh attempts; any invalid
or hard-fail attempt fails closed. Acceptance: process-level fixtures prove
fresh-root isolation, no recursion, SHA equality, no cross-attempt reuse, exact attempt
count, correctness/performance separation, 2-of-3 aggregation, and preservation
of existing cold/warm correctness semantics. Files: test/run.f, test/run-lib.f,
test/gate-stats.f, test/gate-stats-test.f, test/run-files.f, docs/bootstrap.md.
Verify: runner/stats fixtures, repeated isolated full native gate evidence,
typed-local diff lint, host/filemap/status/dot lints, Maki and PTX slices.
