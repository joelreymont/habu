---
title: Define robust gate performance verdict policy
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T16:45:56.629024+02:00\""
---

Problem: full-gate timing has no pure checked policy for calibration drift, pass/marginal/hard bands, or retry aggregation, so runner code would encode an unaudited heuristic. Fix: add package PERF-VERDICT in test/perf-verdict.f with private band enum and checked PASS?, MARGINAL?, HARD-FAIL?, CAL-STABLE?, TWO-OF-THREE?, and deterministic ATTEMPT-LINE over a private typed attempt record. Acceptance: exact thresholds; stable pass; one noisy marginal plus two passes; one lucky pass plus two marginal regressions; hard fail; calibration drift; reused/nonempty attempt root; unexpected within-attempt cache counters; missing correctness/control/SHA evidence; SHA mismatch; and deterministic row fixtures all enforce the frozen parent rule. Files: test/perf-verdict.f, test/perf-verdict-test.f, FILEMAP.md. Verify: exact focused test, typed-local diff lint, host/filemap/status/dot lints, full native gate.

Claim: agent=perfverdict workspace=.jj-ws/fable-perfverdict
