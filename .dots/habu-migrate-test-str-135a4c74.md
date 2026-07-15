---
title: "Migrate test string callers to STR:"
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-15T15:05:05.431903+02:00\""
---

Full context: MODEL-CAD-V2-PLAN.md B5.5a legacy-STR census, test lane. Migrate raw STR calls in: test/boot-pin-test.f, gate-engine-lib.f, owner-wid-doctor.f, gate-pool-test.f, seal-absence.f (FIND-SUB/INDEX-OF/SPLIT-NEXT per census), test/run-lib.f (SPLIT-NEXT, BUF-RESET, BUF-APPEND, BUF-APPEND-C), test/run-rerun-failed-test.f (BUF-RESET). Blocks on the STR:BUF-APPEND-C owner extension. Overlap note: gate-engine-lib/seal-absence touched by the MEM test wave (sequential). Acceptance: fresh rg census empty; focused tests/gate slices green + full run.f (harness files). Files: the 7 listed + focused tests. Ownership: the 7 test files.

Claim: agent=teststr workspace=.jj-ws/fable-cadnum
