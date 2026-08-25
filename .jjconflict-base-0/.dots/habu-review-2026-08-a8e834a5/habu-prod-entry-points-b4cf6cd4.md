---
title: production entry points with no scheduled test
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.069392+02:00"
---

Problem: TAIL-BUDGET:PROCESS-MS has one enforcer, test/gate-tail-process.f:31-35, which nothing spawns (:2-4); DETECT-PROFILE's linux-arm64-4x2 arm and TR-CHECK-LINUX-PROFILE have no test (test/run-lib-test.f covers the Spark arm, manually); test/candidate-rebuild-test.f is manual by necessity (docs/gate.md:42-49) with no record it ran; --cold-cache has no test; test/nested-validation-rca-test.f is manual. Acceptance: each scheduled, deleted with its ratchet, or recorded as manual with a run log the gate checks. Files: as listed. Verify: schedule-lint and the runner's phase list. Depends: none. Ownership: gate runner. Claim: unassigned.
