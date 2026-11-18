---
title: "Reject a sealed policy's change in --verify-only"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:43:16.519182+01:00"
---

Problem: a program that changes the source policy after it is sealed is rejected E-POLICY by the runner and tools/check.f (rc 70), while --verify-only passes it (rc 0). Acceptance: --verify-only rejects it with the same record. Depends: none. Claim: unassigned.
