---
title: Tell a stopped check from a clean one
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T16:41:43.657575+02:00"
---

Problem: a check that stops (verify-source TOP-OPAQUE, TICK-REMAINDER) is a W-CHECK-DEFERRED warning like a bounded deferral, and check-all-errors exits 0 (tools/check-all-errors-core.f CA-STATUS), so a source checked only up to a stop reads as clean. Acceptance: a stopped check is told apart from a clean one in the text, the JSON and the exit status. Depends: none. Claim: unassigned.
