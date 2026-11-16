---
title: Check an [if] inside a structure body in the verifier
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T17:22:57.520073+01:00"
---

Problem: an `[if]` inside a BEGIN-STRUCTURE body whose taken branch loads a file is rejected by the load and tools/check.f (E-UNCLOSED, no declaration spans a load), while --verify-only defers it (W-CHECK-DEFERRED, rc 0). Acceptance: --verify-only gives the load's record. Depends: none. Claim: unassigned.
