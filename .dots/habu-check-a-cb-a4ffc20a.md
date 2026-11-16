---
title: "Check a CALLBACK: open across a load in the verifier"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T17:22:57.498165+01:00"
---

Problem: `CALLBACK: RV-CB ( n -- n )` followed by an `include` before its close gives E-UNCLOSED ('`CALLBACK:` is still open when `include` loads a file', rc 70) from the load and tools/check.f, while --verify-only defers the whole CALLBACK: group (W-CHECK-DEFERRED, rc 0). Acceptance: --verify-only gives the load's record. Depends: none. Claim: unassigned.
