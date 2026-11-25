---
title: Restore what a candidate check found
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T20:33:01.905364+01:00"
---

Problem: src/core/check-walk.f CHECK-CANDIDATE-DONE sets OPERANDS-SPENT to -1 rather than to what CHECK-CANDIDATE-START found, and CHECK-CANDIDATE! leaves its text armed when CANDIDATE-NAME-REJECT? or the start throws. Acceptance: a nested or throwing candidate check leaves every cell as it found it, shown by a case that fails without it, or the reason none can. Depends: none. Claim: unassigned.
