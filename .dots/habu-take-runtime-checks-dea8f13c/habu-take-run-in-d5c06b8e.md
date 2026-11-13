---
title: "Take run-in-stack's guard check out of run time"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T11:00:44.394329+02:00"
---

Problem: src/habu/habu1.f GUARDED-EXTENT? (:3704-3713) checks at run time that run-in-stack's stack is a guarded mapping; only tests and the gate's evaluation harness (test/gate-common-lib.f:383) call run-in-stack. Acceptance: no run-time check: run-in-stack is gone and its test callers use what programs use, or its operand's type proves a guarded mapping (MEM:ALLOC-GUARDED). Depends: habu-delete-checks-the-ed728e4f. Claim: unassigned.
