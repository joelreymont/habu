---
title: "Classify the library containers' own checks"
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T09:30:30.798113+01:00"
---

Problem: src/lib/array.f A-CHECK-INDEX (28, 48-70), src/lib/c2-memory.f CHECK-INDEX (330-417), src/lib/queue.f Q-CHECK-CAP (90, 253) and src/lib/byte-buffer.f CHECK-CAP (52-150) check indexes and capacities at run time, like MAP's checks before their removal. Acceptance: each is deleted where it catches a program error, or named as guarding outside data; their suites pass. Depends: none. Claim: unassigned.
