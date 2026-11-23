---
title: "Classify C2's frame-state checks"
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T09:58:04.476921+01:00"
blocks:
  - habu-classify-the-lib-289cd5d4
---

Problem: src/lib/c2-memory.f throws E-C2-STATE at run time on the frame chain's state (124, 128: no chain head; 197: a zero id; 224: a node that is not a live bytes frame), checks the checker's scope rules may already make. Acceptance: each is deleted where it catches a program error or repeats a static rule (its test disabled under habu-add-the-checks-a49d054d), or named as guarding outside data; the c2 suites pass. Depends: habu-classify-the-lib-289cd5d4 (same file). Claim: unassigned.
