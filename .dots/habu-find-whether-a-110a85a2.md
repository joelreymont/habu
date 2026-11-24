---
title: Find whether a host-word miss under the seal reads XREF-NULL
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T19:29:26.587556+01:00"
---

Problem: on a HOST-WORD? miss while the design seal is on, REC holds XREF-NULL and POLICY-PRE's found=true path would read it (E-XREF-NULL); no build has been seen running with the seal on. Found by the hostword review (nymlnkoz). Acceptance: a reproducer that reaches the miss with the seal on and its root cause, or the path from a program's entry shown unreachable; then the fix. Depends: nymlnkoz on master. Claim: unassigned.
