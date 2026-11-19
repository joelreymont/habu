---
title: "Delete the kernel's cell-kind checks"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T15:06:42.380931+01:00"
---

Problem: once the four cell declarations are BOOT privates, E-XTKIND (test/address-cell-kind-bad.f, program-diagnostics-test.f:91-92 reach it only through xt!) and E-XTCELL check what only the engine can pass: runtime checks no program reaches. Acceptance: each is deleted with its test, or a program path that reaches it is named. Depends: none; it lands in or after the commit that removes the four global rows. Claim: unassigned.
