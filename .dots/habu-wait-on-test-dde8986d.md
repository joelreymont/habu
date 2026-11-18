---
title: Wait on test children without a clock
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T12:38:59.440297+01:00"
---

Problem: test/gate-common-lib.f GE-TIMEOUT-MS (120 s) is a wall deadline guarding the children of test/x86-64-main-entry.f :58 and :67, its only readers, against test/AGENTS.md's rule that a hang is named by its WAIT line, never ended by a clock. Acceptance: those two calls pass `PROCESS:WAIT-FOREVER MS>N`, as other GE-RUN-STDIN callers do, and GE-TIMEOUT-MS is deleted; x86-64-main-entry passes. Depends: none. Claim: unassigned (x86-64 files are carl's).
