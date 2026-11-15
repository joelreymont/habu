---
title: Drop run-rc from the x86-64 harness
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T12:28:01.982408+01:00"
---

Problem: the arm64 kernel no longer has run-rc (the primitive only test/engine-suite.f called), but the set-aside x86-64 files still name it: test/x86-64-kernel-syscalls.f:361-369 runs `s" run-rc" ROW`, and src/habu/kernel-x64.f's comments at :883, :1154 and :1203 name BRUNRC. Acceptance: once x86-64 work resumes, x86-64's run-rc body, row and harness case are deleted and the x86-64 kernel tests pass. Depends: x86-64 work resuming (habu-fix-remaining-x86-5a0c47fd). Claim: unassigned.
