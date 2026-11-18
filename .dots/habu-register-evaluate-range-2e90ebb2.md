---
title: Register evaluate-range the same way on both machines
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T10:02:17.301878+01:00"
---

Problem: src/habu/kernel-x64.f:2667 makes evaluate-range a GLOBAL-INT-WID (internal) primitive while src/habu/habu1.f:4085 registers it with FPRIM-L, so it is internal on x86-64 and not on arm64. Acceptance: one registration on both machines, the shared layer deciding it. Depends: none. Claim: unassigned.
