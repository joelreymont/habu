---
title: "Sign a build's output only when it changed"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T04:37:15.174786+02:00"
---

Problem: src/lib/codesign.f:348 runs codesign three times per build (check the team signature, sign, verify), 118 ms of a warm engine build, also when the output's bytes equal the last signed ones. Acceptance: a build whose output equals the signed file it replaces runs no codesign, and a changed one is signed and verified as now. Depends: none. Claim: unassigned.
