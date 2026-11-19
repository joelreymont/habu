---
title: "Type the exit vector's cell"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T15:06:42.391521+01:00"
---

Problem: BOOT:EXIT-VECTOR (src/habu/layout.f:772) is public `( -- ^n )`, so `: Q ( -- ) 12345 BOOT:EXIT-VECTOR ! ;` is meant to pass tools/check.f -- --verify-only (read, not run) and the exit path would call a number. Acceptance: a program cannot store a non-quotation there (the vector typed, or private to its owner); a rejection test. Depends: none. Claim: unassigned.
