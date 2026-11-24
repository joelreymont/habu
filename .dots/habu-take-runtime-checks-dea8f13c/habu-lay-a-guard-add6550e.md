---
title: Lay a guard message without a buffer
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T16:04:33.729196+01:00"
---

Problem: src/habu/fault-record.f GUARD-MSG$ joins "`name` rest" in GUARD-MSG, a 128-byte buffer whose BUF-APPEND dies past it, only so src/habu/habu1.f:1296 GUARD-RECORD, and src/habu/kernel-x64.f:326 can hand FAULT-OPEN, one message; the fault texts it feeds already grow (VEC). Acceptance: GUARD-MSG, GUARD-MSG-CAP and GUARD-MSG$ go and the guard's message reaches the fault texts directly; engine builds converge. Depends: none. Claim: unassigned.
