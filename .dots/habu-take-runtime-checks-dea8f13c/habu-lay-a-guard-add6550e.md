---
title: Lay a guard message without a buffer
status: closed
priority: 2
issue-type: task
created-at: "2026-11-24T16:04:33.729196+01:00"
closed-at: "2026-11-24T16:20:39.653850+01:00"
---

Problem: src/habu/fault-record.f GUARD-MSG$ joins "`name` rest" in GUARD-MSG, a 128-byte buffer whose BUF-APPEND dies past it, only so src/habu/habu1.f:1296 GUARD-RECORD, and src/habu/kernel-x64.f:326 can hand FAULT-OPEN, one message; the fault texts it feeds already grow (VEC). Acceptance: GUARD-MSG-CAP and the fixed GUARD-MSG buffer go and the guard's message grows; engine builds converge. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-guard.
