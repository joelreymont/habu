---
title: Verify what a parsing immediate runs
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:54:31.862960+01:00"
---

Problem: src/core/checker.f:21720-21728 NEUTRAL-SYM-IMM? takes a neutral parse-imm's contract (checker.f:17069-17071: it consumes source tokens and contributes no runtime step) on trust and never reads its body, so --verify-only certifies with rc 0 programs --load rejects: /tmp/plan-vw/q3.f (E-UNDEFINED), q4.f (E-MISMATCH), q6.f (rc 5), q7.f (E-INPUT-UNDERFLOW), q8.f (E-DEFINITION-OPEN). Acceptance: the three readers agree on each probe. Depends: none. Claim: unassigned.
