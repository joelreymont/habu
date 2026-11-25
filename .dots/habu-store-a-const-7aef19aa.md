---
title: Store a constant zero from xzr on arm64
status: open
priority: 3
issue-type: task
created-at: "2026-11-25T16:42:29.550855+01:00"
---

Problem: T0 stores a constant 0 as movz x10,#0 and str x10 (seen on NULL-PTR swap !), where arm64 has str xzr. Acceptance: a store of constant 0 in T0's arm64 code takes one instruction, and the engine's code is smaller by it. Depends: none. Claim: unassigned.
