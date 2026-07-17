---
title: Harden field tokens and IDs
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T12:11:52.142245+02:00"
blocks:
  - habu-internalize-field-liveness-7c1d7e14
---

Review findings: TFAM-RESET at src/core/type-family.f:1320 resets PF-TX-SERIAL and discards active frames, allowing a stale token to alias a new transaction; PF-REC@ at :787 uses raw die 76 for public reflection IDs typed as n. Fix: reject reset while PF-TX-DEPTH is nonzero, keep transaction generation monotonic across reset and fail closed before wrap, introduce named E-PF-ID throw for every invalid/provisional reflected ID, and keep reset/snapshot semantics explicit. Tests: active-reset reject preserves frame, completed-token remains stale across reset/new begin, negative and out-of-range IDs, every public indexed accessor rejects a guessed provisional ID without process exit. Acceptance: exact type-family/rollback/declaration/internal gates, docs, fixpoint, trust/lints green. Files: src/core/type-family.f:787-892,1320, src/core/checker.f:4659-4668, test/type-family-suite.f, docs/type-families.md.
