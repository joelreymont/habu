---
title: Harden field tokens and IDs
status: closed
priority: 1
issue-type: task
created-at: "2026-07-17T12:11:52.142245+02:00"
closed-at: "2026-07-21T06:39:56.176501+02:00"
close-reason: "Landed 1271cac2: PF-ROW/PF-REC@ reject bad ids with catchable E-PF-ID 7122 (not die 76); TFAM-RESET refuses under a live field transaction (E-PF-TX) and no longer rewinds PF-TX-SERIAL - PF-BEGIN is the sole writer of a process-monotonic token generation counter, so pre-reset tokens can never alias post-reset transactions. Regressions in type-family-suite; suites + battery green on the merged tree; install --force x2 byte-identical."
---

Review findings: TFAM-RESET at src/core/type-family.f:1320 resets PF-TX-SERIAL and discards active frames, allowing a stale token to alias a new transaction; PF-REC@ at :787 uses raw die 76 for public reflection IDs typed as n. Fix: reject reset while PF-TX-DEPTH is nonzero, keep transaction generation monotonic across reset and fail closed before wrap, introduce named E-PF-ID throw for every invalid/provisional reflected ID, and keep reset/snapshot semantics explicit. Tests: active-reset reject preserves frame, completed-token remains stale across reset/new begin, negative and out-of-range IDs, every public indexed accessor rejects a guessed provisional ID without process exit. Acceptance: exact type-family/rollback/declaration/internal gates, docs, fixpoint, trust/lints green. Files: src/core/type-family.f:787-892,1320, src/core/checker.f:4659-4668, test/type-family-suite.f, docs/type-families.md.
