---
title: Invalidate MEM evidence across mutation
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:44:46.926716+02:00"
blocks:
  - habu-add-immutable-lexical-28b79e06
---

Problem: clear, shrink, grow, reallocate, or owner recreation can leave an old index/subspan/alignment proof apparently valid for changed storage. Fix: every structural mutation consumes generation g and returns fresh g2; all indices, subspans, and borrows are generation-indexed and cannot cross the transition; counters fail before wrap or identity reuse. Acceptance: stale-after-clear/shrink/grow/reallocation, same-size recreation, old alignment proof, and generation exhaustion fixtures reject; role-preserving byte writes within one generation remain valid. Files: lib/memory-region-mutate.f, focused test, checker fixtures only if required, docs/effects.md. Verify: red-first matrix, runtime mutation sentinels, linear/type-family/memory suites, full native gate.
