---
title: "TFAM 3: transactional reentrant registry rollback"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-03T23:36:48.923155+02:00\""
---

PLAN.md item 3. Rollback-frame stack (depth-safe, replaces single-slot) with high-water marks for TFAM/SUMV/SCHEMA/product-field/layout/string-pool plus VREC/CT/SYM/LIN/USIG, package mode/name, DFER, deferred caches; hash-index entries retired on rollback, not just counters. Used by CHECKER-SCOPE-START/DONE + CHECK-CANDIDATE-START/DONE (src/core/checker.f:5010-5095). Nested all-errors tests prove parent frames survive. Gate 17c. Depends: TFAM 2a.
