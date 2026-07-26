---
title: Store compiler control structure
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:54:58.876373+02:00"
blocks:
  - habu-store-compiler-ops-10440e3e
---

Full context: design section 6.3 requires function/block parents and deterministic windows over generic operation pools. Add function signatures/regions, block arguments, parent identities, operation/successor windows, visibility, and terminator placement metadata. Acceptance: duplicate insertion, cross-function block use, bad parent/window/order, and foreign-owner cases reject. Dependency: operation pools.
