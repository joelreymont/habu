---
title: Validate GPU reduction slice
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:02:41.458288+02:00"
blocks:
  - habu-lower-gpu-softmax-7eeacc64
---

Full context: complete GPU Wave C with independent stage/witness validation, ptxas/resource evidence, shadow coverage, and pinned performance rows. Acceptance: reduction/softmax correctness and convergence mutations pass or reject; covered production paths have no hidden fallback.
