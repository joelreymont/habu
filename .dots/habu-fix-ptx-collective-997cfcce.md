---
title: Fix PTX collective mask and block semantics
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T15:32:50.883376+02:00"
---

Deep-review finding 2026-06-27: lib/ptx/cg-collective.f ROW-LOAD seeds inactive lanes to -inf and erases the mask, which is valid for BLOCK-MAX but wrong for BLOCK-SUM and backward loads; collective codegen also hardcodes SM-BLK=256/SMEM[1024] while some fixtures/docs use block-1024; WHERE constraints are skipped instead of validated. Correct fix: carry lane predicate/mask in emitted tile representation or make each collective apply its own identity, derive/reject block size consistently from PTX-BLOCK@, parse WHERE enough to validate launch-time constraints, and add device tests for direct row sum, softmax/backward with k < block, malformed WHERE, block mismatch, and k > block.
