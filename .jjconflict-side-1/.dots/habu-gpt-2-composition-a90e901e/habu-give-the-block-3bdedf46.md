---
title: Give the block composition a real B extent
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T22:41:31.066433+02:00"
blocks:
  - habu-complete-batched-pos-99332bf6
---

The composition is 2D TxC throughout (embedding.f:79 documents the token+pos path as B=1). Give it B>1 sequences end-to-end. Honest blocker: habu-complete-batched-pos-99332bf6 (open, itself blocked on lowering-hash/type chain). Also prove cross-batch sequence isolation at the composition level (perturb one sequence, other sequences' outputs and grads bit-unchanged).
